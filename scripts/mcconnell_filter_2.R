## Apply stricter McConnell speed filter (and B LQ and land mask filter) ##

pacman::p_load(trip, dplyr, ggplot2, lubridate)

source("C:/Users/Martim Bill/Documents/R/source_scripts/pt2pt_fxns.WV.R")

tracks <- readRDS("data/analysis/speed_angle_1/speed_angle_1.rds")

## include B locations?
B <- TRUE
# B <- FALSE

## convert to 'trip' object
tr <- trip(tracks, c("DateTime", "ID")) ## also acts as duplicate filter

n_ids <- n_distinct(tr$ID)
alist <- list()

for(i in seq_len(n_ids)){
  print(i)
  one <- subset(tr, ID == unique(tr$ID)[i])
  
  one$mcfilter <- speedfilter(one, max.speed = 5)    # Create a filter of a track for "bad" points implying a speed of motion that is unrealistic.
  
  alist[[i]] <- one
}

tracks <- do.call(rbind, alist)

### visualize
# tracks_sf <- st_as_sf(tracks, coords = c("Longitude", "Latitude"),
#                       crs = 4326, agr = "constant")
# mapview(tracks_sf, zcol = "mcfilter")

x <- subset(tracks, ID == "224389")

mapview(x, zcol="mcfilter")

## 
tracks_f <- subset(tracks, mcfilter == TRUE)


### What % of points are of each Location.Quality? ----------------------------
round(table(tracks_f$Location.Quality) / nrow(tracks_f) *100, 1)

## per individual
LQsumm <- tracks_f@data %>% group_by(ID) %>% 
  summarise(
    n_pnts = n()
  ) %>% left_join(tracks_f@data) %>% 
  group_by(ID, Location.Quality) %>% 
  summarise(
    n_LQ = n(),
    n_pnts = first(n_pnts),
    perc_LQ = round(n_LQ / n_pnts *100, 1)
  )
# View(LQsumm)
LQsumm %>% filter(Location.Quality == "B") %>% ungroup() %>% 
  summarise(perc_B=mean(perc_LQ))

# write.csv(LQsumm, "data/summaries/location_quality_mcconnellfilter.csv", row.names = F)

## Get duration of device life for each individual ----------------------------

trckdur <- tracks_f@data %>% group_by(ID, sex) %>% 
  summarise(
    firstday = first(DateTime),
    lastday  = last(DateTime),
    duration = round(difftime(lastday, firstday), 0)
  )

write.csv(trckdur, "data/summaries/tracking_duration_ids.csv", row.names = F)

## filter out LQ 'B' points ---------------------------------------------------

if(B == FALSE){
  tracks_f <- subset(tracks_f, Location.Quality != "B")
}


## calculate time step btwn consec. locations
calcdur <- function(x) pt2pt.duration(datetime=x$DateTime)
ts_list <- lapply(split(tracks_f, tracks_f$ID),'calcdur')
tracks_f$step_t <- as.numeric(unlist(ts_list)) / 60 / 60 # hours

## summarize time steps
ts_summ_id <- tracks_f@data %>% group_by(ID) %>% summarise(
  md_ts = median(na.omit(step_t)),
  mn_ts = mean(na.omit(step_t))
)

ts_summ <- ts_summ_id  %>% 
  summarise(
    mn_md_ts = mean(na.omit(md_ts)),
    mn_mn_ts = mean(na.omit(mn_ts))
  )
ts_summ

## how many gaps (of certain size) does each individual have?
gapsumm2 <- tracks_f@data %>% group_by(ID, sex) %>% 
  summarise(
    n_pnts = n(),
    n_gaps_48h = sum(na.omit(step_t) > 48)
  )

## convert to 'trip' object
tr <- trip(tracks_f, c("DateTime", "ID")) ## also acts as duplicate filter

## split individual data by gaps (> 1 day)
tr$IDgap <- sepIdGaps(tr$ID, tr$DateTime, 3600 * 48 ) # hours

## convert back to data.frame
tracks_g <- as.data.frame(tr)

## plot sampling over time per individual 

ggplot() + 
  geom_path(data=tracks_g, aes(x = DateTime, y = step_t, color = sex)) + 
  coord_cartesian(ylim = c(0,100)) +
  geom_hline(yintercept=48, color="darkred") +
  xlab("") + ylab("Time step (h)") + 
  theme_bw() + 
  facet_wrap(~ID)

if(B == FALSE){
  ggsave("figures/timesteps_trackduration_mcconnell_wB_n25_sex.png", width = 8, height=6)
} else {
  ggsave("figures/timesteps_trackduration_mcconnell_noBpnts_n25_sex.png", width = 8, height=6)
}


### calculate net-squared displacement on speed/angle filtered data -----------
## Equidistant projection so NSD is correctly calculated (and in m)
# use Poilão as reference point (from which distances should be correct)

poilao <- data.frame(label=c("Poilão"), 
                     "Longitude" = c(-15.726667), 
                     "Latitude" = c(10.864722)
)

proj <- sp::CRS(
  paste0(
    "+proj=aeqd +lon_0=", poilao$Longitude,
    " +lat_0=", poilao$Latitude,  # latitude of true scale
    " +x_0=0 +y_0=0", sep = ""
  ))

tracks_amt <- tracks_g %>% 
  make_track(.x=Longitude, .y=Latitude, .t=DateTime, 
             id = ID, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_d"), 
             all_cols = T) %>% rename(id = ID)

tracks_amt_proj <- transform_coords(tracks_amt, crs_to=proj)

tracks_amt_proj$nsd <- tracks_amt_proj %>% group_by(id) %>% nsd()

alist <- list()
for(i in 1:n_distinct(tracks_amt_proj$id)){
  one <- subset(tracks_amt_proj, id == unique(tracks_amt_proj$id)[i])
  one_g <- subset(tracks_g, ID == unique(tracks_amt_proj$id)[i])
  one_g$nsd <- nsd(one)
  alist[[i]] <- one_g 
}

tracks_g <- do.call(rbind, alist)

## Save -----------------------------------------------------------------------
if(B == FALSE){
  saveRDS(tracks_g, "data/analysis/mcconnell_2/mcconnell_2_noB.rds")
} else {
  saveRDS(tracks_g, "data/analysis/mcconnell_2/mcconnell_2_wB.rds")
}

## plot
ggplot() + 
  geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.5) + 
  geom_path(data=tracks_amt_proj, 
            aes(x = t_, y = sqrt(nsd)/1000, group=IDgap, color = sex), 
            size=0.75) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~id)

if(B == FALSE){
  ggsave("figures/nsd/nsd_mcconnell_noBpnts_data_n12.png", width = 8, height=6)
} else {
  ggsave("figures/nsd/nsd_mcconnelldata_wB_n12.png", width = 8, height=6)
}

