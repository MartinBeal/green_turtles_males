## Initial cleaning steps - Male turtle distributions ## ----------------------

pacman::p_load(trip, dplyr, raster, ggplot2, lubridate, amt)
source("C:/Users/Martim Bill/Documents/R/source_scripts/pt2pt_fxns.WV.R")


## raw tracking data (ALL TRACKED TURTLES)
# all <- read.csv("data/raw_tracks/all_turtles_22APR22.csv")
all <- read.csv("data/raw_tracks/all_turtles_12MAY22.csv")

## turtle deployment metadata
meta <- readxl::read_xlsx("data/metadata_tracked_turtles_v3.xlsx")

## remove rows w/ NA coordinates
all <- filter(all, Latitude != 0 | !is.na(Latitude))

## format data
tracks <- track2KBA::formatFields(
  all, 
  fieldID = "Tag_ID", 
  fieldLat="Latitude", fieldLon="Longitude", 
  fieldDate="UTC_Date", fieldTime="UTC_Time", formatDT = "ymd HMS"
  )

## get rid of "6524" from IDs
tracks$ID <- do.call(
  rbind,
  stringr::str_split(tracks$ID, pattern = stringr::fixed(":"))
  )[,2]

## keep just males (removing 2 males caught in Maur.)
tracks <- subset(
  tracks, 
  ID %in% c("213020", "213021", "213037", "213038", 
            "213039", "213040", "213041", "213042", "213043", "213044", 
            "213045", "213046", "224389", "224390", "224391", "224392", 
            "224393", "224394", "224395", "224396", "224397", "224398", 
            "224399", "224400", "224401")
  )

## remove data from before deployment data for each individual
meta <- meta %>% mutate(PTT = as.character(PTT)) %>%
  filter(PTT %in% unique(tracks$ID))

tracks <- right_join(tracks, 
                     meta[, c("PTT", "deploy_date", "sex")], by = c("ID" = "PTT"))

tracks <- tracks %>% 
  filter(DateTime > deploy_date)

## remove points from NZ (calibration)
# tracksSP <- SpatialPointsDataFrame(
#   SpatialPoints(data.frame(tracks$Longitude, tracks$Latitude), proj4string=CRS("+proj=longlat +datum=WGS84")),
#   data=tracks)
# tracksSP <- tracksSP[bbox,] ## Keep only points in bbox (a polygon around West Africa)

## recombine w/ non-spatial data
# tracks <- data.frame(tracksSP) %>% dplyr::select(-tracks.Longitude, -tracks.Latitude)
tracks <- tracks[order(tracks$ID, tracks$DateTime), ] ## order date time stamps within each

tracks <- tracks %>% 
  dplyr::select(
    "Latitude", "Longitude", "ID", "sex", "DateTime", "Location.Quality", "eRes", 
    "HDOP", "SatNum") %>% 
  mutate(
    Location.Quality = ifelse(Location.Quality == "", "G", Location.Quality)
  )

tracks <- tracks %>% 
  left_join(
    data.frame(ID = unique(tracks$ID), 
               IDv = validNames(unique(tracks$ID))))

tracks <- tracks %>% group_by(ID) %>% 
  dplyr::select("Longitude", "Latitude", "DateTime", everything()) %>% 
  mutate( DateTime = lubridate::ymd_hms(DateTime))


## calculate time step btwn consec. locations
calcdur <- function(x) pt2pt.duration(datetime=x$DateTime)
ts_list <- lapply(split(tracks, tracks$ID),'calcdur')
tracks$step_t <- as.numeric(unlist(ts_list)) / 60 / 60 # hours


## summarize time steps
ts_summ_id <- tracks %>% group_by(ID) %>% summarise(
  md_ts = median(na.omit(step_t)),
  mn_ts = mean(na.omit(step_t))
)

ts_summ <- ts_summ_id  %>% 
  summarise(
    mn_md_ts = mean(na.omit(md_ts)),
    mn_mn_ts = mean(na.omit(mn_ts))
  )

## how many gaps (of certain size) does each individual have?
tracks %>% group_by(ID, sex) %>% 
  summarise(
    n_pnts = n(),
    n_gaps_24h = sum(na.omit(step_t) > 24)
  )

## convert to 'trip' object
tr <- trip(tracks, c("DateTime", "ID")) ## also acts as duplicate filter

## split individual data by gaps (> 1 day)
tr$IDgap <- sepIdGaps(tr$ID, tr$DateTime, 3600 * 24 )

## convert back to data.frame
tracks <- as.data.frame(tr)

## per individual
LQsumm <- tracks %>% group_by(ID) %>% 
  summarise(
    n_pnts = n()
  ) %>% left_join(tracks) %>% 
  group_by(ID, Location.Quality) %>% 
  summarise(
    n_LQ = n(),
    n_pnts = first(n_pnts),
    perc_LQ = round(n_LQ / n_pnts *100, 1)
  )
# View(LQsumm)
LQsumm %>% filter(Location.Quality == "B") %>% ungroup() %>% 
  summarise(perc_B=mean(perc_LQ))

write.csv(LQsumm, "data/summaries/location_quality_raw.csv", row.names = F)

## Save 

saveRDS(tracks, "data/analysis/cleaned_0/cleaned_0.rds")


## plot sampling over time per individual 

ggplot() + 
  geom_path(data=tracks, aes(x = DateTime, y = step_t, color=sex)) + 
  coord_cartesian(ylim = c(0,100)) +
  geom_hline(yintercept=24, color="darkred") +
  xlab("") + ylab("Time step (h)") + 
  theme_bw() + 
  facet_wrap(~ID)

ggsave("figures/timesteps_trackduration_raw_n25_sex.png", width = 8, height=6)


### calculate net-squared displacement on raw data and plot -------------------
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

tracks_amt <- tracks %>% 
  make_track(.x=Longitude, .y=Latitude, .t=DateTime, 
             id = ID, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_d"), 
             all_cols = T) %>% rename(id = ID)

tracks_amt_proj <- transform_coords(tracks_amt, crs_to=proj)

tracks_amt_proj$nsd <- tracks_amt_proj %>% group_by(id) %>% nsd()

alist <- list()
for(i in 1:n_distinct(tracks_amt_proj$id)){
  one <- subset(tracks_amt_proj, id == unique(tracks_amt_proj$id)[i])
  one$nsd <- nsd(one)
  alist[[i]] <- one 
}

tracks_amt_proj <- do.call(rbind, alist)

## plot
ggplot() + 
  geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.5) + 
  # geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000, group=IDgap),
  #           color = "red", size=0.75) + 
  geom_path(data=tracks_amt_proj, 
            aes(x = t_, y = sqrt(nsd)/1000, group=IDgap, color=sex), size=0.75) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~id)

ggsave("figures/nsd/nsd_rawdata_n25_sex.png", width = 8, height=6)
