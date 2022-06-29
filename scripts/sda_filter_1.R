## Apply speed-angle filter to simple-filter data

pacman::p_load(argosfilter, dplyr, sf, mapview, amt, ggplot2)

tracks <- readRDS("data/analysis/cleaned_0/cleaned_0.rds")

n_ids <- n_distinct(tracks$ID)
alist <- list()

for(i in seq_len(n_ids)){
  print(i)
  one <- subset(tracks, ID == unique(tracks$ID)[i])
  
  one$sdafilter <- sdafilter(
    lat = one$Latitude, lon = one$Longitude, dtime = one$DateTime, 
    lc = one$Location.Quality, 
    vmax = 1.39, ang = 20, distlim = 2500
  )
  
  alist[[i]] <- one
}

tracks <- do.call(rbind, alist)

## visualize
# tracks_sf <- st_as_sf(tracks, coords = c("Longitude", "Latitude"),
#                  crs = 4326, agr = "constant")
# mapview(tracks_sf, zcol = "sdafilter")

x <- filter(tracks, ID == "224389")
st_as_sf(x, coords = c("Longitude", "Latitude"), 
         crs = 4326, agr = "constant") %>% 
  mapview(zcol="sdafilter")

## SAVE -----------------------------------------------------------------------

tracks_f <- subset(tracks, sdafilter != "removed")


## % proportion of locations of each class
LQsumm <- tracks_f %>% group_by(ID) %>% 
  summarise(
    n_pnts = n()
  ) %>% left_join(tracks_f) %>% 
  group_by(ID, Location.Quality) %>% 
  summarise(
    n_LQ = n(),
    n_pnts = first(n_pnts),
    perc_LQ = round(n_LQ / n_pnts *100, 1)
  )
# View(LQsumm)
LQsumm %>% filter(Location.Quality == "B") %>% ungroup() %>% 
  summarise(perc_B=mean(perc_LQ))

## Get duration of device life for each individual ----------------------------

trckdur <- tracks_f %>% group_by(ID, sex) %>% 
  summarise(
    firstday = first(DateTime),
    lastday  = last(DateTime),
    duration = round(difftime(lastday, firstday), 0)
  )

trckdur %>% ungroup() %>% summarise(
  n = n_distinct(ID),
  mn = mean(duration), md = median(duration), 
  mx = max(duration), min=min(duration)
)

write.csv(trckdur, "data/summaries/tracking_duration_speedfilter_ids.csv", row.names = F)

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

tracks_amt <- tracks_f %>% 
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


alist <- list()
for(i in 1:n_distinct(tracks_amt_proj$id)){
  one <- subset(tracks_amt_proj, id == unique(tracks_amt_proj$id)[i])
  one_f <- subset(tracks_f, ID == unique(tracks_amt_proj$id)[i])
  one_f$nsd <- nsd(one)
  alist[[i]] <- one_f 
}

tracks_g <- do.call(rbind, alist)

### Save filtered dataset -----------------------------------------------------
saveRDS(tracks_g, "data/analysis/speed_angle_1/speed_angle_1.rds")

## plot -----------------------------------------------------------------------
ggplot() + 
  geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.5) + 
  geom_path(data=tracks_amt_proj, 
            aes(x = t_, y = sqrt(nsd)/1000, group=IDgap, color=sex), size=0.75) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~id)

ggsave("figures/nsd/nsd_speedangledata_n25_sex.png", width = 8, height=6)


