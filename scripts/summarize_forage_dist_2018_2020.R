## Calculate distance to foraging ground for females 2018-2020

pacman::p_load(sf, dplyr, lubridate)

tsumm <- read.csv("data/tracking_summary_2018_2020.csv")
tsumm$PTT <- as.character(tsumm$PTT)

tracks <- readRDS("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/analysis/raw_filtered/foraging_satfilt4.rds")

poilao <- data.frame(label=c("Poilão"), 
                     "Longitude" = c(-15.726667),
                     "Latitude" = c(10.864722)
)
poilao <- st_as_sf(poilao, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")


tracks_sf <- st_as_sf(tracks, 
                   coords = c("Longitude", "Latitude"),
                   crs = 4326, agr = "constant")

tracks$displace <- st_distance(tracks_sf, poilao)

## summarise dates -- note this is only for individuals w/ foraging data
old_summ <- tracks %>% group_by(ID) %>%
  summarise(
    mn_displace = as.numeric(mean(displace)/1000)
  ) %>% 
  left_join(tsumm[,c("PTT", "Start.date", "date.leaving.nesting.site", "CCL..cm.")], by=c("ID"="PTT")) %>% 
  mutate(
    deploy_yr  = year(dmy(Start.date)),
    deploy_doy = yday(dmy(Start.date)),
    depart_doy = yday(dmy(date.leaving.nesting.site)),
    depart_mon = month(dmy(date.leaving.nesting.site)),
    sex = "F"
  ) %>% dplyr::select(-Start.date, -date.leaving.nesting.site)

write.csv(old_summ, "data/foraging_distance_females_2018_2020.csv", row.names = F)

