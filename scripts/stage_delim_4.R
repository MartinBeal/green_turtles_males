## Delimit stages for each individual, based on visual inspection of NSD graphs

meta <- read.csv("data/stage_dates.csv") # metadata

meta$PTT <- as.character(meta$PTT)
meta <- meta %>% mutate(
  PTT = as.character(PTT),
  end_nesting    = parse_date_time(end_nesting, orders="dmy HM"),
  start_foraging = parse_date_time(start_foraging, orders="dmy HM")
)

## Movement data - either filtered, or model-predicted ------------------------
B <- TRUE
# B <- FALSE

## speed filtered
if(B == FALSE){
  tracks <- readRDS("data/analysis/mcconnell_2/mcconnell_2_noB.rds")
} else {
  ## mcconnell or just speed filter?
  # tracks <- readRDS("data/analysis/mcconnell_2/mcconnell_2_wB.rds")
  tracks <- readRDS("data/analysis/speed_angle_1/speed_angle_1.rds") ###
}

## model prediction
# tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_gapfilter_3.rds")
# tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nsd_gapfilter_3.rds")

tracks <- tracks %>% 
  left_join(
    meta[,c("PTT", "end_nesting", "start_foraging")],
    by=c("ID"="PTT")
  )


tracks$state <- ifelse(
  tracks$DateTime <= tracks$end_nesting | is.na(tracks$end_nesting), "mate_nest", 
         ifelse(tracks$DateTime > tracks$end_nesting & 
                  is.na(tracks$start_foraging), "migration", 
                ifelse(tracks$DateTime >= tracks$start_foraging, "foraging", 
                       "migration")
  ))

tracks$state <- factor(tracks$state, levels = c("mate_nest", "migration", "foraging"))

one <- subset(tracks, ID == "224389")

one_sf <- st_as_sf(
  one,
  coords = c("Longitude", "Latitude"),
  crs = 4326, agr = "constant")

# mapview(one_sf)
mapview(one_sf, zcol="state")

## plot NSD w/ periods separated
ggplot() + 
  geom_path(data=tracks, aes(x = DateTime, y = sqrt(nsd)/1000), size=0.5) + 
  geom_path(data=tracks, 
            aes(x = DateTime, y = sqrt(nsd)/1000, color=state, group=IDgap),
            size=1.5) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~ID)

## SaVe
if(B == FALSE){
  ggsave("figures/nsd/nsd_mcconnellfilter_noBpnts_stages_n25.png", width = 8, height=6)
} else {
  # ggsave("figures/nsd/nsd_mcconnellfilter_wBpnts_stages_n25.png", width = 8, height=6)
  ggsave("figures/nsd/nsd_speedfilter_stages_n25.png", width = 8, height=6)
}


## Clean it up
# tracks <- as_tibble(tracks) %>% 
#   dplyr::select(-end_nesting, -start_foraging)

### Save stage-classified data ------------------------------------------------
## Filtered data
if(B == FALSE){
  saveRDS(tracks, "data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
} else {
  # saveRDS(tracks, "data/analysis/stage_delim_4/mcconnell_stages_wB_4.rds")
  saveRDS(tracks, "data/analysis/stage_delim_4/speedfilter_stages_wB_4.rds")
}

## Model data
# saveRDS(tracks, "data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
# saveRDS(tracks, "data/analysis/stage_delim_4/prediction_single_wB_stages_4.rds")
# saveRDS(tracks, "data/analysis/stage_delim_4/prediction_single_noB_stages_4.rds")
