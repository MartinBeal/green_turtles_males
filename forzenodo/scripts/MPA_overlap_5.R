### estimate proportion of positions within and outside MPAs ##----------------

pacman::p_load(dplyr, lubridate, sf, ggplot2, sp)

## Metadata -- sex info
meta <- read.csv("data/stage_dates.csv") 
meta$id <- as.character(meta$PTT)

## Males -- tracking data
tracks  <- readRDS("data/analysis/stage_delim_4/prediction_single_noB_stages_4.rds")

tracks  <- subset(tracks,  sex == "M")

breed <- tracks %>% 
  filter(state == "mate_nest")

breed <- breed %>% 
  dplyr::select(-Longitude, -Latitude) %>% 
  rename(Longitude=mu.x, Latitude=mu.y)

# project tracks to data-centered projection
TD <- track2KBA::projectTracks(breed, projType = "azim", custom=T) # equal area azimuthal proj

TD_ll <- sp::spTransform(TD, CRS("+init=epsg:4326")) # back to lat/lon


### estimate proportion of positions within and outside MPAs ##---------------
mpas <- raster::shapefile("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/geodata/WDPA_MPAs_Wafrica_May2021/WDPA_MPAs_Wafrica_May2021_dissolve.shp")
babr <- raster::shapefile("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/geodata/WDPA_MPAs_Wafrica_May2021/BABR_polygon.shp")

# combine and union BABR with MPAs dataset
cons_areas <- raster::aggregate(rbind(mpas[,-c(31:32)], babr))
# mapview(cons_areas)
# mapview(mpas)

## extract info of MPA which each point falls over (NA if none) ##
over_mpas <- over(TD_ll, mpas)

# add info on which PA each point overlaps, and if none call is 'no PA'
TD_ll$WDPA_PID <- ifelse(is.na(over_mpas$WDPA_PID), "no PA", over_mpas$WDPA_PID)
TD_ll$PA_NAME <- ifelse(is.na(over_mpas$NAME), "no PA", over_mpas$NAME)
# table(TD_ll$PA_NAME)  # how many points fall within EACH PA?
TD_ll$over_PA <- ifelse(TD_ll$WDPA_PID == "no PA", F, T)
table(TD_ll$over_PA)    # how many points fall in ANY PA?

## including BABR ##
over_cons <- over(TD_ll, cons_areas)

# add info on which CA each point overlaps, and if none call is 'no PA'
TD_ll$over_CA <- ifelse(is.na(over_cons), F, T)
table(TD_ll$over_CA)    # how many points fall in ANY PA?


### Calculating MPA coverage of tracking points in various ways -------------

tracks <- TD_ll@data

## summarize total points, and proportion of points (including IDs which don't visit at all ##
gen_summ <- tracks %>% summarise(
  n_ind   = n_distinct(ID),
  n_pnts  = n(),                  # total number of tracking locations
  pnts_in_pa = sum(over_PA),      # total number of locations falling w/in any PA
  prop_in_pa = pnts_in_pa/n_pnts, # proportion of locations w/in PAs
  pnts_in_ca = sum(over_CA),      # total number of locations falling w/in any PA
  prop_in_ca = pnts_in_ca/n_pnts  # proportion of locations w/in PAs
)

## Averages per individual ## ----------------------------------------------
## summarize total points, and proportion of points ##
id_summ <- tracks %>% group_by(ID) %>% summarise(
  n_pnts  = n(),              # total number of tracking locations
  pnts_in_pa = sum(over_PA),     # total number of locations falling w/in any PA
  prop_in_pa = pnts_in_pa/n_pnts, # proportion of locations w/in PAs
  pnts_in_ca = sum(over_CA),     # total w/in any CONSERVATION AREA
  prop_in_ca = pnts_in_ca/n_pnts # prop. of locations w/in CONSERVATION AREA
) 

## averages for all IDs
gen_id_summ <- id_summ %>% summarise(
  n_ind    = n_distinct(ID),
  tot_pnts = sum(n_pnts),
  m_pnts   = mean(n_pnts),
  sd_pnts  = sd(n_pnts),
  m_pnts_in_pa  = mean(pnts_in_pa), # Protected areas only
  sd_pnts_in_pa = sd(prop_in_pa),
  m_prop_in_pa  = mean(prop_in_pa),
  sd_prop_in_pa = sd(prop_in_pa),
  m_pnts_in_ca  = mean(pnts_in_ca), # all conservation areas
  sd_pnts_in_ca = sd(prop_in_ca),   # including BABR
  m_prop_in_ca  = mean(prop_in_ca),
  sd_prop_in_ca = sd(prop_in_ca)
)


## save ## ~~~
data.table::fwrite(gen_summ, "data/summaries/pnts_in_pa_gen.csv")

data.table::fwrite(gen_id_summ, "data/summaries/pnts_in_pa_id.csv")
