## Overlap foragaing locations on home range areas from Patricio 2022 ---------

pacman::p_load(dplyr, sf, sp, mapview)

UD <- readRDS("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/analysis/UDs/raw_filtered/CDF/a_groupCDFs_h1.97_c1_foraging.rds")

UD95 <- UD$`95`

## convert 95% areas to polygon -----------------------------------------------
UD95_poly <- raster::aggregate(
  as(UD95, "SpatialPolygonsDataFrame")
)

B <- TRUE
# B <- FALSE

## fully tracking data filtered data ------------------------------------------
##*** need to update this to modelled data, eventually
# tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds") # n B pnts
# forage <- subset(tracks, state == "foraging")

if(B == FALSE){
  forage <- readRDS(
    "data/analysis/crawl_3/prediction_single_noB_noland500m_gapfilter_forage_3.rds"
    )
} else {
  forage <- readRDS(
    "data/analysis/crawl_3/prediction_single_wB_noland500m_gapfilter_forage_3.rds")
}

forage$sex <- ifelse(forage$sex == "M", "Male", "Female")

forage %>% group_by(sex) %>% summarise(n_id=n_distinct(ID))

## spatialize
# forage_sf <- st_as_sf(forage, coords = c("Longitude", "Latitude"),
#                       crs = 4326, agr = "constant")
forage_sf <- st_as_sf(forage, coords = c("mu.x", "mu.y"),
                      crs = 4326, agr = "constant")

## only males
forage_sf_m <- subset(forage_sf, sex == "Male")

forage_sp_m <- as_Spatial(forage_sf_m)

## project to custom CRS used in KDE
forage_sp_m <- spTransform(forage_sp_m, CRSobj = CRS(proj4string(UD95)))

## Overlap binary - are male points w/in female foraging 95% area? ------------
forage_sp_m$over_fem <- sp::over(forage_sp_m, UD95_poly)
forage_sp_m$over_fem <- ifelse(is.na(forage_sp_m$over_fem), F, T)

mapview(UD95_poly) + mapview(forage_sp_m, zcol="over_fem")

## Summarize overlap w/ females -----------------------------------------------
forage_m <- as.data.frame(forage_sp_m)

ovrsumm_id <- forage_m %>% group_by(ID) %>% 
  summarise(
    n_pnts = n(),
      perc = sum(over_fem) / n() * 100
  )

ovrsumm_id %>% filter()

ovrsumm_id %>% summarise(
  n_id    = n(),
  n_over  = sum(ifelse(perc > 0,T,F)),
  mn_perc = mean(perc),
  sd_perc = sd(perc),
  md_perc = median(perc),
  min_perc = min(perc),
  max_perc = max(perc)
)

## calculate how close individuals are which do not overlap w/ female areas at all

nooverids <- subset(forage_sp_m, ID %in% c("213038", "213043"))
UD95_p_sf <- st_as_sf(UD95_poly)

alist <- lapply(split(nooverids, nooverids$ID), function(x){
  x_sf <- st_as_sf(x)
  x_sf$near_fem <- st_distance(x_sf, UD95_p_sf)
  return(as_tibble(x_sf))
})

nearest <- do.call(rbind, alist)


nearest %>% group_by(ID) %>% 
  summarise(
    mn_dist = mean(near_fem)/1000,
    sd_dist = sd(near_fem)/1000
  )
