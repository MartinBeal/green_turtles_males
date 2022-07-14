## Apply buffered land filter to foraging data ## -----------------------------

pacman::p_load(crawl, dplyr, ggplot2, sf, amt, lubridate, mapview)

# B <- TRUE
B <- FALSE

if(B == FALSE){
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds") ## w/ ctmm filter
} else {
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_wB_4.rds")
}

## just foraging points 
tracks <- subset(tracks, state == "foraging")

## land polyons ## 
land <- raster::shapefile("data/geodata/GB_SEN_GAM_MAU_merge/GB_SEN_GAM_MAU_merge.shp")
land <- st_as_sf(land)

## project data 
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

tracks_sf <- st_as_sf(
  tracks,
  coords = c("Longitude", "Latitude"), 
  crs = 4326, agr = "constant") %>% st_transform(crs=proj)

land <- st_transform(land, crs=proj)


## compute buffered polygons
buffer_size <- (-500) # 1 km

land_dissolve <- st_union(land)
land_dissolve <- sfheaders::sf_remove_holes(land_dissolve)

land_buff <- st_buffer(land_dissolve, buffer_size) 

## Remove points which fall on land beyond coastal buffer?

tracks_sf$rown <- seq_len(nrow(tracks_sf))
rows2rmv  <- st_intersection(tracks_sf, land_buff)$rown

tracks_sf2 <- tracks_sf[-rows2rmv, ]
tracks_f <- tracks[-rows2rmv, ]

# mapview(land_buff) + mapview(tracks_sf2)

if(B == FALSE){
  saveRDS(tracks_f, "data/analysis/stage_delim_4/mcconnell_noB_noland500m_forage_4.rds")
} else {
  saveRDS(tracks_f, "data/analysis/stage_delim_4/mcconnell_wB_noland500m_forage_4.rds")
}
