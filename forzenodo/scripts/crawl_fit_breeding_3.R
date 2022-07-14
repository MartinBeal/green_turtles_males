## Fit crawl model to filtered locations and make predictions
# BREEDING (mating/inter-nesting) ONLY

pacman::p_load(crawl, dplyr, ggplot2, sf, amt, lubridate, mapview)

B <- TRUE
# B <- FALSE

if(B == FALSE){
  # tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_noB_nooutlie_breed_4.rds") ## w/ ctmm filter
} else {
  # tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_wB_4.rds")
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_wB_nooutlie_breed_4.rds") ## w/ ctmm filter (only breeding)
}

## filter to just breeding period ---------------------------------------------
tracks <- subset(tracks, state == "mate_nest")

## set up loop ----------------------------------------------------------------
ids <- unique(tracks$ID)
n_ids  <- n_distinct(tracks$ID)
fit.kf <- list()
plocs_list <- list()
olocs_list <- list()

for(i in seq_len(n_ids)){
  print(i)
  # i <- 22
  id <- ids[i]
  ## results tables 
  loc.kf <- data.frame(Time = NULL, mu.y = NULL, mu.x = NULL, se.mu.x = NULL,  
                       se.mu.y = NULL, ln.speed = NULL,  gmt = NULL, ref = NULL) 
  
  loc.clean <- data.frame("PTT" = NULL, "Time" = NULL, "mu.x" = NULL, "mu.y" = NULL,
                          "se.mu.x" = NULL, "se.mu.y" = NULL, "Lon" = NULL, "Lat" = NULL)
  
  ## subset to individual
  one <- subset(tracks, ID == unique(tracks$ID)[i])
  
  one$Location.Quality <- as.factor(one$Location.Quality)
  
  ## assign error radii for different LQs
  ArgosMultFactors <- data.frame(
    Location.Quality = factor(c("3", "2", "1", "0", "A", "B", "G")),
    errX = log(c(1, 1.54, 3.72, 13.51, 23.9, 44.22, 0.1)), # longitudinal error multiplication factor (emf)
    errY = log(c(1, 1.29, 2.55, 14.99, 22, 32.53, 0.1))
  ) 
  
  ## Merge these emf values (longitudinal and latitudinal error as new columns) to the original dataset
  one <- merge(one,
               ArgosMultFactors,  # list of emfs
               by = c("Location.Quality"),      # merge the 2 dataframes thanks to the location classes
               all.x = TRUE)      # keep the whole original dataset
  
  one <- one[order(one$DateTime),] # Order the lines by time values
  # This was causing the rounding errors in crwPostIS
  one <- one[!duplicated(one$DateTime),] # remove duplicated records
  
  
  ###---------- 1. Fit the correlated random walk model ----------###
  
  ### State starting values for the model - these reflect where it starts, and 
  ## the ACCURACY of where you think it starts.
  # See Johnson et al., (2008) for more detailed explanation.
  
  initial <- list(a = c(one$Longitude[1], 0, 
                        one$Latitude[1], 0),
                  P = diag(c(1, 1, 1, 1))
  )
  
  ## Fit the correlated random walk model
  cmat_na <- TRUE
  j <- 0
  while(cmat_na == TRUE){
    j <- j + 1
    print(j)
    fit <- crwMLE(mov.model = ~1, 
                  err.model = list(x = ~errX, y = ~errY), # emfs that we previously set
                  data = one, 
                  coord = c("Longitude","Latitude"), 
                  Time.name = "DateTime", 
                  fixPar = c(NA, 1, NA, 1, NA, NA), # fixPar contains all parameter values with NA for parameters which are to be estimated. 
                  initial.state = initial, 
                  control = list(maxit = 2000, trace = 1, REPORT = 10),
                  initialSANN = list(maxit = 500), # , trace = 1, REPORT = 10
                  attempts = 50)
    cmat_na <- all(is.na(fit$Cmat))
  }
  
  ## Save the fit of the model as a list, in the fit.kf (for fit kalman filter) object.
  fit.kf[i] <- list(fit)
  
  ###---------- 2. Prediction : make interpolated location predictions ----------###
  
  t.step <- 7200 # 2 hr time step
  predTime_2h <- seq(min(one$DateTime), max(one$DateTime), t.step)
  
  ## Let's launch the prediction (estimation of locations ----------------------
  pred_2h <- crwPredict(
    object.crwFit = fit, # a model object from crwMLE
    predTime_2h, # vector of desired prediction times (numeric or POSIXct)
    speedEst = TRUE, # If speedEst is set to TRUE, then animal log-speed is also estimated. 
    return.type = "flat"
  )
  
  pred_2h_df <- as.data.frame(pred_2h) # currently a crwPredict object, we want a dataframe
  
  ## SAVE
  # For this dataframe, we will only keep predicted locs at 2-hrs time interval, so the "p"
  loc_interpolated <- pred_2h[pred_2h$locType == "p",] # "p" for prediction
  loc_observed     <- pred_2h[pred_2h$locType == "o",] # "p" for prediction
  
  loc.kf <- rbind(loc.kf, as.data.frame(loc_interpolated)) # save the predicted locs (2-hr timestep) of the "j" individual (= loc_interpolated dataframe) in the global dataframe which will gather all individuals (= loc.kf)
  
  plocs_list[[i]] <- loc.kf
  olocs_list[[i]] <- loc_observed
  
}

## SAVE -----------------------------------------------------------------------

## w/out B locations
if(B == FALSE){
  saveRDS(fit.kf, "data/analysis/crawl_3/fit_list_3_noB_breed.rds")
  saveRDS(plocs_list, "data/analysis/crawl_3/prediction_single_list_3_noB_breed.rds")
} else {
  # saveRDS(fit.kf, "data/analysis/crawl_3/fit_list_3_wB_breed.rds")
  # saveRDS(plocs_list, "data/analysis/crawl_3/prediction_single_list_3_wB_breed.rds")
  saveRDS(fit.kf, "data/analysis/crawl_3/fit_list_3_wB_nooutlie_breed.rds")
  saveRDS(plocs_list, "data/analysis/crawl_3/prediction_single_list_3_wB_nooutlie_breed.rds")
}


## Visualize one individual's prediction vs. observed locations 
one_p <- plocs_list[[i]] # one individual
one_o <- olocs_list[[i]]

obs_sf <- st_as_sf(
  one_o,
  coords = c("Longitude", "Latitude"), 
  crs = 4326, agr = "constant")

pred <- data.frame(one_p) %>% dplyr::select(mu.y, mu.x, DateTime) 
pred_sf <- st_as_sf(
  pred,
  coords = c("mu.x", "mu.y"), 
  crs = 4326, agr = "constant")
plot(pred_sf)

pred_line <- pred_sf %>% summarise(do_union=FALSE) %>% st_cast("LINESTRING")

mapview::mapview(pred_line) + mapview::mapview(obs_sf, zcol="Location.Quality")


## Make a map of prediction per individual ------------------------------------

# depth <- raster("data/geodata/ETOPO1_patchedw_bathyrastPNBA.tif")

## Load land data ~~~~~~~~~~~~~~~~~
land  <- st_read("C:/Users/Martim Bill/Documents/geodata/diva-gis_admin/guineabissau_admin/GNB_adm0.shp")

for(i in seq_along(plocs_list)){
  print(i)
  # i <- 14
  one_p <- plocs_list[[i]] # one individual
  one_o <- olocs_list[[i]]
  # one_p <- loc.kf
  # one_o <- loc_observed
    
  id  <- one_o$ID[1]
  sex <- one_o$sex[1]
  
  ## observed locations 
  obs_sf <- st_as_sf(
    one_o,
    coords = c("Longitude", "Latitude"), 
    crs = 4326, agr = "constant")
  
  if(id == "213039"){
    xtnt <- st_bbox(obs_sf)
  } else {
    xtnt <- st_bbox(
      c(xmin = -15.93, xmax = -15.49, ymax = 11.11, ymin = 10.75), 
      crs = st_crs(4326)) 
  }
  
  ## crawl predicted locations 
  pred <- data.frame(one_p) %>% dplyr::select(mu.y, mu.x, DateTime) 
  pred_sf <- st_as_sf(
    pred,
    coords = c("mu.x", "mu.y"), 
    crs = 4326, agr = "constant")
  
  pred_line <- pred_sf %>% summarise(do_union=FALSE) %>% st_cast("LINESTRING")
  
  if(sex == "F") {cl <- "red"} else {cl <- "blue"}
  
  ## map it
  ggplot() +
    geom_sf(
      data = obs_sf, color="black", alpha=1, size=0.25, inherit.aes = FALSE) +
    geom_sf(
      data = pred_line, color=cl, alpha=1, size=0.25, inherit.aes = FALSE) +
    geom_sf(
      data = land, inherit.aes = FALSE, fill="grey65", color=NA) +
    coord_sf(
      xlim = c(xtnt[1], xtnt[3]), ylim = c(xtnt[2], xtnt[4]), expand = T) +
    theme_bw() +
    ggspatial::annotation_scale(
      style="ticks", height=unit(0.35, "cm"), text_cex = 1)
  
  ## save it
  
  ## w/out B locations
  if(B == FALSE){
    ggsave(paste0("figures/crawl_pred/maps/", id, "noB.png"), width = 7, height=6)
  } else {
    # ggsave(paste0("figures/crawl_pred/maps/", id, "_wB.png"), width = 7, height=6)
    ggsave(paste0("figures/crawl_pred/maps/", id, "_wB_nooutlie.png"), width = 7, height=6)
  }
  
}

### calculate net-squared displacement on crawl prediction data ---------------
## Use equidistant projection so NSD is correctly calculated (and in m)
# w/ Poilão as reference point

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

plocs <- do.call(rbind, plocs_list)

tracks_amt <- plocs %>% 
  make_track(.x=mu.x, .y=mu.y, .t=DateTime, 
             id = ID, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_d"), 
             all_cols = T) %>% rename(id = ID)

tracks_amt_proj <- transform_coords(tracks_amt, crs_to=proj)

alist <- list()
for(i in 1:n_distinct(tracks_amt_proj$id)){
  one <- subset(tracks_amt_proj, id == unique(tracks_amt_proj$id)[i])
  one_plocs <- subset(plocs, ID == one$id[1])
  one_plocs$nsd <- nsd(one)
  alist[[i]] <- one_plocs 
}

plocs <- do.call(rbind, alist)

## Clean it up
plocs <- as_tibble(plocs) %>% 
  dplyr::select(-locType, -"eRes", -HDOP, -SatNum, -step_t, IDgap, -sdafilter,
                -mcfilter) %>% 
  arrange(ID)

## plot
ggplot() + 
  # geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.35) + 
  geom_path(data=plocs, aes(x = DateTime, y = sqrt(nsd)/1000),
            color = "red", size=0.75) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~ID)

### Save single prediction data ------------------------------------------------

if(B == FALSE){
  # saveRDS(plocs, "data/analysis/crawl_3/prediction_single_noB_nsd_breed_3.rds")
  saveRDS(plocs, "data/analysis/crawl_3/prediction_single_noB_nooutlie_nsd_breed_3.rds")
} else {
  saveRDS(plocs, "data/analysis/crawl_3/prediction_single_wB_nsd_nooutlie_breed_3.rds")
}

