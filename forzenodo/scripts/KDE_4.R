## Estimate individal utilization distributions for inter-nesting and foraging
# (update MAY0622)

pacman::p_load(track2KBA, amt, dplyr, sp, sf, move, ggplot2, stringr, lubridate)

# datatype <- "filter"
datatype <- "model"

## include ARGOS data?
argos <- TRUE
# argos <- FALSE

## include B locations?
B <- TRUE
# B <- FALSE

if(datatype == "filter"){
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
} else{
  # tracks <- readRDS("data/analysis/stage_delim_4/prediction_single_wB_stages_4.rds")
  ### breeding only data
  ## w/ gap filter
  # tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nsd_gapfilter_breed_3.rds")
  ## w/out gap filter
  if(B == FALSE){
    tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nooutlie_nsd_gapfilter_breed_3.rds")
    # tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nsd_breed_3.rds")
  } else {
    # tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_nooutlie_breed_3.rds")
    tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nooutlie_nsd_gapfilter_breed_3.rds")
    # tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_nooutlie_breed_3.rds")
  }
  tracks <- tracks %>% 
    dplyr::select(-Longitude, -Latitude) %>% 
    rename(Longitude=mu.x, Latitude=mu.y)
}


### Run for breeding or foraging period ---------------------------------------
periods <- c("mate_nest", "foraging")

# for(i in seq_along(periods)){
print(i)
i <- 1
## analyze inter-nesting, foraging or migration?
period <- periods[i]

tracks <- subset(tracks, state == period)

## filter out individuals w/ too few data
tsumm <- tracks %>% group_by(ID) %>% 
  summarise(
    n_tdays = n_distinct(yday(DateTime)),
    n_pnts  = n()
  )

# tracks <- subset(tracks, ID %in% subset(tsumm, n_tdays > 7)$ID)

if(argos == FALSE) {
  tracks <- subset(tracks, Location.Quality == "G")
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## track2KBA analysis ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# project tracks to data-centered projection
TD <- projectTracks(tracks, projType = "azim", custom=T) # equal area azimuthal proj

### remove on-beach points for inter-nesting period data ### 
# if(period == "mate_nest"){
#   poilao <- data.frame(label="Poilão", "Longitude" = -15.725273, "Latitude" = 10.870530)
#   poilao <- st_as_sf(poilao, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")
#   
#   poilao_buff <- poilao %>% st_transform(crs = TD@proj4string) %>% st_buffer(dist = 200) # 700m buffer from island center
#   
#   idxs <- TD %>% st_as_sf() %>% st_within(poilao_buff)
#   inside <- apply(idxs, 1, any)
#   
#   TD <- st_as_sf(TD)[which(inside==F),] %>% as_Spatial()
# }

# estimate smoothing parameter candidates (output in km)
HVALS <- findScale(TD, scaleARS = F)
HVALS

## choose one (reference parameter used here)
# h <- HVALS$href
# h <- .5
h <- 1 # value used in internesting study (Raposo et al)
h

# cres <- 1 ## cell resoluation (kmXkm)
cres <- .25 ## cell resoluation (kmXkm)
# calculate UDs for each individual
KDE  <- estSpaceUse(TD, scale=h, polyOut=F, res = cres)
UD50 <- estSpaceUse(TD, scale=h, polyOut=T, levelUD = 50, res = cres)
UD95 <- estSpaceUse(TD, scale=h, polyOut=T, levelUD = 95, res = cres)
mapKDE(UD50$UDPolygons)
mapKDE(UD95$UDPolygons)

## assess representativeness of just male data ##

# TD_m <- subset(TD, sex=="M")
# KDE  <- estSpaceUse(TD_m, scale=h, polyOut=F, res = cres)
# represent <- repAssess(TD, KDE, iteration=100, nCores=2, levelUD = 50,
#                        avgMethod = "mean", bootTable = F)

## Save ## ------------------------------------------------------------------
# if(argos == T){
  # saveRDS(KDE,
  #         paste0( paste0("data/analysis/KDE_4/", datatype, "/ind_areas", "/individual_UDs_h"),
  #                 round(h,1), "_", period, ".rds") )
  # saveRDS(UD50,
  #         paste0( paste0("data/analysis/KDE_4/", datatype, "/ind_areas", "/individual_UD50_h"),
  #                 round(h,1), "_", period, ".rds") )
  # saveRDS(UD95,
  #         paste0( paste0("data/analysis/KDE_4/", datatype, "/ind_areas", "/individual_UD95_h"),
  #                 round(h,1), "_", period, ".rds") )
# } else {
  # saveRDS(KDE,  
  #         paste0( paste0("data/exploration/KDE_4/gpsonly/ind_areas/individual_UDs_h"),
  #                 round(h,1), "_", period, ".rds") )
  # saveRDS(UD50,  
  #         paste0( paste0("data/exploration/KDE_4/gpsonly/ind_areas/individual_UD50_h"),
  #                 round(h,1), "_", period, ".rds") )
  # saveRDS(UD95,  
  #         paste0( paste0("data/exploration/KDE_4/gpsonly/ind_areas/individual_UD95_h"),
  #                 round(h,1), "_", period, ".rds") )
# }

if(B == FALSE){
  saveRDS(KDE,  
          paste0( paste0("data/analysis/KDE_4/wargos/ind_areas/individual_UDs_noB_h"),
                  round(h,1), "_", period, ".rds") )
  saveRDS(UD50,  
          paste0( paste0("data/analysis/KDE_4/wargos/ind_areas/individual_noB_UD50"),
                  round(h,1), "_", period, ".rds") )
  saveRDS(UD95,  
          paste0( paste0("data/analysis/KDE_4/wargos/ind_areas/individual_UD95_noB_h"),
                  round(h,1), "_", period, ".rds") )
} else {
  saveRDS(KDE,  
          paste0( paste0("data/analysis/KDE_4/wargos/ind_areas/individual_UDs_wB_h"),
                  round(h,1), "_", period, ".rds") )
  saveRDS(UD50,  
          paste0( paste0("data/analysis/KDE_4/wargos/ind_areas/individual_UD50_wB_h"),
                  round(h,1), "_", period, ".rds") )
  saveRDS(UD95,  
          paste0( paste0("data/analysis/KDE_4/wargos/ind_areas/individual_UD95_wB_h"),
                  round(h,1), "_", period, ".rds") )
}

## Split by sex and average together individual UDs  ## 
males   <- raster::validNames(unique(subset(TD, TD$sex == "M")$ID))
females <- raster::validNames(unique(subset(TD, TD$sex == "F")$ID))

## convert UDs from estUDm to rasterStack object ##
KDEraster <- raster::stack(lapply(KDE, function(x) {
  raster::raster(as(x, "SpatialPixelsDataFrame"), values=TRUE)
} ))

# check that N KDEs == N individuals (if not, remove individuals from tracks b4 next step)
nlayers(KDEraster)
n_distinct(TD$ID)

Mraster <- raster::subset(KDEraster, males)
Fraster <- raster::subset(KDEraster, females)

## calculate weighted average KDEs per destination for foraging data --------
# per dest. so that Mauritania doesn't reduce importance of sites at other locales

# if(period == "foraging") {
#   KDEcmbnd_w_list <- list()
#   
#   for(q in seq_len(n_distinct(TD$destination))) {
#     print(paste("q", "==", q))
#     
#     dest <- unique(tracks$destination)[q]
#     
#     if(dest == "unknown") {next}
#     
#     tracks_dest <- tracks %>% 
#       filter(destination == dest)
#     
#     id_w_kde <- unique(tracks_dest$ID)[validNames(unique(tracks_dest$ID)) %in% names(KDEraster)]
#     
#     weights <- tracks_dest %>%
#       dplyr::filter(ID %in% id_w_kde) %>%
#       group_by(ID) %>% summarise(n_pnts = n()) %>% pull(n_pnts)
#     
#     # weighted mean - number of points per ID -------------------------------
#     dest_KDErast <- subset(KDEraster, validNames(id_w_kde))
#     
#     KDEcmbnd_w_list[[q]] <- raster::weighted.mean(dest_KDErast, w=weights)   
#     
#   }
#   
#   ## combine weighted results (non-overlapping so this shouldn't change values)
#   KDEcmbnd_w <- raster::calc(stack(KDEcmbnd_w_list), mean) 
# } else if (period == "mate_nest") {
#   id_w_kde <- unique(TD$ID)[validNames(unique(TD$ID)) %in% names(KDEraster)]
#   # weighted mean - number of points per ID -------------------------------
#   weights <- tracks %>%
#     dplyr::filter(ID %in% id_w_kde) %>% 
#     group_by(ID) %>% summarise(n_pnts = n()) %>% pull(n_pnts)
#   KDEcmbnd_w <- raster::weighted.mean(KDEraster, w=weights)   
#   # arithmetic mean - all individuals equally weighted --------------------
# }

## equal weights per ID ---------------------------------------------------
Mcmbnd_a <- raster::calc(Mraster, mean) # arithmetic mean
Fcmbnd_a <- raster::calc(Fraster, mean) # arithmetic mean

## compare ##
# dev.new()
# sp::plot(KDEcmbnd_w)
# dev.new()
# sp::plot(KDEcmbnd_a)

## Save ## --------------------------------------------------------------
# if(argos==TRUE){
#   # saveRDS(KDEcmbnd_w, 
#   #         paste0( paste0("data/analysis/UDs/", datatype, "/PMF", "/w_groupUD_h"),
#   #                 h, "_c",  cres, "_", period, ".rds"))
#   saveRDS(Mcmbnd_a, 
#           paste0( paste0("data/analysis/KDE_4/", datatype, "/PMF", "/a_groupUD_h"),
#                   round(h,1), "_", period, "_males.rds"))
#   saveRDS(Fcmbnd_a, 
#           paste0( paste0("data/analysis/KDE_4/", datatype, "/PMF", "/a_groupUD_h"),
#                   round(h,1), "_", period, "_females.rds"))
# } else {
#   # saveRDS(KDEcmbnd_w, 
#   #         paste0( paste0("data/analysis/UDs/", datatype, "/PMF", "/w_groupUD_h"),
#   #                 h, "_c",  cres, "_", period, ".rds"))
#   saveRDS(Mcmbnd_a, 
#           paste0( paste0("data/analysis/KDE_4/gpsonly/PMF/a_groupUD_h"),
#                   round(h,1), "_", period, "_males.rds"))
#   saveRDS(Fcmbnd_a, 
#           paste0( paste0("data/analysis/KDE_4/gpsonly/PMF/a_groupUD_h"),
#                   round(h,1), "_", period, "_females.rds"))
# }
# }

if(B == FALSE){
  saveRDS(Mcmbnd_a, 
          paste0( paste0("data/analysis/KDE_4/wargos/PMF/a_groupUD_noB_h"),
                  round(h,1), "_", period, "_males.rds"))
  saveRDS(Fcmbnd_a, 
          paste0( paste0("data/analysis/KDE_4/wargos/PMF/a_groupUD_noB_h"),
                  round(h,1), "_", period, "_females.rds"))
} else {
  saveRDS(Mcmbnd_a, 
          paste0( paste0("data/analysis/KDE_4/wargos/PMF/a_groupUD_wB_h"),
                  round(h,1), "_", period, "_males.rds"))
  saveRDS(Fcmbnd_a, 
          paste0( paste0("data/analysis/KDE_4/wargos/PMF/a_groupUD_wB_h"),
                  round(h,1), "_", period, "_females.rds"))
}

## Derive %UD from full UDs ## -----------------------------------------------
# Mcmbnd_a <- readRDS(
#   paste0( paste0("data/analysis/KDE_4/", datatype, "/PMF", "/a_groupUD_h"),
#           round(h,1), "_", period, "_males.rds"))
# Fcmbnd_a <- readRDS(
#   paste0( paste0("data/analysis/KDE_4/", datatype, "/PMF", "/a_groupUD_h"),
#           round(h,1), "_", period, "_females.rds")
# )

### Convert UD in PMF form to CMF (i.e. to % UD) ### -------------------------
kdelist <- list(Mcmbnd_a, Fcmbnd_a)
# kdelist <- list(KDEcmbnd_a)
for(t in 1:2){
  
  CUD <- kdelist[[t]]
  # CUD <- kdelist[[1]]
  
  pixArea <- raster::res(CUD)[1]
  levelUD <- c(50, 95)
  
  percUDs <- lapply(1:2, function(x) {
    lvl <- levelUD[x]
    df <- data.frame(UD = raster::getValues(CUD)) %>%
      mutate(
        rowname = seq_len(length(raster::getValues(CUD))),
        usage = .data$UD * (pixArea^2)
      ) %>%
      arrange(desc(.data$usage)) %>%
      mutate(cumulUD = cumsum(.data$usage),
             INSIDE = ifelse(.data$cumulUD < (lvl/100), 1, NA)
      ) %>%
      arrange(.data$rowname) %>%
      dplyr::select(.data$INSIDE)
    CUD[] <- df$INSIDE
    
    return(CUD)
  })
  
  names(percUDs) <- levelUD
  
  sp::plot(percUDs[[1]])
  sp::plot(percUDs[[2]])
  
  
  ## Save ## --------------------------------------------------------------------
  if(t == 1){
    # saveRDS(percUDs,
    #         paste0(paste0("data/analysis/KDE_4/", datatype, "/CDF", "/a_groupCDFs_h"), 
    #                round(h,1), "_", period, "_males.rds"))
    if(B == FALSE){
      saveRDS(percUDs,
              paste0(paste0("data/analysis/KDE_4/wargos/CDF", "/a_groupCDFs_h_noB"), 
                     round(h,1), "_", period, "_males.rds"))
    } else {
      saveRDS(percUDs,
              paste0(paste0("data/analysis/KDE_4/wargos/CDF", "/a_groupCDFs_h_wB"), 
                     round(h,1), "_", period, "_males.rds"))
    }

  } else if(t==2){
    # saveRDS(percUDs, 
    #         paste0(paste0("data/analysis/KDE_4/", datatype, "/CDF", "/a_groupCDFs_h"), 
    #                round(h,1), "_", period, "_females.rds"))
    if(B == FALSE){
      saveRDS(percUDs, 
              paste0(paste0("data/analysis/KDE_4/wargos/CDF", "/a_groupCDFs_h_noB"), 
                     round(h,1), "_", period, "_females.rds"))
      saveRDS(percUDs, 
              paste0(paste0("data/analysis/KDE_4/wargos/CDF", "/a_groupCDFs_h_wB"), 
                     round(h,1), "_", period, "_females.rds"))
    }
  }
}
