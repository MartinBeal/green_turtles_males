## Filter out outliers using CTMM outlier detection functionality -------------
# This filter is applied AFTER speed filters etc.

pacman::p_load(ctmm, move)

# B <- TRUE
B <- FALSE

if(B == FALSE){
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
} else {
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_wB_4.rds")
}

## metadata - **including individual-specific displacement thresholds**
meta <- read.csv("data/stage_dates.csv")

## filter to just one stage ---------------------------------------------------
breed <- subset(tracks, state == "mate_nest")
n_ids <- n_distinct(breed$ID)

outlist <- list()

for(i in seq_len(n_ids)){
  ## Run an individual at a time
  # i <- 15
  id  <- unique(breed$ID)[i]
  print(id)
  one <- subset(breed, ID == unique(breed$ID)[i]) # try 13 too
  
  # convert to move object
  one_move <- move::move(
    x      = one$Longitude, 
    y      = one$Latitude, 
    time   = one$DateTime, 
    proj   = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"),  # common project. for all individuals, custom set by tripSplit
    animal = one$ID,
    data   = one
  )
  mapview(one_move)
  
  ## Convert to telemetry object (ctmm)
  one_tel <- as.telemetry(one_move)
  plot(one_tel, error=2) # red circles illustrate error radii for each point
  
  ## check and remove outliers ------------------------------------------------
  OUT <- outlie(one_tel)
  plot(OUT, units=FALSE, main=id)
  # print(id)
  
  ## apply thresholds from metadata table
  if(B == FALSE){
    ## get individual specific deviation threshold (how far from core distribuion)
    thresh <- subset(meta, PTT == id)$dist_thresh_noB
    
    if(is.na(thresh)){ 
      one_f <- one
    } else if(id == "213044") {
      BAD <- one_move$Location.Quality == "G" & OUT$speed > .5
      one_tel <- one_tel[!BAD,]
      plot(one_tel) # red circles illustrate error radii for each point
      one_f <- one[!BAD,]
    } else{
      BAD <- OUT$distance > thresh # distance from distribution 'core' (m)
      one_tel <- one_tel[!BAD,]
      plot(one_tel) # red circles illustrate error radii for each point
      one_f <- one[!BAD,]
    }
  } else {
    type <- subset(meta, PTT == id)$thresh_type
    
    if(type == "speed"){
      thresh <- subset(meta, PTT == id)$sp_thresh
      BAD <- OUT$speed > thresh # distance from distribution 'core' (m)
      one_tel <- one_tel[!BAD,]
      plot(one_tel) # red circles illustrate error radii for each point
      one_f <- one[!BAD,]
    } else if(type == "dist"){
      thresh <- subset(meta, PTT == id)$dist_thresh_wB
      BAD <- OUT$distance > thresh # distance from distribution 'core' (m)
      one_tel <- one_tel[!BAD,]
      plot(one_tel) # red circles illustrate error radii for each point
      one_f <- one[!BAD,]
    } else if(type == "none"){
      one_f <- one
    } else if(type == "or"){
      thresh1 <- subset(meta, PTT == id)$sp_thresh
      thresh2 <- subset(meta, PTT == id)$dist_thresh_wB
      BAD <- OUT$speed > thresh1 | OUT$distance > thresh2 # distance from distribution 'core' (m)
      one_tel <- one_tel[!BAD,]
      plot(one_tel) # red circles illustrate error radii for each point
      one_f <- one[!BAD,]
    } else if(type == "specific"){
      thresh <- subset(meta, PTT == id)$specific
      BAD <- as.numeric(unlist(stringr::str_split(thresh, pattern=" ")))
      one_tel <- one_tel[-BAD,]
      plot(one_tel) # red circles illustrate error radii for each point
      one_f <- one[-BAD,]
    }
    
  }
  
  outlist[[i]] <- one_f

}

out <- do.call(rbind, outlist)

if(B == FALSE){
  saveRDS(out, "data/analysis/stage_delim_4/mcconnell_noB_nooutlie_breed_4.rds")
} else {
  saveRDS(out, "data/analysis/stage_delim_4/mcconnell_wB_nooutlie_breed_4.rds")
}
