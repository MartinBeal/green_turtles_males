### filter out predicted periods occurring during gaps of >=48 h ###
#------------------------------------------------------------------------------
#***ONLY set up for breeding period at the moment

pacman::p_load(trip, ggplot2, amt)

# period <- "breeding"
# period <- "migration"
period <- "foraging"

B <- TRUE
# B <- FALSE

if(period == "breeding"){
  ## filtered data
  if(B == FALSE){
    mc <- readRDS("data/analysis/stage_delim_4/mcconnell_noB_nooutlie_breed_4.rds")
  } else {
    # tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_wB_4.rds")
    mc <- readRDS("data/analysis/stage_delim_4/mcconnell_wB_nooutlie_breed_4.rds")
  }
  ## Modelled data
  if(B == FALSE){
    # tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nsd_breed_3.rds")
    crawl <- readRDS("data/analysis/crawl_3/prediction_single_noB_nooutlie_nsd_breed_3.rds")
  } else {
    crawl <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_nooutlie_breed_3.rds")
  }
} else if(period == "migration"){
  ## filtered data
  if(B == FALSE){
    mc <- readRDS("data/analysis/stage_delim_4/mcconnell_noB_noland5km_migrate_4.rds")
  } else {
    mc <- readRDS("data/analysis/stage_delim_4/mcconnell_wB_noland5km_migrate_4.rds")
  }
  ## Modelled data
  if(B == FALSE){
    crawl <- readRDS("data/analysis/crawl_3/prediction_single_noB_noland5km_migrate_3.rds")
  } else {
    crawl <- readRDS("data/analysis/crawl_3/prediction_single_wB_noland5km_migrate_3.rds")
  }

} else if(period == "foraging"){
  ## filtered data
  if(B == FALSE){
    mc <- readRDS("data/analysis/stage_delim_4/mcconnell_noB_noland500m_forage_4.rds")
  } else {
    mc <- readRDS("data/analysis/stage_delim_4/mcconnell_wB_noland500m_forage_4.rds")
  }
  ## Modelled data
  if(B == FALSE){
    crawl <- readRDS("data/analysis/crawl_3/prediction_single_noB_nsd_noland500m_forage_3.rds")
  } else {
    crawl <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_noland500m_forage_3.rds")
  }
  
}


## Remove crawl-predicted data from gap periods -------------------------------

## Set gap size here ##
gapsize <- 24 #h
# gapsize <- 48 #h

alist <- list()

for(i in seq_along(unique(crawl$ID))){
  print(i)
  id <- unique(crawl$ID)[i]
  one_mc <- subset(mc,    ID == id)
  one_cr <- subset(crawl, ID == id)
  
  ## implement gap size here ##
  one_mc$IDgap <- trip::sepIdGaps(one_mc$ID, one_mc$DateTime, 3600 * 24 ) # hours
  
  n_gaps <- n_distinct(one_mc$IDgap) - 1
  
  if(n_gaps == 0){ 
    alist[[i]] <- one_cr 
    next } else {
    startgap <- one_mc[which(one_mc$IDgap != dplyr::lag(one_mc$IDgap))-1, ]$DateTime
    endgap   <- one_mc[which(one_mc$IDgap != dplyr::lag(one_mc$IDgap)), ]$DateTime
    
    ## deal w/ multiple gaps
    rmv_list <- lapply(seq_along(startgap), function(x){
      rmv <- which(one_cr$DateTime > startgap[x] & one_cr$DateTime < endgap[x])
      return(rmv)
    })
    
    rmv_rows <- unlist(rmv_list)
    
    one_cr_f <- one_cr[-rmv_rows, ]
    
    alist[[i]] <- one_cr_f
  }
}

crawl_f <- do.call(rbind, alist)

round(nrow(crawl_f) / nrow(crawl) * 100) # % predicted points retained

# new IDgaps - depends on period
# if(period == "migration"){
  crawl_f$IDgap <- trip::sepIdGaps(crawl_f$ID, crawl_f$DateTime, 3600 * gapsize ) # hours
# } else {
#   crawl_f$IDgap <- trip::sepIdGaps(crawl_f$ID, crawl_f$DateTime, 3600 * 24 ) # hours
# }


## SAVE ## --------------------------------------------------------------------
# 
if(period == "breeding"){
  if(B == FALSE){
    # saveRDS(crawl_f, "data/analysis/crawl_3/prediction_single_noB_nsd_gapfilter_breed_3.rds")
    saveRDS(crawl_f, "data/analysis/crawl_3/prediction_single_noB_nooutlie_nsd_gapfilter_breed_3.rds")
  } else {
    saveRDS(crawl_f, "data/analysis/crawl_3/prediction_single_wB_nooutlie_nsd_gapfilter_breed_3.rds")
  }
} else if(period == "migration"){
    if(B == FALSE){
      saveRDS(crawl_f, 
              "data/analysis/crawl_3/prediction_single_noB_noland5km_gapfilter_migrate_3.rds")
    } else {
      saveRDS(crawl_f, 
              "data/analysis/crawl_3/prediction_single_wB_noland5km_gapfilter_migrate_3.rds")
    }
} else if(period == "foraging"){
  if(B == FALSE){
    saveRDS(crawl_f, 
            "data/analysis/crawl_3/prediction_single_noB_noland500m_gapfilter_forage_3.rds")
  } else {
    saveRDS(crawl_f, 
            "data/analysis/crawl_3/prediction_single_wB_noland500m_gapfilter_forage_3.rds")
  }
  
}

## Displacement graph  ---------------
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


tracks_amt <- crawl_f %>% 
  make_track(.x=mu.x, .y=mu.y, .t=DateTime, 
             id = ID, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_d"), 
             all_cols = T) %>% rename(id = ID)

tracks_amt_proj <- transform_coords(tracks_amt, crs_to=proj)

alist <- list()
for(i in 1:n_distinct(tracks_amt_proj$id)){
  one <- subset(tracks_amt_proj, id == unique(tracks_amt_proj$id)[i])
  one_plocs <- subset(crawl_f, ID == one$id[1])
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
  # geom_path(data=plocs, aes(x = DateTime, y = sqrt(nsd)/1000),
  #           color = "black", size=0.5) +
  geom_path(data=plocs, aes(x = DateTime, y = sqrt(nsd)/1000, group=IDgap),
            color = "red", size=0.75) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~ID)

if(period == "migration"){
  if(B == FALSE){
    ggsave("figures/nsd/migration_gapfilter_noB_noland5km_crawlfit.png", width = 8, height=6)
  } else {
    ggsave("figures/nsd/migration_gapfilter_wB_noland5km_crawlfit.png", width = 8, height=6)
  }
} else if(period == "foraging"){
    if(B == FALSE){
      ggsave("figures/nsd/foraging_gapfilter_noB_noland500m_crawlfit.png", width = 8, height=6)
    } else {
      ggsave("figures/nsd/foraging_gapfilter_wB_noland500m_crawlfit.png", width = 8, height=6)
    }
  }

