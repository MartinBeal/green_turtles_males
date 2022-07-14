## Calculate how far turtles range from tagging site during breeding

pacman::p_load(dplyr, amt, ggplot2)

## Crawl-predicted trajectories for breeding ----------------------------------
plocs <- readRDS("data/analysis/crawl_3/prediction_single_wB_nooutlie_nsd_gapfilter_breed_3.rds")## Load land 
## GB land shapefile
land  <- st_read("C:/Users/Martim Bill/Documents/geodata/diva-gis_admin/guineabissau_admin/GNB_adm0.shp")
## tagging locations for each individual
taglocs <- read.csv("data/males_females_trackingsumm_2021_RP_Martin.csv")

n_ids <- n_distinct(plocs$ID)

## Calculate displacement from tagging location -------------------------------

## Add tagging location as first point for nsd calculation

## location of Poilão
poilao_meio <- data.frame(label=c("Poilão", "Meio"), 
                          "Longitude" = c(-15.726667, -15.666024), 
                          "Latitude" = c(10.864722, 10.976397)
)
# poilao_meio <- st_as_sf(poilao_meio, coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

proj <- sp::CRS(
  paste0(
    "+proj=aeqd +lon_0=", poilao_meio$Longitude[1],
    " +lat_0=", poilao_meio$Latitude[1],  # latitude of true scale
    " +x_0=0 +y_0=0", sep = ""
  ))

## View tagging locations on map

taglocs_sf <- st_as_sf(taglocs, coords = c("long", "lat"), 
                       crs = 4326, agr = "constant")
mapview::mapview(taglocs_sf)

## Add tagging coordinates for each individual to first line -------------------

plocs_list <- lapply(split(plocs, plocs$ID), function(x){
  x <- rbind(x[1, ], x)
  id  <- x$ID[1]
  sex <- x$sex[1]
  
  taglat <- subset(taglocs, PTT == id)$lat
  taglon <- subset(taglocs, PTT == id)$lon
  
  x$DateTime[1] <- x$DateTime[1] - 2*60*60
  x$mu.x[1] <- taglon
  x$mu.y[1] <- taglat
  return(x)
})

plocs_adj <- do.call(rbind, plocs_list)

tracks_amt <- plocs_adj %>% 
  make_track(.x=mu.x, .y=mu.y, .t=DateTime, 
             id = ID, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_d"), 
             all_cols = T) %>% rename(id = ID)

tracks_amt_proj <- transform_coords(tracks_amt, crs_to=proj)

alist <- list()
for(i in 1:n_distinct(tracks_amt_proj$id)){
  one <- subset(tracks_amt_proj, id == unique(tracks_amt_proj$id)[i])
  one_plocs <- subset(plocs_adj, ID == one$id[1])
  one_plocs$nsd <- nsd(one)
  alist[[i]] <- one_plocs 
}

plocs_adj <- do.call(rbind, alist)

## Summarise max/avg distance from tagging site -------------------------------

displ_id <- plocs_adj %>% group_by(ID, sex) %>% 
  summarise(
    mn_displ = mean(sqrt(nsd)/1000),
    sd_displ = sd(sqrt(nsd)/1000),
    md_displ = median(sqrt(nsd)/1000),
    mx_displ = max(sqrt(nsd)/1000)
  )

displ_sex <- displ_id %>% group_by(sex) %>% 
  summarise(
    n_id     = n(),
    mn_mn_displ = mean(mn_displ),
    sd_displ = sd(mn_displ),
    mn_md_displ = mean(md_displ),
    mn_mx_displ = mean(mx_displ),
    mx_mx_displ = max(mx_displ)
  )

write.csv(displ_id, "data/summaries/idsumm_displace_breed_crawlgapfilter_n22.csv", 
          row.names = F)
write.csv(displ_sex, "data/summaries/sexsumm_displace_breed_crawlgapfilter_n22.csv", 
          row.names = F)


## plot displacement during breeding

ggplot() + 
  # geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.35) + 
  geom_path(data=plocs_adj, aes(x = DateTime, y = sqrt(nsd)/1000, color = sex),
            size=0.75) + 
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~ID, scales="free_x") +
  theme(axis.text.x = element_text(angle = 35, vjust = 1, hjust=1))

ggsave("figures/nsd/breed_wB_n22.png", width=10, height=8.5)
