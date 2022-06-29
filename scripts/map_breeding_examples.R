## Map examples of breeding period movements of male turtles ## 

pacman::p_load(sf, ggplot2, dplyr, mapview)

B <- T

## land polyons - hi res ## 
land <- raster::shapefile("data/geodata/GB_SEN_GAM_MAU_merge/GB_SEN_GAM_MAU_merge.shp")
land <- st_as_sf(land) %>% filter(NAME_0 == "Guinea-Bissau")
land <- smoothr::smooth(land)

## Tracks
if(B == FALSE){
  tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nooutlie_nsd_gapfilter_breed_3.rds")
  # tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_nsd_breed_3.rds")
} else {
  # tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_nooutlie_breed_3.rds")
  tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nooutlie_nsd_gapfilter_breed_3.rds")
  # tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_nsd_nooutlie_breed_3.rds")
}


selids <- subset(tracks, ID %in% c("213020", "213039","213040"))

selids$ID <- factor(selids$ID, levels=c("213040", "213020", "213039"))

## spatialize tracks
ids_sf <- st_as_sf(
  selids,
  coords = c("mu.x", "mu.y"), 
  crs = 4326, agr = "constant")
# plot(pred_sf)

ids_line <- ids_sf %>% 
  group_by(ID, sex) %>% summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# mapview(ids_line, zcol="ID")

xtnt <- st_bbox(ids_line)


map1 <- ggplot() +
  geom_sf(ids_line, mapping=aes(color=ID), inherit.aes = FALSE) + 
  # geom_sf(data=land, inherit.aes = FALSE, fill="grey75", colour="grey40") +
  coord_sf(
    xlim = c(xtnt$xmin, xtnt$xmax), ylim = c(xtnt$ymin, xtnt$ymax), expand = T) +
  ggspatial::annotation_scale(
    location = "bl",
    style="ticks", height=unit(0.3, "cm"),
    line_width = 1, text_cex = .8) +
  theme_bw() +
  theme_void() +
  theme(
    # panel.grid.major = element_blank(),
    # panel.grid.minor = element_blank(),
    panel.background=element_rect(colour="black"))


## Facetted map

map2 <- ggplot() +
  geom_sf(ids_line, mapping=aes(color=ID), inherit.aes = FALSE) + 
  scale_color_brewer(palette = "Dark2") +
  # colorspace::scale_color_discrete_qualitative(palette = "Dynamic") +
  geom_sf(data=land, inherit.aes = FALSE, fill="grey75", colour="grey40") +
  coord_sf(
    xlim = c(xtnt$xmin, xtnt$xmax), ylim = c(xtnt$ymin, xtnt$ymax), expand = T) +
  ggspatial::annotation_scale(
    location = "bl",
    style="ticks", height=unit(0.3, "cm"),
    line_width = 1, text_cex = .8) +
  theme_bw() +
  # theme_void() +
  facet_wrap(~ID, ncol=1) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background=element_rect(colour="black", fill="white"),
    legend.position = "none",
    strip.background = element_rect(fill="white"))
# map2

# ggsave("figures/test2.png", 
#        plot=map2, width=5, height = 8)


## plot
nsd <- ggplot() + 
  # geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.35) + 
  geom_path(data=selids, aes(x = DateTime, y = sqrt(nsd)/1000, color = ID),
            size=0.75) + 
  scale_color_brewer(palette = "Dark2") +
  xlab("") + ylab("Net displacement (km)") + 
  theme_bw() + 
  facet_wrap(~ID, ncol=1, scales="free_x") +
  theme(
    # panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background=element_rect(colour="black", fill="white"),
    legend.position = "none",
    strip.background = element_rect(fill="white"))

## combine

library(patchwork)

comb <- nsd + map2

# ggsave("figures/testcomb.png", 
#        plot=comb, width=9, height = 8)

layout <- 
  "
  AABBB
  AABBB
  AABBB
  "

comb <-  nsd + map2 +
  plot_layout(design = layout)

# ggsave("figures/testcomb2X.png", 
#        plot=comb, width=8, height = 8)


## ----------------------------------------------------------------------------
## Create separate maps for each individual - to be able to inset nsd plots

lineslist <- split(ids_line, ids_line$ID)
pntslist  <- split(selids, selids$ID)

colors <- RColorBrewer::brewer.pal(3, name="Dark2")

maplist <- list()
nsdlist <- list()

for(i in seq_along(lineslist)){
  
  oneline <- lineslist[[i]]
  id <- oneline$ID[1]
  onepnts <- subset(tracks, ID == id)
  
  ## map
  map <- ggplot() +
    geom_sf(data=oneline, color=colors[i], inherit.aes = FALSE) + 
    geom_sf(data=land, inherit.aes = FALSE, fill="grey75", colour="grey40") +
    coord_sf(
      xlim = c(xtnt$xmin, xtnt$xmax), ylim = c(xtnt$ymin, xtnt$ymax), expand = T) +
    {if(id == "213040") ## make scale bar only appears in one plot
      ggspatial::annotation_scale(
        location = "tr",
        style="bar", height=unit(0.3, "cm"),
        line_width = 1, text_cex = 1, text_face = "bold")
    } +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background=element_rect(colour="black", fill="white"),
      legend.position = "none",
      strip.background = element_rect(fill="white")) +
    ggtitle(label=paste("ID:", id))
  
  ## plot
  nsd <- ggplot() + 
    # geom_path(data=tracks_amt_proj, aes(x = t_, y = sqrt(nsd)/1000), size=0.35) + 
    geom_path(data=onepnts, aes(x = DateTime, y = sqrt(nsd)/1000),
              color=colors[i],
              size=0.75) + 
    scale_color_brewer(palette = "Dark2") +
    scale_x_datetime(
      breaks = c(min(onepnts$DateTime), median(onepnts$DateTime), max(onepnts$DateTime)),
      date_labels = "%b %d") +
    xlab("") + ylab("Displacement (km)") + 
    theme_bw() + 
    # facet_wrap(~ID, ncol=1, scales="free_x") +
    theme(
      # panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      # axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background=element_rect(colour="black", fill="white"),
      plot.background = element_rect(fill = "transparent", colour = NA),
      legend.position = "none",
      strip.background = element_rect(fill="white")) +
    scale_y_continuous(breaks=c(0, 50, 100), limits=c(0, 100))
  
  maplist[[i]] <- map
  nsdlist[[i]] <- nsd
}


maps <- (maplist[[1]] + 
           inset_element(nsdlist[[1]], .0, -.05, .38, .4, align_to = 'panel')) /
  (maplist[[2]] + 
     inset_element(nsdlist[[2]], .0, -.05, .38, .4, align_to = 'panel')) /
  (maplist[[3]] + 
     inset_element(nsdlist[[3]], .0, -.05, .38, .4, align_to = 'panel'))
# maps

ggsave("figures/testcomb4.png", 
       plot=maps, width=6, height = 10.5)
