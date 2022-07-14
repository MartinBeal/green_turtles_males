## Create maps of tracking locations and KDEs zoomed in on each region ##
pacman::p_load(raster, mapview, sf, track2KBA, dplyr, ggplot2, stringr, rasterVis)

B <- TRUE
# B <- FALSE

## metadata
meta <- read.csv("data/stage_dates.csv")
meta$PTT <- as.character(meta$PTT)

## land polyons - low res ## 
land_lo <- raster::shapefile("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/geodata/WA_adminboundaries/wca_admbnda_adm0_ocha_18022021.shp")
land_lo <- st_as_sf(land_lo)

## land polyons - hi res ## 
land <- raster::shapefile("data/geodata/GB_SEN_GAM_MAU_merge/GB_SEN_GAM_MAU_merge.shp")
land <- st_as_sf(land)
land <- smoothr::smooth(land)

## 2018-2020 female 95% foraging areas
UD <- readRDS("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/analysis/UDs/raw_filtered/CDF/a_groupCDFs_h1.97_c1_foraging.rds")

UD95 <- UD$`95`

## convert 95% areas to polygon -----------------------------------------------
UD95_poly <- raster::aggregate(
  as(UD95, "SpatialPolygonsDataFrame")
) %>% st_as_sf()

UD95_poly <- smoothr::smooth(UD95_poly)

## 2021 foraging data
if(B == FALSE){
  forage <- readRDS(
    "data/analysis/crawl_3/prediction_single_noB_noland500m_gapfilter_forage_3.rds")
} else {
  forage <- readRDS(
    "data/analysis/crawl_3/prediction_single_wB_noland500m_gapfilter_forage_3.rds")
}

forage <- left_join(forage, 
                    meta[,c("PTT", "foraging_destination")], by=c("ID"="PTT"))

## only male data
forage$sex <- ifelse(forage$sex == "M", "Male", "Female")
forage <- subset(forage, sex=="Male")

## Loop through each region individually
regions <- unique(
  forage$foraging_destination[forage$foraging_destination!="unknown"]
)

forage$ID <- as.factor(forage$ID)

plotcolors <- forage %>% group_by(ID) %>% summarise()
# plotcolors$color <- RColorBrewer::brewer.pal(9, "Set1")
plotcolors <- setNames(RColorBrewer::brewer.pal(9, "Set1"), plotcolors$ID)
# plotcolors <- setNames(scales::hue_pal()(9), plotcolors$ID)

for(i in 1:length(regions)) {
  one <- regions[i]
  
  regtrcks <- forage %>% filter(foraging_destination == one) %>% 
    st_as_sf(
      coords = c("mu.x", "mu.y"), 
      crs = 4326, agr = "constant")
  
  ## get bounding box in lat lon for expanding
  boxll <- st_bbox(regtrcks)

  ## project data
  regtrcks_prj <- regtrcks%>% 
    st_transform(crs=st_crs(UD95_poly))

  regline <- regtrcks_prj %>% 
    group_by(ID, sex) %>% summarise(do_union=FALSE) %>% st_cast("LINESTRING")
    
  xtnt <- st_bbox(regline)
  
  if(one == "Bijagos"){
    # xtnt[1] <- xtnt[1]-.15 ## original
    # xtnt[1] <- xtnt[1]-.25
    # xtnt[1] <- -16.81
    # xtnt[2] <- 10.375
    # xtnt[3] <- xtnt[3]+.1
    # xtnt[3] <- -14.9
    # xtnt[4] <- 11.875
    xtnt_b <- xtnt
    
    # c(xtnt[1]-.23, xtnt[3]+.1), ylim = c(xtnt[2], xtnt[4])
    w <- 9
    h <- 8
    # depth_crp <- crop(depth, extent(xtnt[1]-.22, xtnt[3]+.1, xtnt[2],xtnt[4]))
    # axis labels
    ytix <- seq(10.5, 11.7, by=0.4)
    xtix <- seq(-16.5, -15.0, by=0.5)
  } else if(one=="Senegal"){
    # xtnt[1] <- -17.3
    # xtnt[2] <- 13.18
    # xtnt[3] <- -16.35
    # xtnt[4] <- 14.4
    xtnt_s <- xtnt
    w <- 7
    h <- 10
    # depth_crp <- crop(depth, extent(xtnt[1]-.23, xtnt[3], xtnt[2]-0.1, xtnt[4] + 0.1))
    # axis labels
    ytix <- seq(13.2, 14.4, by=0.4)
    xtix <- seq(-17.1, -16.5, by=0.2)
  } else if(one=="Mauritania"){
    boxll[1] <- -16.6
    boxll[2] <- 20.48
    boxll[3] <- -16.375
    boxll[4] <- 20.68
    
    
    x <- boxll %>% st_as_sfc() %>% 
      st_transform(crs=st_crs(UD95_poly)) %>% st_bbox()
    xtnt <- x
    xtnt_m <- xtnt
    
    w <- 7
    h <- 10
    # depth_crp <- crop(depth, extent(xtnt[1]-.23, xtnt[3], xtnt[2]-0.1, xtnt[4]+0.1))
    # axis labels
    ytix <- seq(19.4, 20.6, by=0.4)
    xtix <- seq(-17.1, -16.3, by=0.2)
  }

  ##
  map <- ggplot() + 
    # geom_sf(data = UD95_poly, inherit.aes = FALSE, fill="#1B9E77", color=NA) +
    geom_sf(data = UD95_poly, inherit.aes = FALSE, fill="paleturquoise", color="black", size=1) +
    geom_sf(data = land, inherit.aes = FALSE, fill="grey60", colour="grey35") +
    # geom_sf(data = regline, color="black", size=0.25, inherit.aes = FALSE) +
    geom_sf(data = regline, aes(color=ID), size=0.65, inherit.aes = FALSE) +
    scale_color_manual(values = plotcolors) +
    coord_sf(
      xlim = c(xtnt[1], xtnt[3]), ylim = c(xtnt[2], xtnt[4]), expand = T) +
    theme_bw() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      plot.margin=unit(c(0,0,0,0), "mm")
    ) +
    ggspatial::annotation_scale( # scale bar
      location = "br",
      style="bar", height=unit(0.25, "cm"),
      width_hint = .1,
      line_width = 1, text_cex = .7)
  # map
  
  if(one == "Bijagos") {p1 <- map} else if(one == "Senegal") {p2 <- map} else {p3 <- map}
  
}
  

### Create reference map w/ inset boxes for foraging locations
fullbbox <- forage %>% st_as_sf(
  coords = c("mu.x", "mu.y"), 
  crs = 4326, agr = "constant") %>% 
  st_transform(crs=st_crs(UD95_poly)) %>% 
  st_bbox()

fullbbox[1] <- fullbbox$xmin - 100000
fullbbox[3] <- fullbbox$xmax + 100000
fullbbox[2] <- fullbbox$ymin - 50000
fullbbox[4] <- fullbbox$ymax + 23000


## project land dataset
land_lo_prj <- st_transform(land_lo, crs=st_crs(UD95_poly))

## create polygons from bboxes
sbox <- st_as_sfc(xtnt_s)
bbox <- st_as_sfc(xtnt_b)
mbox <- st_as_sfc(xtnt_m)

bboxs <- st_union(sbox, bbox)
bboxs <- st_union(bboxs, mbox)

## axis labels
ytix <- seq(12, 20, by=4)
xtix <- seq(-17, -15, by=2)

refmap <- ggplot() +
  # geom_sf(data = regline, color="black", size=0.25, inherit.aes = FALSE) +
  geom_sf(data = land_lo_prj, inherit.aes = FALSE, fill="grey75", colour="grey40") +
  geom_sf(data = bboxs, color="red", fill=NA, size=1, inherit.aes = FALSE) +
  coord_sf(
    xlim = c(fullbbox[1], fullbbox[3]), ylim = c(fullbbox[2], fullbbox[4]), 
    expand = T) +
  # theme_minimal()
  scale_y_continuous(breaks = ytix) + # custom tick labels
  scale_x_continuous(breaks = xtix) + # custom tick labels
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )


library(patchwork)

## long reference map on the side
# layout <- 
#   "
#   AABBBB
#   AABBBB
#   AACCCC
#   AACCCC
#   AADDDD
#   AADDDD
#   "

## long reference map on the side
# layout <- 
#   "
#   AABBBB
#   AABBBB
#   CCDDDD
#   CCDDDD
#   CCDDDD
#   "
# 
# maps <- refmap + p3 + p2 + p1 +
#   plot_layout(design = layout)

## long reference map on the side
layout <- 
  "
  ##CCC
  ##CCC
  AABBB
  AABBB
  AABBB
  "

maps <- p2 + p1 +
  (p3 + inset_element(refmap, -1.08, 0, -.08, 1, align_to = 'full', clip = F)) +
  plot_layout(design = layout,
              guides = 'collect') & theme(legend.position = c(.2, .95))

layout <- 
  "
  #DCCC
  #DCCC
  AABBB
  AABBB
  AABBB
  "

maps <- p2 + 
  p1 +
  (p3 + inset_element(refmap, -1.4, 0, -.4, 1, align_to = 'full', clip = F)) +
  guide_area() +
  plot_layout(design = layout,
              guides = 'collect')

ggsave("C:/Users/Martim Bill/Desktop/test/AAA1119X.png", plot=maps, width = 9, height=9 )

