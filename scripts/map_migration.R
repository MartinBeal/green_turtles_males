## Map male and female migratory trajectories ## ------------------------------

pacman::p_load(dplyr, lubridate, ggplot2, sf)

source("C:/Users/Martim Bill/Documents/R/source_scripts/pt2pt_fxns.WV.R")

meta <- read.csv("data/stage_dates.csv") # metadata
meta$PTT <- as.character(meta$PTT)

## include B locations or not? ------------------------------------------------
# B <- TRUE
B <- FALSE

## Modelled data
if(B == FALSE){
  tracks <- readRDS("data/analysis/crawl_3/prediction_single_noB_noland5km_migrate_3.rds")
} else {
  tracks <- readRDS("data/analysis/crawl_3/prediction_single_wB_noland5km_migrate_3.rds")
}

## location of Poilão
poilao <- data.frame("Longitude" = -15.726667, "Latitude" = 10.864722) 
poilao <- poilao %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

## land polyons ## 
land <- raster::shapefile("C:/Users/Martim Bill/Documents/other_projects/R.Patricio_turtleMPAs/data/geodata/WA_adminboundaries/wca_admbnda_adm0_ocha_18022021.shp")
land <- st_as_sf(land)

## remove id 224389 whose mig track is very spotty (not included in calcs) ----
tracks <- subset(tracks, ID != "224389")

## prettify 
tracks$sex <- ifelse(tracks$sex == "M", "Male", "Female")

## spatialize tracks
pred_sf <- st_as_sf(
  tracks,
  coords = c("mu.x", "mu.y"), 
  crs = 4326, agr = "constant")
# plot(pred_sf)

pred_line <- pred_sf %>% 
  group_by(ID, sex) %>% summarise(do_union=FALSE) %>% st_cast("LINESTRING")
# plot(pred_line)

## show track endpoints 
# endpnts <- pred_sf %>% group_by(ID) %>% slice(n()) %>% 
#   dplyr::select("ID", "sex", "Latitude", "Longitude", "DateTime", "nsd", "state")

endpnts <- tracks %>% group_by(ID) %>% slice(n()) %>% 
  dplyr::select("ID", "sex", "mu.y", "mu.x", "DateTime", "nsd", "state")

## jitter end points of two males so they show up better on map 

endpnts <- mutate(endpnts, 
                  mu.y = ifelse(ID %in% c("213039", "213042"), mu.y-0.06, mu.y))

forage_locs <- readRDS("data/analysis/forage_locs_n20.rds")

# forage_locs <- 
#   mutate(forage_locs, sex = ifelse(sex %in% c("F", "Female"), "Female", "Male"))

forage_locs <- filter(forage_locs, ID %in% c("224389", "224401", "224393")) %>% 
  mutate(sex = ifelse(sex == "F", "Female", sex)) %>% 
  rename(mu.y=Latitude, mu.x=Longitude)

endpnts <- filter(endpnts, ID != "224393")

endpnts <- bind_rows(endpnts, forage_locs)

## adjust 

endpnts <- st_as_sf(
  endpnts,
  coords = c("mu.x", "mu.y"), 
  crs = 4326, agr = "constant")

# endpnts <- st_as_sf(
#   forage_locs,
#   coords = c("Longitude", "Latitude"), 
#   crs = 4326, agr = "constant")

## map extent w/out showing ghana migration
pred_sf_noghana <- subset(pred_sf, ID != "224395") 
xtnt <- st_bbox(pred_sf_noghana)

## make scale bar only appear in one facet panel
scale_params <- tibble::tibble(
  sex = c("Male"),
  location = c("bl")
)

## map it
map <- ggplot() +
  geom_sf(data = land, inherit.aes = FALSE, fill="grey75", colour="grey40") +
  geom_sf(
    data=pred_line, 
    inherit.aes = FALSE, 
    aes(color=sex), size=1, 
    show.legend = FALSE) + 
  geom_sf(
    data=endpnts, 
    inherit.aes = FALSE, 
    aes(), pch=4, size=3, stroke=.75, 
    show.legend = FALSE) + 
  geom_sf(
    data=endpnts, 
    inherit.aes = FALSE, 
    aes(), size=1.6, fill="gold1", pch=21, stroke=.75,
    show.legend = FALSE) + 
  geom_sf(
    data = poilao, 
    inherit.aes = FALSE, 
    fill="gold1", 
    aes(), 
    size = 2.5, stroke=1, shape=23,
    show.legend = FALSE) +
  coord_sf(
    xlim = c(xtnt$xmin-.3, xtnt$xmax+1.1), 
    ylim = c(xtnt$ymin-.05, xtnt$ymax), 
    expand = T) +
  ggspatial::annotation_scale(
    data=scale_params, aes(location = location),
    style="ticks", height=unit(0.35, "cm"),
    line_width = 1.5, text_cex = 1.1) +
  theme_bw() + 
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  facet_wrap(~sex)

ggsave("C:/Users/Martim Bill/Desktop/test/11AAAA.png", plot=map, width=6, height = 7.5)

# if(B == FALSE){
#   ggsave("figures/migration_crawl_noB_sexfacet_n18X.png", plot=map, width=6,height = 7.5)
# } else {
#   ggsave("figures/migration_crawl_wB_sexfacet_n18X.png", plot=map, width=6,height = 7.5)
# }


### Make an inset map of the study area ---------------------------------------
africa <- subset(rnaturalearth::ne_countries(scale=110, returnclass = "sf"),
                 continent == "Africa")

africa <- subset(africa, name != "Madagascar")
xtnt2 <- st_bbox(pred_sf_noghana)

## widen bbox a bit

xtnt2[1] <- xtnt$xmin - .3
xtnt2[3] <- xtnt$xmax + 1.1
xtnt2[2] <- xtnt$ymin - .4
xtnt2[4] <- xtnt$ymax + 1.1

reg_bbox <- st_as_sfc(xtnt2)

inset <- ggplot() +
  geom_sf(data = africa, inherit.aes = FALSE, fill="grey30", colour="grey30") +
  geom_sf(data = reg_bbox, color="red", fill=NA, size=.75, inherit.aes = FALSE) +
  theme_void()


### Combine maps w/ distance plot 
library(patchwork)

## no speed 
# 
# layout <- 
#   "
#   AAAAA##
#   AAAAABB
#   AAAAABB
#   AAAAABB
#   AAAAABB
#   AAAAA##
#   "
# 
# # figs <- map + dst +
# #   plot_layout(design = layout)
# 
# figs <- map + 
#   inset_element(inset, -.1, -.02, .285, .2, align_to = 'full') +
#   dst2 +
#   plot_layout(design = layout)
# 
# 
# ggsave("figures/test_mig2.png", 
#        plot=figs, width=9, height = 6.5)

## inset in top left ------------------------------------------------------
layout <- 
  "
  BBBBB##
  BBBBBAA
  BBBBBAA
  BBBBBAA
  BBBBBAA
  BBBBB##
  "

figs <- dst2 +
  map + 
  inset_element(inset, .8, .775, 1.115, 1, align_to = 'full', clip = F) +
  plot_layout(design = layout)


ggsave("figures/test_mig5.png", 
       plot=figs, width=9, height = 6.5)

## coord_flipped histogram 
figs <- dst3 +
  map + 
  inset_element(inset, .8, .775, 1.115, 1, align_to = 'full', clip = F) +
  plot_layout(design = layout)

ggsave("figures/test_mig6.png", 
       plot=figs, width=9, height = 6.5)

## Combine w/ migration speed too ## ---------------------------
# layout <- 
#   "
#   AAAAABB
#   AAAAABB
#   AAAAABB
#   AAAAACC
#   AAAAACC
#   AAAAACC
#   "
# 
# figs <- map + 
#   inset_element(inset, -.04, .75, .25, .99, align_to = 'full') +
#   spd + dst +
#   plot_layout(design = layout)
#
# ggsave("figures/migration_crawl_noB_sexfacet_n18_spddstpatch_inset.png", 
#        plot=figs, width=9, height = 6.5)


