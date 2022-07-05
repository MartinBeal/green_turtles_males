### Preparing data for Movebank

## Initial cleaning steps - Male turtle distributions ## ----------------------

pacman::p_load(trip, dplyr, raster, ggplot2, lubridate, amt)
source("C:/Users/Martim Bill/Documents/R/source_scripts/pt2pt_fxns.WV.R")


## raw tracking data (ALL TRACKED TURTLES)
all <- read.csv("data/raw_tracks/all_turtles_12MAY22.csv")

## turtle deployment metadata
meta <- readxl::read_xlsx("data/metadata_tracked_turtles_v3.xlsx")

## remove rows w/ NA coordinates
all <- filter(all, Latitude != 0 | !is.na(Latitude))

## format data
tracks <- track2KBA::formatFields(
  all, 
  fieldID = "Tag_ID", 
  fieldLat="Latitude", fieldLon="Longitude", 
  fieldDate="UTC_Date", fieldTime="UTC_Time", formatDT = "ymd HMS"
)

## get rid of "6524" from IDs
tracks$ID <- do.call(
  rbind,
  stringr::str_split(tracks$ID, pattern = stringr::fixed(":"))
)[,2]

## keep just 2021 individauls (removing 2 males caught in Maur.)
tracks <- subset(
  tracks, 
  ID %in% c("213020", "213021", "213037", "213038", 
            "213039", "213040", "213041", "213042", "213043", "213044", 
            "213045", "213046", "224389", "224390", "224391", "224392", 
            "224393", "224394", "224395", "224396", "224397", "224398", 
            "224399", "224400", "224401")
)

## remove data from before deployment data for each individual
meta <- meta %>% mutate(PTT = as.character(PTT)) %>%
  filter(PTT %in% unique(tracks$ID))

tracks <- right_join(tracks, 
                     meta[, c("PTT", "deploy_date", "sex")], by = c("ID" = "PTT"))

tracks <- tracks %>% 
  filter(DateTime > deploy_date)

## recombine w/ non-spatial data
# tracks <- data.frame(tracksSP) %>% dplyr::select(-tracks.Longitude, -tracks.Latitude)
tracks <- tracks[order(tracks$ID, tracks$DateTime), ] ## order date time stamps within each

tracks <- tracks %>% 
  dplyr::select(
    "Latitude", "Longitude", "ID", "sex", "DateTime", "Location.Quality", "eRes", 
    "HDOP", "SatNum") %>% 
  mutate(
    Location.Quality = ifelse(Location.Quality == "", "G", Location.Quality)
  )

tracks$sensortype <- ifelse(tracks$Location.Quality == "G", "GPS", "ARGOS")

## Movebank (annoyingly) doesnt allow NAs in numeric columns; convert to 0
tracks$HDOP <- ifelse(is.na(tracks$HDOP), 0, tracks$HDOP)
tracks$SatNum <- ifelse(is.na(tracks$SatNum), 0, tracks$SatNum)
tracks$eRes <- ifelse(is.na(tracks$eRes), 0, tracks$eRes)


## split into male and female datasets

males <- tracks %>% filter(sex == "M")
females <- tracks %>% filter(sex == "F")



## Save 
write.csv(males, "data/movebank/males.csv", row.names = F)
write.csv(females, "data/movebank/females.csv", row.names = F)
