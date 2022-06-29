## Compare distances of foraging grounds for males and females ##

pacman::p_load(ggplot2, dplyr, lubridate)

## Migration data
## McConnell filtered data
tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_noB_noland5km_migrate_4.rds")

## use regular speed filtered data to include 224401's foraging data
# tracks <- readRDS("data/analysis/stage_delim_4/speedfilter_stages_wB_4.rds")

## stage dates metadata -------------------------------------------------------
meta <- read.csv("data/stage_dates.csv") 
meta$PTT <- as.character(meta$PTT)

foragers <- subset(
  meta, 
  PTT %in% meta$PTT[which(meta$foraging_ground_location != "unknown")]
)

## tracking data (either filtered or crawl prediction) -----------------------

trips <- subset(tracks, ID %in% foragers$PTT)

# trips <- left_join(trips,
#                       meta[,c("PTT", "deploy_date", "end_nesting", "CCL_cm")],
#                       by=c("ID"="PTT"))

trips$ID <- as.factor(trips$ID)


# -----------------------------------------------------------------------------
### As not possible to know end date and path of migration for some IDs, 
## append centroid of foraging period as migration 'end point'
# apprxs. min distance travelled during mig.

### Foraging location of each individual
# forage <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
forage <- readRDS("data/analysis/stage_delim_4/speedfilter_stages_wB_4.rds")

forage <- subset(forage, state == "foraging")

forage_locs <- forage %>% 
  group_by(ID, sex) %>% 
  summarise(
    Latitude  = mean(Latitude),
    Longitude = mean(Longitude),
    DateTime  = first(DateTime),
    # end_nesting = first(end_nesting),
    nsd = first(nsd),
    state = first(state)
  )

# saveRDS(forage_locs, "data/analysis/forage_locs_n20.rds")

# st_as_sf(forage_locs, coords = c("Longitude", "Latitude"),
         # crs = 4326, agr = "constant") %>% mapview()


### Add forage locs to end of each individual's track

# trips <- subset(tracks, state == "migration")
trips <- select(trips,
                Longitude, Latitude, DateTime, ID, sex, nsd, state)

trips <- bind_rows(trips, forage_locs) %>% arrange(ID, DateTime)

# x <- filter(trips, ID == "224401")
# st_as_sf(x, coords = c("Longitude", "Latitude"), 
#          crs = 4326, agr = "constant") %>% mapview()
# 
# st_as_sf(trips, coords = c("Longitude", "Latitude"), 
#          crs = 4326, agr = "constant") %>% mapview(zcol="state")



### (1) Calculate great circle distance between Poilao and foraging destination
for_dist <- trips %>% as_tibble() %>% 
  filter(state == "foraging") %>% 
  group_by(ID, sex) %>% 
  summarise(
    gc_dist_km = mean(sqrt(nsd))/1000
  )

### (2) Calculate total distance travelled
poss_dist <- purrr::possibly(geosphere::distm, otherwise = NA)

trip_distances <- trips %>%
  tidyr::nest(coords = c(Longitude, Latitude)) %>%
  group_by(ID, sex) %>%
  mutate(prev_coords = dplyr::lag(coords)) %>%
  ungroup() %>%
  mutate(Dist = purrr::map2_dbl(
    coords, prev_coords, poss_dist)
  ) %>%
  mutate(Dist = if_else(is.na(Dist), sqrt(nsd), Dist)) %>%
  group_by(ID, sex) %>%
  dplyr::summarise(
    n_locs = n(),
    forage_arrival = as.Date(max(DateTime)),
    duration_h = as.numeric(
      difftime(max(DateTime),
               min(DateTime),
               units = "hours")
    ),
    total_dist_km = sum(.data$Dist, na.rm = TRUE) / 1000
  )


### Join w/ summary of great circle distance 
dists <- for_dist %>% left_join(trip_distances, by=c("ID", "sex"))

dists <- left_join(dists,
                   meta[,c("PTT", "deploy_date", "end_nesting", "CCL_cm")],
                   by=c("ID"="PTT"))

dists <- dists %>% mutate(
  deploy_date = as.Date(parse_date_time(deploy_date, orders = c("dmY HM"))),
  deploy_yr   = year(deploy_date),
  deploy_doy  = yday(deploy_date),
  depart_date = as.Date(parse_date_time(end_nesting, orders = c("dmY HM"))),
  depart_doy  = yday(depart_date),
  depart_mon  = month(end_nesting) 
) %>% dplyr::select(sex, CCL_cm, n_locs, deploy_yr, deploy_date, deploy_doy, 
                    depart_date, depart_doy, forage_arrival, duration_h, 
                    total_dist_km, gc_dist_km)


plot(dists$gc_dist_km, dists$total_dist_km)

dists <- dists %>% group_by(ID, sex) %>% 
  mutate(
    forage_arrival = ifelse(ID %in% c("224389", "224401"), NA, forage_arrival),
    total_dist_km  = ifelse(ID %in% c("224389", "224401"), NA, total_dist_km),
    duration_h     = ifelse(ID %in% c("224389", "224401"), NA, duration_h)
  )
plot(dists$gc_dist_km, dists$total_dist_km)

# View(dists)

gensumm <- dists %>% group_by(sex) %>% 
  summarise(
    n_id = n_distinct(ID),
    mn_mn_gcdist = mean(gc_dist_km),
    md_mn_gcdist = median(gc_dist_km),
    sd_mn_gcdist = sd(gc_dist_km),
    max_gcdist   = max(gc_dist_km),
    min_gcdist   = min(gc_dist_km),
    mn_mn_tdist = mean(na.omit(total_dist_km)),
    md_mn_tdist = median(na.omit(total_dist_km)),
    sd_mn_tdist = sd(na.omit(total_dist_km)),
    max_tdist   = max(total_dist_km),
    min_tdist   = min(total_dist_km)
  )

## SAVE -----------------------------------------------------------------------
write.csv(dists, row.names = F, "data/summaries/migration_dist_id.csv")


#### Test difference in distances between males and females -------------------

## Great circle distance --- ## run Mann-Whitney test -- w/ all 2021 data------
wilcox.test(gc_dist_km ~ sex, data=dists) 

## run Mann-Whitney test -- excluding individual travelling to Ghana ----------
dists_f <- subset(dists, ID != "224395")

wilcox.test(gc_dist_km ~ sex, data=dists_f) 

## Great circle distance --- ## run Mann-Whitney test -- w/ all 2021 data------
wilcox.test(total_dist_km ~ sex, data=dists) 

## plot ------------------------------------------------

dists$sex <- ifelse(dists$sex == "M", "Male", "Female")

dst <- ggplot() +
  geom_boxplot(data=dists, aes(x=sex, y=gc_dist_km, fill=sex), width=.5) +
  geom_point(data=dists, aes(x=sex, y=gc_dist_km, fill=sex), alpha=.5) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Great Circle distance to foraging area (km)") + xlab("")
dst

# ggsave("figures/dist2foraging_sex_n25.png", width = 4, height=5)

## show as histogram
dst2 <- ggplot() +
  geom_histogram(data=dists, aes(x=gc_dist_km, fill=sex), bins=30) +
  theme_bw() +
  ylab("N individuals") + xlab("Distance to foraging area (km)") +
  scale_y_continuous(
    limits=c(0,9), 
    breaks=scales:::pretty_breaks(),
    expand = c(0,0)) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom") 
dst2

dst3 <- dst2 +
  geom_text(data=data.frame( ## add sample size
    label=paste("N =", n_distinct(dists$ID)),
    x=1800, y=7.5),
    mapping = aes(x=x, y=y, label=label)) + 
  coord_flip() 

dst2 <- dst2 +
  geom_text(data=data.frame( ## add sample size
    label=paste("N =", n_distinct(dists$ID)),
    x=200, y=8.5),
    mapping = aes(x=x, y=y, label=label))

## Distance against body size (2021)
ggplot() +
  geom_point(data=dists, aes(x=CCL_cm, y=gc_dist_km, color=sex)) +
  theme_bw() +
  # facet_wrap(~sex) +
  theme(legend.position = "none") +
  ylab("Distance to foraging area (km)") +
  xlab("Curved carapace length (cm)")

# ggsave("figures/dist2foraging_CCL_sex_n25.png", width = 6, height=5)


## ----------------------------------------------------------------------------
## Add distances, deploy dates + CCL for 2018-20 females ----------------------
old_summ <- read.csv("data/foraging_distance_females_2018_2020.csv")
old_summ <- mutate(old_summ, ID = as.character(ID), sex=(as.character(sex)))
old_summ$sex <- plyr::revalue(old_summ$sex, c("FALSE" = "F"))

old_summ <- dplyr::rename(old_summ, CCL_cm=CCL..cm., gc_dist_km = mn_displace)

for_dist_all <- rbind(dists, old_summ)

for_dist_all$sex <- ifelse(for_dist_all$sex == "F", "Female", for_dist_all$sex)

## compare distance of sexes (w/ all individuals)
ggplot() +
  geom_boxplot(data=for_dist_all, aes(x=sex, y=gc_dist_km, fill=sex)) +
  geom_point(data=for_dist_all, aes(x=sex, y=gc_dist_km, fill=sex)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Distance to foraging area (km)")


## Foraging distance against body size for ALL individuals 
ggplot() +
  geom_point(data=for_dist_all, aes(x=CCL_cm, y=gc_dist_km, color=sex)) +
  theme_bw() +
  # facet_wrap(~sex) +
  theme(legend.position = "none") +
  ylab("Distance to foraging area (km)") +
  xlab("Curved carapace length (cm)")

# ggsave("figures/dist2foraging_CCL_sex_n53.png", width = 6, height=5)


### Distance against deployment date -- are there seasonal shifts in destination?

## convert plot labels to calendar dates for display
xticks <- c(180,220,260,300)

Dates <- as.Date(xticks, origin=as.Date("1960-01-01"))
date_lbls <- paste(month.abb[month(Dates)], format(Dates ,"%d"))

ggplot() +
  geom_point(data=for_dist_all, aes(x=deploy_doy, y=gc_dist_km, color=sex)) +
  scale_x_continuous(
    breaks=xticks, labels=date_lbls) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Distance to foraging area (km)") + xlab("Deployment date")

# ggsave("figures/dist2foraging_deploydate_sex_n53.png", width = 6, height=5)

## Split by year 
ggplot() +
  geom_point(data=for_dist_all, aes(x=deploy_doy, y=gc_dist_km, color=sex)) +
  scale_x_continuous(
    breaks=xticks, labels=date_lbls) +
  theme_bw() +
  facet_wrap(~deploy_yr) +
  theme(legend.position = "none") +
  ylab("Distance to foraging area (km)") + xlab("Deployment date")

# ggsave("figures/dist2foraging_deploydate_yr_sex_n53.png", width = 6, height=5)


## Migration departure date ---------------------------------------------------
# xticks <- c(0,50,100,150)
xticks <- c(11,42,72,103,133,164,195)
Dates <- as.Date(abs(xticks+201), origin=as.Date("1960-01-01"))
date_lbls <- paste(month.abb[month(Dates)], format(Dates ,"%d"))

## center dates from earliest in the season (July instead of January)
for_dist_all <- for_dist_all %>% mutate(
  depart_doy_cntr = ifelse(
    depart_doy < 201, depart_doy + 365, depart_doy
  ))
# for_dist_all$depart_doy_cntr <- abs(for_dist_all$depart_doy - 201)

ggplot() +
  geom_point(data=for_dist_all, aes(x=depart_doy_cntr, y=gc_dist_km, color=sex)) +
  scale_x_continuous(
    breaks=xticks+201, labels=date_lbls) +
  theme_bw() +
  ylab("Distance to foraging area (km)") + xlab("Departure date")

# ggsave("figures/dist2foraging_departdate_sex_n53.png", width = 6, height=5)

## split by year 
ggplot() +
  geom_point(data=for_dist_all, aes(x=depart_doy_cntr, y=gc_dist_km, color=sex)) +
  scale_x_continuous(
    breaks=xticks+201, labels=date_lbls) +
  theme_bw() +
  facet_wrap(~deploy_yr) +
  ylab("Distance to foraging area (km)") + xlab("Departure date")

# ggsave("figures/dist2foraging_departdate_yr_sex_n53.png", width = 8, height=6)


## Just females ---------------------------------------------------------------

for_dist_fem <- subset(for_dist_all, sex == "Female")

## center dates from earliest in the season (July instead of January)
for_dist_fem <- for_dist_fem %>% mutate(
  depart_doy_cntr = ifelse(
    depart_doy < 247, depart_doy + 365, depart_doy
  ))

# xticks <- c(0,50,100,150)
xticks <- c(11,42,72,103,133,164,195)
Dates <- as.Date(abs(xticks+247), origin=as.Date("1960-01-01"))
date_lbls <- paste(month.abb[month(Dates)], format(Dates ,"%d"))

## split by year 
ggplot() +
  geom_point(data=for_dist_fem, 
             aes(x=depart_doy_cntr, y=gc_dist_km, color=sex)) +
  scale_x_continuous(
    breaks=xticks+201, labels=date_lbls) +
  theme_bw() +
  facet_wrap(~deploy_yr) +
  ylab("Distance to foraging area (km)") + xlab("Departure date")

# ggsave("figures/dist2foraging_departdate_yr_females_n45.png", width = 8, height=6)


## ----------------------------------------------------------------------------
## Model departure date effect for females ------------------------------------
## ----------------------------------------------------------------------------

for_dist_all$destination <- ifelse(
  for_dist_all$gc_dist_km < 100, "Guinea-Bissau/Guinea", 
  ifelse(
    for_dist_all$gc_dist_km > 100 & for_dist_all$gc_dist_km < 500,
    "Senegal/Gambia", ifelse(
      for_dist_all$gc_dist_km > 500 & for_dist_all$gc_dist_km < 1500,
      "Mauritania", "Ghana")
  )
)

## Just females
for_dist_fem <- subset(for_dist_all, sex == "Female")

## remove one individual w/ no known departure date
for_dist_fem <- for_dist_fem[-which(is.na(for_dist_fem$depart_doy)),]

## center dates from earliest in the season (July instead of January)
for_dist_fem <- for_dist_fem %>% mutate(
  depart_doy_cntr = ifelse(
    depart_doy < 247, depart_doy + 365, depart_doy
  ))


### Group destinations as short and long to increase counts ------------------

for_dist_fem$destination <- ifelse(
  for_dist_fem$destination == "Mauritania" | for_dist_fem$destination == "Ghana",
  "long", "short")

for_dist_fem$dest_tf <- ifelse(
  for_dist_fem$destination == "long", 1, 0)

ggplot() + 
  geom_point(data=for_dist_fem, aes(x=depart_doy_cntr, y=dest_tf)) +
  facet_wrap(~deploy_yr)


## just the two years w/ turtles going both short and long --------------------

for_dist_18.20 <- subset(for_dist_fem, deploy_yr %in% c(2018, 2020))

for_dist_18.20$deploy_yr <- factor(for_dist_18.20$deploy_yr)

## fit logistic regression ----------------------------------------------------
# Does probability of migrating far (Maur) increase w/ departure date?
mylogit <- glm(dest_tf ~ depart_doy + deploy_yr, 
               data = for_dist_18.20, family = "binomial")

modsumm <- summary(mylogit)
modsumm
confint.default(mylogit)

for_dist_18.20$predict <- predict(mylogit, type="response")
for_dist_18.20$dest_tf <- ifelse(for_dist_18.20$dest_tf == T, 1, 0)

## prettify date ticks
xticks <- c(58,88,119)
Dates <- as.Date(abs(xticks+247), origin=as.Date("1960-01-01"))
date_lbls <- paste0(month.abb[month(Dates)], 
                    DescTools::Format(Dates, fmt = "d"), 
                    "st")

ggplot() +
  geom_point(data=for_dist_18.20, aes(x=depart_doy, y=dest_tf)) +
  geom_smooth(data=for_dist_18.20, aes(x=depart_doy, y=predict),
              method = "glm", method.args = list(family = "binomial"), 
              linetype=0, se = T) +
  scale_x_continuous(
    breaks=xticks+201, labels=date_lbls) +
  facet_wrap(~deploy_yr) +
  theme_bw() + 
  xlab("Departure date") + ylab("Probability to migrate far")


# ggsave("figures/dist2foraging_logistic_18.20_females_n26_noline.png", width = 8, height=4)