### Compare size of home ranges for each sex and datatype ---------------------
## ***Using basic KDE

pacman::p_load(ggplot2, tidyr, dplyr)

## include B locations?
B <- TRUE
# B <- FALSE

## UD based on filtered data
# UD_fi_fe  <- readRDS("data/analysis/KDE_4/filter/CDF/a_groupCDFs_h0.5_c0.25_mate_nest_females.rds")
# UD_fi_ma  <- readRDS("data/analysis/KDE_4/filter/CDF/a_groupCDFs_h0.5_c0.25_mate_nest_males.rds")

## UD based on modelled data
# UD_mo_fe  <- readRDS("data/analysis/KDE_4/model/CDF/a_groupCDFs_h0.5_c0.25_mate_nest_females.rds")
# UD_mo_ma  <- readRDS("data/analysis/KDE_4/model/CDF/a_groupCDFs_h0.5_c0.25_mate_nest_males.rds")
## UD based on modelled data -- only breeding period
UD_mo_fe  <- readRDS("data/analysis/KDE_4/wargos/CDF/a_groupCDFs_h1_mate_nest_females.rds")
UD_mo_ma  <- readRDS("data/analysis/KDE_4/wargos/CDF/a_groupCDFs_h1_mate_nest_males.rds")


# model_tracks <- readRDS("data/analysis/stage_delim_4/prediction_single_noB_stages_4.rds")

mapview(UD_mo_fe[[1]]) + mapview(UD_mo_ma[[1]], col.regions="red")
mapview(UD_mo_fe[[2]]) + mapview(UD_mo_ma[[2]], col.regions="red")

mapview(UDs_model[[2]]) + mapview(TD)


### Size of individual core areas and home ranges ------------------------------

meta <- read.csv("data/stage_dates.csv") 
meta$id <- as.character(meta$PTT)

## UD based on modelled data -- only breeding period --------------------------
if(B == FALSE){
  UD50_mo_pi <- readRDS(
    "data/analysis/KDE_4/wargos/ind_areas/individual_UD50_noB_h1_mate_nest.rds")
  UD95_mo_pi <- readRDS(
    "data/analysis/KDE_4/wargos/ind_areas/individual_UD95_noB_h1_mate_nest.rds")
} else {
  UD50_mo_pi <- readRDS(
    "data/analysis/KDE_4/wargos/ind_areas/individual_UD50_wB_h1_mate_nest.rds")
  UD95_mo_pi <- readRDS(
    "data/analysis/KDE_4/wargos/ind_areas/individual_UD95_wB_h1_mate_nest.rds")
}

UD50_mo <- UD50_mo_pi$UDPolygons %>% st_drop_geometry() %>% 
  mutate(UDtype  = "UD50", datatype="model")
UD95_mo <- UD95_mo_pi$UDPolygons %>% st_drop_geometry() %>% 
  mutate(UDtype  = "UD95", datatype="model")

UD50_mo <- left_join(UD50_mo, meta[,c("id", "sex")], by="id")
UD95_mo <- left_join(UD95_mo, meta[,c("id", "sex")], by="id")

## combine
UDs <- rbind(UD50_mo, UD95_mo)

ind_areas <- rbind(UD50_mo, UD95_mo)

## get summary stats
mod_summ <- ind_areas %>%
  group_by(UDtype, sex) %>% 
  summarise(
    mn_area = mean(area),
    sd_area = sd(area),
    median_area = median(area),
    max_area = max(area), min_area = min(area)
  )

## plot it
ggplot() +
  geom_boxplot(data=UDs, aes(x=UDtype, y=area, fill=sex)) +
  geom_point(data=ind_areas, aes(x=UDtype, y=area, group=sex), 
             position=position_dodge(width=0.75)) +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Sex") 

ggsave("figures/mate_nest_wargos_ind_HR_h1.png", width = 5.5, height=5)



## UD based on filtered data --------------------------------------------------
UD50_fi_po <- readRDS("data/analysis/KDE_4/filter/ind_areas/individual_UD50_h0.8_mate_nest.rds")
UD95_fi_po <- readRDS("data/analysis/KDE_4/filter/ind_areas/individual_UD95_h0.8_mate_nest.rds")

UD50_fi <- UD50_fi_po$UDPolygons %>% st_drop_geometry() %>% 
  mutate(UDtype  = "UD50", datatype="filter")
UD95_fi <- UD95_fi_po$UDPolygons %>% st_drop_geometry() %>% 
  mutate(UDtype  = "UD95", datatype="filter")

UD50_fi <- left_join(UD50_fi, meta[,c("id", "sex")], by="id")
UD95_fi <- left_join(UD95_fi, meta[,c("id", "sex")], by="id")

filt_summ50 <- UD50_fi %>% 
  group_by(sex) %>% 
  summarise(
    mn_area = mean(area),
    sd_area = sd(area),
    median_area = median(area),
    max_area = max(area), min_area = min(area)
  )
filt_summ95 <- UD95_fi %>%
  group_by(sex) %>% 
  summarise(
    mn_area = mean(area),
    sd_area = sd(area),
    median_area = median(area),
    max_area = max(area), min_area = min(area)
  )

filt_summ <- rbind(filt_summ50, filt_summ95)

## Combine datatypes to compare -------------------------------------------
UDs <- rbind(UD50_fi, UD95_fi, UD50_mo, UD95_mo)

ggplot() +
  geom_boxplot(data=UDs, aes(x=UDtype, y=area, fill=sex)) +
  facet_wrap(~datatype) + 
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Sex") 

ggsave("figures/mate_nest_ind_HR_h5.png", width = 5.5, height=5)


## Is ind. home range size explained by # of tracking locations? --------------

# filt_tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds") # n B pnts
mod_tracks  <- readRDS("data/analysis/stage_delim_4/prediction_single_noB_stages_4.rds")

# filt_tracks <- subset(filt_tracks, ID == "224401")
# mod_tracks  <- subset(mod_tracks,  ID == "224401")
# 
# one_filt <- st_as_sf(filt_tracks, coords = c("Longitude", "Latitude"), 
#                  crs = 4326)
one_mod <- st_as_sf(one_mod, coords = c("mu.x", "mu.y"), 
                     crs = 4326)

mapview(subset(one_filt, state=="mate_nest")) +
  mapview(subset(one_mod, state=="mate_nest"), col.regions="red") 


##

## Filtered data
# tracks <- filt_tracks
# UDs <- rbind(UD50_fi, UD95_fi)
## Modelled data 
tracks <- mod_tracks
UDs <- rbind(UD50_mo, UD95_mo)

trcksumm <- tracks %>% 
  filter(state == "mate_nest") %>% 
  group_by(ID) %>% 
  summarise(
    n_pnts = n(),
    day1   = first((DateTime)),
    dayX   = last((DateTime)),
    recording_duration_days = difftime(dayX, day1, units="days")#,
    # n_trkday = n_distinct(yday(DateTime))
  ) %>% dplyr::select(-day1, -dayX)

trcksumm <- trcksumm %>% left_join(UDs, by=c("ID"="id")) %>% 
  filter(!is.na(UDtype)) %>% 
  mutate(
    sex = ifelse(sex == "F", "Female", "Male"),
    sex = factor(sex, levels = c("Female", "Male"))
  )

ggplot() +
  geom_point(data=trcksumm, aes(x=n_pnts, y=area, color=sex)) +
  facet_wrap(~UDtype, scales = "free") +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Tracking locations") 

ggplot() +
  geom_point(data=trcksumm, aes(x=n_trkday, y=area, color=sex)) +
  facet_wrap(~UDtype, scales = "free") +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Tracking days") 


## Fit mixed-effect model to test whether sex difference is significant while 
# controlling for tracking effort

trcksumm50 <- subset(trcksumm, UDtype == "UD50")
trcksumm95 <- subset(trcksumm, UDtype == "UD95")

## 50% UD ---------------------------------------------------------------------
mod1 <- lm(area ~ sex * n_trkday, data=trcksumm50)
anova(mod1)
summary(mod1)

## normality - extreme values deviate 
notrnsfrm.model <- mod1
qqnorm(resid(notrnsfrm.model))
qqline(resid(notrnsfrm.model))
hist(resid(notrnsfrm.model))
shapiro.test(resid(notrnsfrm.model))

## transform area to meet normality assumption
mod1b <- lm(log(area) ~ sex * n_trkday, data=trcksumm50)
anova(mod1b)
summary(mod1b)

trnsfrm.model <- mod1b
qqnorm(resid(trnsfrm.model))
qqline(resid(trnsfrm.model))
hist(resid(trnsfrm.model))
shapiro.test(resid(trnsfrm.model))

## 50% UD ---------------------------------------------------------------------
mod2 <- lm(area ~ sex * n_trkday, data=trcksumm95)
anova(mod2)
summary(mod2)

## normality - extreme values deviate 
notrnsfrm.model <- mod2
qqnorm(resid(notrnsfrm.model))
qqline(resid(notrnsfrm.model))
hist(resid(notrnsfrm.model))
shapiro.test(resid(notrnsfrm.model))

## 1. Plot result #### --------------------------------------------------------
library(ggeffects)
# library(viridis)

# Extract the prediction data frame
pred.m1 <- ggpredict(mod1b, terms = c("n_trkday", "sex"))  # this gives overall predictions for the model
pred.m2 <- ggpredict(mod2, terms = c("n_trkday", "sex"))  # this gives overall predictions for the model

ggplot() + 
  geom_jitter(                     # adding the raw data (scaled values)
    aes(x = n_trkday, y = area, colour = sex), height = 0.1,
    data = trcksumm50) +
  # scale_color_viridis(discrete = T, option = "E") +
  geom_line(aes(x = x, y = predicted, group=group), 
            size=1, data=pred.m1) +          # slope
  geom_ribbon(aes(x = x, 
                  ymin = (conf.low), 
                  ymax = (conf.high),
                  color = group), fill=NA, #linetype="dashed",
              data=pred.m1) +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Tracking days") 

## To showcase sex effects ---------------------------------------------------
pred.m1b <- ggpredict(mod1b, terms = c("sex"))  # this gives overall predictions for the model
pred.m2b <- ggpredict(mod2, terms = c("sex"))  # this gives overall predictions for the model

## 50% UD ---------------------------------------------------------------------
ggplot() + 
  geom_jitter(                     # adding the raw data (scaled values)
    aes(x = sex, y = area), size=.75, width=.05,
    data = trcksumm50) +
  geom_errorbar(data=pred.m1b, 
                aes(x=x, y=predicted, ymin = conf.low, ymax = conf.high,
                    color=x), width=.1,
                position = position_dodge(.3)) +
  geom_point(data=pred.m1b, 
             aes(x, predicted, color=x), 
             position = position_dodge(.3)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Area (sq. km)") + xlab("Sex") 

## 95% UD ---------------------------------------------------------------------
ggplot() + 
  geom_jitter(                     # adding the raw data (scaled values)
    aes(x = sex, y = area), size=.75, width=.05,
    data = trcksumm95) +
  geom_errorbar(data=pred.m2b, 
                aes(x=x, y=predicted, ymin = conf.low, ymax = conf.high,
                    color=x), width=.1,
                position = position_dodge(.3)) +
  geom_point(data=pred.m2b, 
             aes(x, predicted, color=x), 
             position = position_dodge(.3)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Area (sq. km)") + xlab("Sex")


###---------------------------------------------------------------------------
## Comparing males (2021) to all 44 females (2018-2020, from Raposo manu) ----

load("Raposo et al_internesting manus/data/tracks_variables.RData")

females <- tracks_variables

UDs <- rename(UDs, ID = id)
UDs$sex <- as.factor(ifelse(UDs$sex == "F", "Female", "Male"))


UDs <- UDs %>% left_join(
  trcksumm, 
  by=c("UDtype", "sex", "area", "ID", "datatype"))

UD_males <- subset(UDs, sex == "Male")
UD_f_2021 <- subset(UDs, sex == "Female")

all <- females %>% rename(ID = PTT, n_pnts = nb_locs) %>% 
  dplyr::select(ID, kud50_km2, kud95_km2, n_pnts, recording_duration_days) %>% 
  pivot_longer(
    cols = c("kud50_km2", "kud95_km2"), 
    names_to = "UDtype", 
    values_to = "area"
    ) %>% 
  mutate(
    sex = "Female",
    UDtype = ifelse(UDtype == "kud50_km2", "UD50", "UD95"),
    datatype = "model"
  )

all <- rbind(all, UD_males)

ggplot() +
  geom_boxplot(data=all, aes(x=UDtype, y=area, fill=sex)) +
  geom_point(data=all, aes(x=UDtype, y=area, group=sex), 
             position=position_dodge(width=0.75)) +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Sex")

## filter out two females that make biig loop trips
all_f <- all %>% filter(!ID %in% c("60897", "60891"))

ggplot() +
  geom_boxplot(data=all_f, aes(x=UDtype, y=area, fill=sex)) +
  geom_point(data=all_f, aes(x=UDtype, y=area, group=sex), 
             position=position_dodge(width=0.75)) +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Sex")
