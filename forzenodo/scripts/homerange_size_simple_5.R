## Simply calculate home range size for 2021 individuals ----------------------

pacman::p_load(dplyr, tidyr, sf, ggplot2)

## include B locations?
B <- TRUE
# B <- FALSE

## Metadata -- sex info
meta <- read.csv("data/stage_dates.csv") 
meta$id <- as.character(meta$PTT)

## Males -- tracking data
if(B == FALSE){
  # mod_tracks  <- readRDS("data/analysis/stage_delim_4/prediction_single_noB_stages_4.rds")
  mod_tracks  <- readRDS("data/analysis/crawl_3/prediction_single_noB_nooutlie_nsd_gapfilter_breed_3.rds")
  
} else {
  # mod_tracks  <- readRDS("data/analysis/stage_delim_4/prediction_single_wB_stages_4.rds")
  mod_tracks  <- readRDS("data/analysis/crawl_3/prediction_single_wB_nooutlie_nsd_gapfilter_breed_3.rds")
  
}

# one_mod  <- subset(mod_tracks,  sex == "M")

## summarise tracking duration for males
trcksumm <- mod_tracks %>% 
  filter(state == "mate_nest") %>% 
  group_by(ID) %>% 
  summarise(
    n_pnts = n(),
    recording_duration_days = difftime(last(DateTime), first(DateTime), units = "day")
  ) %>% mutate()


## Males -- breeding UDs
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

## combine 95 and 50 UDs
UDs <- rbind(UD50_mo, UD95_mo)

UDs$sex <- ifelse(UDs$sex == "M", "Male", "Female")

UDs <- left_join(UDs, trcksumm, by=c("id"="ID"))


## Save ##
write.csv(UDs, "data/summaries/homerange_size_breed_2021.csv", row.names = F)


## plot it -------------------------------------------------------------------
ggplot() +
  geom_boxplot(data=UDs, aes(x=UDtype, y=area, fill=sex)) +
  geom_point(data=UDs, aes(x=UDtype, y=area, group=sex), 
             position=position_dodge(width=0.75)) +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Sex") 

ggsave("figures/breed_HRarea_h1_wB_n22.png", width = 5.5, height=5)

## removing extra exploratory individual
UDs_f <- subset(UDs, id != "213039")

ggplot() +
  geom_boxplot(data=UDs_f, aes(x=UDtype, y=area, fill=sex)) +
  geom_point(data=UDs_f, aes(x=UDtype, y=area, group=sex), 
             position=position_dodge(width=0.75)) +
  theme_bw() +
  ylab("Area (sq. km)") + xlab("Sex")

ggsave("figures/breed_HRarea_h1_wB_n21.png", width = 5.5, height=5)


## Test for sex effect while controlling for tracking effort ------------------

UD50 <- subset(UDs, UDtype == "UD50")
UD50_f <- subset(UD50, id != "213039")

UD95 <- subset(UDs, UDtype == "UD95")
UD95_f <- subset(UD95, id != "213039")

## 50% UD

## transform area to meet normality assumption
mod50c <- lm(log(area) ~ sex + recording_duration_days, data=UD50_f)
anova(mod50c)
summary(mod50c)

trnsfrm.model <- mod50c
qqnorm(resid(trnsfrm.model))
qqline(resid(trnsfrm.model))
hist(resid(trnsfrm.model))
shapiro.test(resid(trnsfrm.model))

## transform area to meet normality assumption
mod95c <- lm(log(area) ~ sex + recording_duration_days, data=UD95_f)
anova(mod95c)
summary(mod95c)

trnsfrm.model <- mod95c
qqnorm(resid(trnsfrm.model))
qqline(resid(trnsfrm.model))
hist(resid(trnsfrm.model))
shapiro.test(resid(trnsfrm.model))

