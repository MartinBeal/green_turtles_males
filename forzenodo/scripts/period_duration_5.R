### How long do turtles stay in breeding area? Difference btwn sexes? ---------

pacman::p_load(dplyr, lubridate, ggplot2)

# B <- TRUE
B <- FALSE

## fully filtered data
if(B == FALSE){
  tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_noB_4.rds")
} else {
  # tracks <- readRDS("data/analysis/stage_delim_4/mcconnell_stages_wB_4.rds")
  tracks <- readRDS("data/analysis/stage_delim_4/speedfilter_stages_wB_4.rds")
  }


tracks <- tracks %>% 
  mutate(
    sex = ifelse(sex == "F", "Female", "Male"),
    sex = factor(sex, levels = c("Female", "Male"))
  )

tracks %>% group_by(sex) %>% summarise(n_id=n_distinct(ID))

## mating/nesting -------------------------------------------------------------
breed <- subset(tracks, state == "mate_nest")

breednmig <- subset(breed, !ID %in% c("213021", "213037", "213044"))

brdsumm <- breednmig %>% group_by(ID, sex) %>% 
  summarise(
    first_pnt = first(DateTime),
    last_pnt  = last(DateTime),
    diff      = difftime(last_pnt, first_pnt, units = "day")
  )

brdsumm2 <- brdsumm %>% group_by(sex) %>% 
  summarise(
    n_id    = n_distinct(ID),
    mn_diff = mean(diff),
    sd_diff = sd(diff),
    md_diff = median(diff),
    min_diff = round(min(diff),1),
    max_diff = round(max(diff),1)
  )
brdsumm2

ggplot() +
  geom_boxplot(data=brdsumm, aes(x=sex, y=diff, fill=sex)) +
  geom_point(data=brdsumm, aes(x=sex, y=diff)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Duration in breeding area (days)") +
  xlab("Sex")

# ggsave("figures/mate_nest_duration.png", width = 4, height=5)


## Which individuals may have overlapped at poilão? ---------------------------
## add individuals in which left immediately after tagging

breed_ids <- unique(brdsumm$ID)
all_ids <- unique(tracks$ID)
nobrd_ids <- all_ids[which(!all_ids %in% breed_ids)]

nobrdrs <- subset(tracks, ID %in% nobrd_ids)

nobrdrs_summ <- nobrdrs %>% 
  group_by(ID, sex) %>% 
  summarise(
    first_pnt = first(DateTime),
    last_pnt  = first(DateTime),
    diff      = difftime(first_pnt, first_pnt, units = "day")
  )


##

breedtime <- round((12.2*6)) + 35 # inter-nest interval * nclutches + mating and oogenesis

times <- brdsumm %>% bind_rows(nobrdrs_summ) %>% 
  mutate(
    diff = round(diff, 1),
    # daysmissed = ifelse(sex == "Female", round(as.difftime(73, unit="days") - diff,1), NA),
    pot_arrival = if_else(sex=="Male", first_pnt, 
                          if_else(diff < breedtime, 
                                  last_pnt - round(as.difftime(breedtime, unit="days")), 
                                  first_pnt)
    ),
    overlap_time = if_else(sex == "Female", pot_arrival, last_pnt)
  )
View(times)

###

times_long <- tidyr::pivot_longer(
  times,
  cols = c(pot_arrival, first_pnt, last_pnt),
  names_to  = "timetype",
  values_to = "time"
)

times_long <- times_long[-which(times_long$diff == 0 & times_long$timetype == "last_pnt"), ]

times_long$timetype <- factor(times_long$timetype, 
                              levels = c("pot_arrival", "first_pnt", "last_pnt")
)

## Plot -----------------------------------------------------------------------

ggplot() +
  geom_line(data=times_long, aes(x=time, y=ID)) + 
  geom_point(data=times_long, aes(x=time, y=ID, color=timetype), 
             size=2) +
  # scale_linetype_manual(labels = c("Back-calc", "Track"), 
  #                       values=c("dashed", "solid") ) +
  scale_color_hue(
    labels = c( "Est. arrival", "Tag date", "End breeding")
  ) +
  scale_x_datetime(
    breaks = scales::date_breaks("1 month"),
    labels = scales::date_format("%b")) +
  facet_wrap(~sex, nrow=2, scales = "free_y",
             strip.position = c("right")) +
  theme_bw() +
  xlab("") +
  guides(
    linetype = guide_legend(title = NULL),
    color    = guide_legend(title = NULL)
  ) + 
  theme(
    legend.position = c(0.86, 0.125),
    legend.background = element_rect(color = "grey30"),
    strip.text=element_text(hjust=.08))

ggsave("figures/breed_estimated_overlap_108d_n22.png", width = 7, height=6)


### ---------------------------------------------------------------------------
## Foraging -------------------------------------------------------------
forage <- subset(tracks, state == "foraging")

tsumm <- forage %>% group_by(ID, sex) %>% 
  summarise(
    n_trkday  = n_distinct(yday(DateTime)),
    first_pnt = first(DateTime),
    last_pnt  = last(DateTime),
    diff      = difftime(last_pnt, first_pnt, units = "day")
  )

tsumm2 <- tsumm %>% group_by(sex) %>% 
  summarise(
    n_id    = n_distinct(ID),
    min_diff = min(diff),
    max_diff = max(diff),
    mn_diff = mean(diff),
    md_diff = median(diff),
    mn_tday = mean(n_trkday),
    min_tdayf = min(n_trkday),
    max_tday = max(n_trkday)
  )
tsumm2

ggplot() +
  geom_boxplot(data=tsumm, aes(x=sex, y=diff, fill=sex)) +
  geom_point(data=tsumm, aes(x=sex, y=diff)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Tracking days in foraging area") +
  xlab("Sex")

ggsave("figures/forage_duration.png", width = 4, height=5)


## Migration -------------------------------------------------------------

mig <- subset(tracks, state == "migration")

tsumm <- mig %>% group_by(ID, sex) %>% 
  summarise(
    first_pnt = first(DateTime),
    last_pnt  = last(DateTime),
    diff      = difftime(last_pnt, first_pnt, units = "day")
  )

## filter out individuals w/ incomplete migrations
tsumm <- subset(tsumm, !ID %in% c("224398", "224401"))

tsumm2 <- tsumm %>% group_by(sex) %>% 
  summarise(
    n_id    = n_distinct(ID),
    mn_diff = mean(diff),
    sd_diff = sd(diff),
    md_diff = median(diff),
    min_diff = round(min(diff),1),
    max_diff = round(max(diff),1)
  )
tsumm2

ggplot() +
  geom_boxplot(data=tsumm, aes(x=sex, y=diff, fill=sex)) +
  geom_point(data=tsumm, aes(x=sex, y=diff)) +
  theme_bw() +
  theme(legend.position = "none") +
  ylab("Duration of migration (days)") +
  xlab("Sex")

ggsave("figures/mig_duration_sex.png", width = 4, height=5)

## check out an individual migration

one_sf <- subset(mig, ID == "224398") %>% 
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, agr = "constant")

mapview(one_sf)

