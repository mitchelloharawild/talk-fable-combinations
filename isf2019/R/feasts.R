library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(tsibbledata)

vic_print_media <- tsibbledata::aus_retail %>%
  filter(Industry == "Newspaper and book retailing", State == "Victoria")
aus_print_media <- tsibbledata::aus_retail %>%
  filter(Industry == "Newspaper and book retailing")

ped_daily <- pedestrian %>% 
  index_by(Date) %>% 
  summarise(Count = sum(Count))

# Graphics

vic_print_media %>% 
  autoplot(Turnover)

aus_print_media %>% 
  autoplot(Turnover) + 
  facet_grid(Industry ~ State, scales = "free_y") + 
  guides(colour = FALSE)

# aus_print_media %>% 
#   model(SNAIVE(Turnover), NAIVE(Turnover)) %>% 
#   forecast() %>% 
#   autoplot(aus_print_media)

ped_daily %>% 
  autoplot(Count)

vic_print_media %>% 
  gg_season(Turnover)

ped_daily %>% 
  gg_season(Count, period = "week")

aus_print_media %>% 
  gg_season(Turnover)

vic_print_media %>% 
  gg_subseries(Turnover)

aus_print_media %>% 
  gg_subseries(Turnover)

ped_daily %>% 
  gg_subseries(Count, period = "week")

# Features
aus_print_media %>% 
  features(Turnover, features = list(guerrero, stl_features, compengine))

library(feasts.tsfresh)
aus_print_media %>% 
  features(Turnover, features = list(tsfresh_features))


aus_print_media %>% 
  features(Turnover, features = list(unitroot_kpss, unitroot_pp))

# Decomposition
aus_print_media %>% 
  model(STL(Turnover)) %>%
  components() %>% 
  autoplot()

vic_print_media %>% 
  model(SNAIVE(Turnover)) %>% 
  residuals() %>% 
  features(.resid, features = ljung_box, lag = 10, fitdf = 0)


vic_print_media %>% 
  X11(Turnover)

vic_print_media %>% 
  model(SNAIVE(Turnover)) %>% 
  glance()


vic_print_media %>% 
  model(SNAIVE(Turnover)) %>% 
  test/hypothesise(test = ljung_box())