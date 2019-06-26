library(tidyverse)
library(tsibble)
library(fable)
library(feasts)
library(tsibbledata)

vic_print_media <- tsibbledata::aus_retail %>%
  filter(Industry == "Newspaper and book retailing", State == "Victoria")

# Decomposition modelling
vic_print_media %>% 
  model(
    stlf = dcmp_model(STL, log(Turnover) ~ trend(window = 13) + season(window = "periodic"),
                      NAIVE(seas_adjust), dcmp_args = list(robust=TRUE))
  ) %>% 
  forecast() %>% 
  autoplot(vic_print_media)


## dcmp_model or dcmp_mdl? Some students confuse model() and dcmp_model()

# Model combination
dcmp <- vic_print_media %>% 
  STL(Turnover ~ trend(window = 13) + season(window = 7))
dcmp %>% 
  model(
    a = NAIVE(seas_adjust),
    b = SNAIVE(season_year)
  ) %>% 
  mutate(
    c = a+b
  ) %>% 
  select(State, Industry, c) %>% 
  forecast() %>% 
  autoplot(dcmp)

# Ensemble
vic_print_media %>% 
  model(
    arima = ARIMA(Turnover),
    ets = ETS(Turnover)
  ) %>% 
  mutate(
    combination = (arima + ets)/2
  ) %>% 
  forecast() %>% 
  autoplot(tail(vic_print_media, 24), level = 80, alpha = 0.5)

# Reconciliation
## Based on forecasts, but requires model residuals for reconciliation weights

aus_print_media %>% 
  model(ETS(Turnover))

fit <- .Last.value

fit$`ETS(Turnover)` %>% str

vic_print_media %>% 
  group/hierarchy(State / Industry) %>% # Data manipulation step that defines the key structure
  model(
    my_mdl = ETS(Turnover)
  ) %>% 
  reconcile(my_mdl, method, ...) %>% # dots are passed to method, providing data if required
  # reconcile changes the lst_mdl structure to include details about the forecast method
  forecast()