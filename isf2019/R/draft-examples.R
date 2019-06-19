library(tidyverse)
library(fable)
library(feasts)
library(tsibble)

aus_trips <- tourism %>% 
  summarise(Trips = sum(Trips))
aus_trips %>% autoplot(Trips)

aus_trips %>% 
  model(ETS(Trips)) %>% 
  forecast() %>% 
  autoplot(aus_trips)

aus_trips %>% 
  model(
    ETS(Trips),
    ARIMA(Trips)
  ) %>% 
  forecast() %>% 
  autoplot(aus_trips)

aus_trips %>% 
  model(
    ETS(Trips),
    ARIMA(Trips),
    (ETS(Trips) + ARIMA(Trips))/2
  ) %>% 
  accuracy()


fit <- tourism %>% 
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips)
  ) %>% 
  mutate(
    combination = (ets + arima)/2
  )

fit %>% 
  accuracy() %>% 
  ggplot(aes(x = MASE, colour = .model)) + 
  geom_density()

library(future)
plan(multiprocess)
fit <- tourism %>% 
  model(
    ets = ETS(Trips),
    arima = ARIMA(Trips),
    lm = TSLM(Trips ~ trend() + season()),
    fasster = fasster::FASSTER(Trips ~ poly(2) + seas("year"))
  ) %>% 
  mutate(
    combination2 = (ets + arima)/2,
    combination3 = (ets + arima + lm)/3,
    combination4 = (ets + arima + lm + fasster)/4
  )
