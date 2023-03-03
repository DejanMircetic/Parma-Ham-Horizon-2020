library(fpp3)

google_stock <- gafa_stock %>%
  filter(Symbol == "GOOG", year(Date) >= 2015) %>%
  mutate(day = row_number()) %>%
  update_tsibble(index = day, regular = TRUE)

google_2015 <- google_stock %>% filter(year(Date) == 2015)

google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1) %>%
  relocate(Date, Symbol, .id)
google_2015_tr


google_2015_tr %>%
  model(RW(Close ~ drift())) %>%
  forecast(h = 1) %>%
  accuracy(google_2015)

google_2015_tr <- google_2015 %>%
  stretch_tsibble(.init = 3, .step = 1)
fc <- google_2015_tr %>%
  model(naive=RW(Close ~ drift()),
        es = ETS(Close)) %>%
  forecast(h = 8) %>%
  group_by(.id,.model) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Close", distribution = Close)
fc %>%
  accuracy(google_2015, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE, group=.model, colour=.model)) +
  geom_point()+
  geom_line()


###

cv_train <- forecasting_data %>%
  stretch_tsibble(.init = 24, .step = 12)
cv_train


fc <- cv_train %>%
  model(
    #neural = NNETAR(),
    #average = MEAN(),
    #naive = NAIVE(~ lag(1)),
    s_naive = SNAIVE(~ lag(12)),
    ets = ETS(),
    theta = THETA(),
    #ar = AR(),
    #var = VAR(),
    arima = ARIMA(),
  ) %>%
  mutate(
    comb1 = (ets + theta + arima) / 3,
    comb2 = (theta + arima) / 2) %>% 
  forecast(h = 24) %>%
  group_by(.id,.model) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Consumption", distribution = Consumption)

fc %>%
  accuracy(forecasting_data, by = c("h", ".model")) %>%
  ggplot(aes(x = h, y = RMSE, group=.model, colour=.model)) +
  geom_point()+
  geom_line()

fc %>%
  accuracy(forecasting_data) %>%
  arrange(RMSSE)


#moze i ovako, po svakom racunatom datumu.

fc %>%
  accuracy(forecasting_data, by = c("month", ".model")) %>%
  ggplot(aes(x = month, y = RMSE, group=.model, colour=.model)) +
  geom_point()+
  geom_line()
####

y <- tsibble(
  Year = 2015:2019,
  Observation = c(110, 110, 110, 110, 120),
  index = Year
)


google_2015_tr <- y %>%
  stretch_tsibble(.init = 2, .step = 1)
google_2015_tr


fc <- google_2015_tr %>%
  model(naive=RW(Observation ~ drift())) %>%
  forecast(h = 1) %>%
  group_by(.id) %>%
  mutate(h = row_number()) %>%
  ungroup() %>%
  as_fable(response = "Observation", distribution = Observation)
fc %>%
  accuracy(y, by = c("Year")) %>%
  ggplot(aes(x = Year, y = RMSE)) +
  geom_point()+
  geom_line()

fc %>%
  accuracy(y, by = c("Year"))

fc %>%
  accuracy(y)

#Znaci to znaci da se racuna normalno kao do sada
sqrt(((110-110)^2+(110-110)^2+(120-110)^2)/3)

#A kada se racuna po periodima onda se samo racuna svaki residual za sebe a ako hoces agregirano za sve te periode 
# mozes i iz te tabele


fc %>%
  accuracy(y, by = c("Year"))

sqrt((0^2+0^2+10^2)/3)
