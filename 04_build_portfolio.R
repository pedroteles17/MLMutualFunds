
clean_data_path <- 'clean_data/'
model_path <- 'model/'

###########################################################################
###########################################################################
###                                                                     ###
###                       IMPORT AND PROCESS DATA                       ###
###                                                                     ###
###########################################################################
###########################################################################

predictions <- read.csv(
  paste0(model_path, 'predictions/predictions.csv'), colClasses="character"
) %>% 
  mutate(date = as.Date(date)) %>% 
  mutate_at(
    c('prediction', 'true_value', 'execution_time'), as.numeric
  ) %>% 
  distinct(date, funds_code, .keep_all = TRUE)

nav_data <- readRDS(paste0(clean_data_path, 'nav_data.rds')) %>% 
  dplyr::filter(
    date > '2008-12-31'
  ) %>% 
  group_by(date) %>% 
  mutate(nav_return = winsorize(nav_return)) %>% 
  ungroup() %>% 
  dplyr::filter(fund_code %in% unique(predictions$funds_code)) %>% 
  arrange(date)

rm(winsorize, clean_data_path)

###########################################################################
###########################################################################
###                                                                     ###
###                         ANALYZE PREDICTIONS                         ###
###                                                                     ###
###########################################################################
###########################################################################

# Based on ranking, each prediction goes to a decile 
## 1: higher predicted abnormal return; 10: lower predicted abnormal return
predictions <- predictions %>% 
  group_by(model, date) %>% 
  mutate(
    pred_decile = ceiling(rank(-prediction) / n() * 10),
    true_decile = ceiling(rank(-true_value) / n() * 10)
  )

# Test the quality of the out-of-sample predictions
## Article: Machine learning and the cross-section of emerging market stock returns
## Diebold-Mariano-West 
dmw_test_statistic <- predictions %>% 
  mutate(
    squared_pred_error = (true_value - prediction) ^ 2,
    squared_true_value = (true_value) ^ 2
  ) %>% 
  group_by(model) %>% 
  summarise(
    dmw = 1 - (sum(squared_pred_error) / sum(squared_true_value)),
    mse = mean(squared_pred_error)
  )

# Frequency of abnormal return decile in each predicted abnormal return decile
pred_true_ranking <- predictions %>% 
  group_by(model, pred_decile, true_decile) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(model, pred_decile) %>% 
  mutate(freq = freq / sum(freq)) %>% 
  dplyr::filter(model == 'XGBRegressor')

# Return of each decile
## WARNING: A monotonic relation appears to exist in the opposite way expected
decile_return <- predictions %>% 
  dplyr::filter(model == 'XGBRegressor') %>% 
  group_by(date, pred_decile) %>% 
  group_map(
    ~data.frame(
      pred_decile = .y$pred_decile,
      calculate_portfolio_retuns(nav_data, .x$funds_code, .y$date, 1)
    )
  ) %>% 
  bind_rows() %>% 
  pivot_wider(names_from = 'pred_decile', values_from = 'portfolio.returns')

decile_return_xts <- xts(decile_return[,-1], decile_return$date)

PerformanceAnalytics::charts.PerformanceSummary(decile_return_xts, plot.engine = 'plotly')

