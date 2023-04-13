
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
  distinct(date, fund_code, model, .keep_all = TRUE)

nav_data <- readRDS(paste0(clean_data_path, 'nav_data.rds')) %>% 
  drop_na(nav_return) %>% 
  dplyr::filter(
    date > '2008-12-31'
  ) %>% 
  group_by(date) %>% 
  mutate(nav_return = winsorize(nav_return)) %>% 
  ungroup() %>% 
  dplyr::filter(fund_code %in% unique(predictions$fund_code)) %>% 
  arrange(date)

model_data <- read_csv(
  str_glue(model_path, 'model_data.csv')
) %>% 
  dplyr::select(
    !c('minimum_first_investment', 'qualified_investor')
  ) %>% 
  drop_na()

nefin <- read_excel(paste0(clean_data_path, 'nefin.xlsx')) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  dplyr::filter(date < '2022-03-01') %>% 
  mutate(Market = Rm_minus_Rf + Risk_free)

rm(winsorize, clean_data_path)

###########################################################################
###########################################################################
###                                                                     ###
###                         ANALYZE PREDICTIONS                         ###
###                                                                     ###
###########################################################################
###########################################################################

# Based on ranking, each prediction goes to a decile (10%)
## 1: higher predicted abnormal return; 10: lower predicted abnormal return
predictions_decil <- predictions %>% 
  group_by(model, date) %>% 
  mutate(
    pred_decile = ceiling(rank(-prediction) / n() * 10),
    true_decile = ceiling(rank(-true_value) / n() * 10)
  ) %>% 
  ungroup()

# Based on ranking, each prediction goes to a tercil 
## 1: higher predicted abnormal return (30%); 3: lower predicted abnormal return (30%)
predictions_tercil <- predictions %>% 
  group_by(model, date) %>% 
  mutate(
    pred_tercil = rank(-prediction) / n(),
    true_tercil = rank(-true_value) / n()
  ) %>% 
  mutate(
    pred_tercil = ifelse(pred_tercil >= 0.7, 3, pred_tercil),
    pred_tercil = ifelse(pred_tercil > 0.3 & pred_tercil < 0.7, 2, pred_tercil),
    pred_tercil = ifelse(pred_tercil <= 0.3, 1, pred_tercil)
  ) %>% 
  mutate(
    true_tercil = ifelse(true_tercil >= 0.7, 3, true_tercil),
    true_tercil = ifelse(true_tercil > 0.3 & true_tercil < 0.7, 2, true_tercil),
    true_tercil = ifelse(true_tercil <= 0.3, 1, true_tercil)
  ) %>% 
  ungroup()

# Return of each decile from each model
decile_return <- predictions_decil %>% 
  group_by(model, date, pred_decile) %>% 
  group_map(
    ~data.frame(
      model = .y$model,
      pred_decile = .y$pred_decile,
      calculate_portfolio_retuns(nav_data, .x$fund_code, .y$date, 1)
    )
  ) %>% 
  bind_rows() %>% 
  arrange(date, model)

# Return of each tercil from each model
tercil_return <- predictions_tercil %>% 
  group_by(model, date, pred_tercil) %>% 
  group_map(
    ~data.frame(
      model = .y$model,
      pred_tercil = .y$pred_tercil,
      calculate_portfolio_retuns(nav_data, .x$fund_code, .y$date, 1)
    )
  ) %>% 
  bind_rows() %>% 
  arrange(date, model)

# Long and Short (buy high predicted (100%), sell low predicted(-100%), buy risk free (100%))
long_short_return <- tercil_return %>% 
  dplyr::filter(pred_tercil != 2) %>% 
  group_by(model) %>% 
  group_map(
    ~data.frame(
      model = .y$model,
      calculate_long_short_returns(.x, nefin)
    )
  ) %>% 
  bind_rows()
  
dummy_models <- tercil_return %>% 
  dplyr::filter(model %ni% long_short_return$model) %>% 
  dplyr::select(model, date, portfolio.returns)

long_short_return <- long_short_return %>% 
  bind_rows(dummy_models)

# Test the quality of the out-of-sample predictions
## Article: Machine learning and the cross-section of emerging market stock returns
## Diebold-Mariano-West 
dmw_test_statistic <- predictions %>% 
  mutate(
    squared_pred_error = (true_value - prediction) ^ 2,
    squared_true_value = (true_value) ^ 2
  ) %>% 
  group_by(model, date) %>% 
  summarise(
    dmw = 1 - (sum(squared_pred_error) / sum(squared_true_value)),
    mse = mean(squared_pred_error)
  ) %>% 
  dplyr::select(!mse)

ggplotly(
  dmw_test_statistic %>%
    ggplot( aes(x=date, y=dmw, group=model, color=model)) +
    geom_line()
)

# Frequency of abnormal return decile in each predicted abnormal return decile
pred_true_ranking <- predictions_decil %>% 
  group_by(model, pred_decile, true_decile) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(model, pred_decile) %>% 
  mutate(freq = freq / sum(freq)) %>% 
  dplyr::filter(model == 'XGBRegressor')

############################################################################
############################################################################
###                                                                      ###
###                                TABLES                                ###
###                                                                      ###
############################################################################
############################################################################

#################################################################
##                 Table 1 - Funds' Statistics                 ##
#################################################################

num_funds <- model_data %>%
  group_by(date) %>% 
  summarise(
    n_funds = n()
  )

num_funds <- as.data.frame(apply(num_funds[,-1], 2, summary))

df_summary <- model_data %>% 
  dplyr::select(
    !c('date', 'fund_code')
  ) 
  
df_summary <- as.data.frame(apply(df_summary, 2, summary))

table1 <- df_summary %>% 
  mutate(
    num_funds = num_funds$n_funds, .before = 1
  ) %>%
  mutate(
    across(everything(), ~ round(.x, 2))
  ) %>% 
  t() %>% 
  as.data.frame()

rownames(table1) <- c(
  "# Funds", "Abnormal Return",  "MIR (STM)", "CVaR (STM)", 
  "Track Error (STM)", "Kurtosis (STM)", "Alpha (STM)",
  "Beta-Market (STM)", "Beta-Size (STM)", "Beta-Value (STM)", 
  "Beta-Momentum (STM)", "R^2 (STM)", 'IVol (STM)', "MIR (Mom.)", 
  "CVaR (Mom.)", "Track Error (Mom.)", "Kurtosis (Mom.)", 
  "Alpha (Mom.)", "Beta-Market (Mom.)", "Beta-Size (Mom.)", 
  "Beta-Value (Mom.)", "Beta-Momentum (Mom.)", "R^2 (Mom.)", 
  'IVol (Mom.)', "MIR (STR)", "CVaR (STR)", "Track Error (STR)", 
  "Kurtosis (STR)", "Alpha (STR)", "Beta-Market (STR)", "Beta-Size (STR)", 
  "Beta-Value (STR)", "Beta-Momentum (STR)", "R^2 (STR)", 'IVol (STR)', 
  "AUM", "Inflows", "Outflows", "# Shareholders", "% Flow", 
  "Leveradge", "Redemption Period", "Condo Type", "FoF", "Exclusive", "Age"
)

rm(num_funds, df_summary)

##################################################################
##             Table 2 - Decile Performance Summary             ##
##################################################################

nefin_eval <- nefin %>% 
  dplyr::filter(
    date >= min(decile_return$date) & date <= max(decile_return$date)
  )

table2 <- decile_return %>% 
  dplyr::filter(model == 'Lasso') %>% 
  group_by(pred_decile) %>% 
  group_map(
    ~performance_summary(
      .x$portfolio.returns, nefin_eval, .y$pred_decile
    )
  ) %>% 
  bind_cols()



