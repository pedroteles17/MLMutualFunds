
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
  drop_na() %>% 
  mutate(age = age / 365)

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
  mutate(weights = 1 / n()) %>% 
  group_map(
    ~data.frame(
      model = .y$model,
      pred_decile = .y$pred_decile,
      calculate_portfolio_retuns(nav_data, .x$fund_code, .x$weights, .y$date, 1)
    )
  ) %>% 
  bind_rows() %>% 
  arrange(date, model)

# Return of each tercil from each model
tercil_return <- predictions_tercil %>% 
  group_by(model, date, pred_tercil) %>% 
  mutate(weights = 1 / n()) %>% 
  group_map(
    ~data.frame(
      model = .y$model,
      pred_tercil = .y$pred_tercil,
      calculate_portfolio_retuns(nav_data, .x$fund_code, .x$weights, .y$date, 1)
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
  "Track Error (STM)", "Kurtosis (STM)", "t-stat Alpha (STM)",
  "t-stat Market (STM)", "t-stat Size (STM)", "t-stat Value (STM)", 
  "t-stat Momentum (STM)", "R^2 (STM)", 'IVol (STM)', "MIR (Mom.)", 
  "CVaR (Mom.)", "Track Error (Mom.)", "Kurtosis (Mom.)", 
  "Alpha (Mom.)", "t-stat Market (Mom.)", "t-stat Size (Mom.)", 
  "t-stat Value (Mom.)", "t-stat Momentum (Mom.)", "R^2 (Mom.)", 
  'IVol (Mom.)', "MIR (STR)", "CVaR (STR)", "Track Error (STR)", 
  "Kurtosis (STR)", "Alpha (STR)", "t-stat Market (STR)", "t-stat Size (STR)", 
  "t-stat Value (STR)", "t-stat Momentum (STR)", "R^2 (STR)", 'IVol (STR)', 
  "AUM", "Inflows", "Outflows", "# Shareholders", "% Flow", 
  "Leverage", "Redemption Period", "Condo Type", "FoF", "Exclusive", "Age"
)

rm(num_funds, df_summary)

##################################################################
##                  Table 2 - Panel Regression                  ##
##################################################################

panel_data <- model_data %>% 
  dplyr::select(
    !c(date, fund_code)
  )

linear_regres <- lm(abnormal_return ~ ., data = panel_data)

variable_names <- c(
  "MIR (STM)", "CVaR (STM)", 
  "Track Error (STM)", "Kurtosis (STM)", "t-stat Alpha (STM)",
  "t-stat Market (STM)", "t-stat Size (STM)", "t-stat Value (STM)", 
  "t-stat Momentum (STM)", "R^2 (STM)", 'IVol (STM)', "MIR (Mom.)", 
  "CVaR (Mom.)", "Track Error (Mom.)", "Kurtosis (Mom.)", 
  "Alpha (Mom.)", "t-stat Market (Mom.)", "t-stat Size (Mom.)", 
  "t-stat Value (Mom.)", "t-stat Momentum (Mom.)", "R^2 (Mom.)", 
  'IVol (Mom.)', "MIR (STR)", "CVaR (STR)", "Track Error (STR)", 
  "Kurtosis (STR)", "Alpha (STR)", "t-stat Market (STR)", "t-stat Size (STR)", 
  "t-stat Value (STR)", "t-stat Momentum (STR)", "R^2 (STR)", 'IVol (STR)', 
  "AUM", "Inflows", "Outflows", "# Shareholders", "% Flow", 
  "Leverage", "Redemption Period", "Condo Type", "FoF", "Exclusive", "Age",
  "Constant"
)

stargazer(
  linear_regres,
  title="Pooled Regression", align=TRUE, 
  covariate.labels = variable_names,
  dep.var.labels = "Abnormal Return",
  no.space = TRUE, single.row=TRUE
)

rm(panel_data, linear_regres, variable_names)

##################################################################
##             Table 3 - Decile Performance Summary             ##
##################################################################

# Characteristic based
model_data_tb3 <- model_data %>% 
  distinct(date, fund_code, .keep_all = TRUE)

average_characteristics <- predictions_decil %>%
  dplyr::filter(
    model == 'XGBRegressor'
  ) %>% 
  dplyr::select(
    fund_code, date, pred_decile
  ) %>% 
  left_join(
    model_data_tb3, by = c('fund_code', 'date')
  ) %>% 
  group_by(pred_decile) %>% 
  summarise(
    aum = mean(avg_aum),
    inflow = mean(inflow),
    outflow = mean(outflow),
    num_shareholders = mean(number_shareholders),
    leverage = mean(leverage),
    fof = mean(fund_of_funds),
    exclusive = mean(exclusive_fund),
    age = mean(age),
    redemption_period = mean(redemption_period)
  ) %>% 
  column_to_rownames('pred_decile') %>% 
  t() %>% as.data.frame()

colnames(average_characteristics) <- paste('Decile', colnames(average_characteristics))

average_characteristics$Market <- NA

# Return Based
nefin_eval <- nefin %>% 
  dplyr::filter(
    date >= min(decile_return$date) & date <= max(decile_return$date)
  )

performance_metrics <- decile_return %>% 
  dplyr::filter(model == 'XGBRegressor') %>% 
  group_by(pred_decile) %>% 
  group_map(
    ~performance_summary(
      .x$portfolio.returns, nefin_eval, .y$pred_decile
    )
  ) %>% 
  bind_cols()

# Add market summary statistics
market_metrics <- performance_summary(nefin_eval$Market, nefin_eval, 'Market') %>% 
  rename(Market = `Decile Market`)
market_metrics$Market[c(3,4,5,6,8)] <- NA

performance_metrics <- cbind(performance_metrics, market_metrics)

# Full table 2
table2 <- rbind(performance_metrics, average_characteristics)

rm(model_data_tb3, average_characteristics, nefin_eval, performance_metrics)

##################################################################
##                Table 4 - Weight by Prediction                ##
##################################################################

# Return of each tercil from each model
tercil_return_by_weight <- predictions_tercil %>% 
  dplyr::filter(model == 'XGBRegressor') %>% 
  group_by(date, pred_tercil) %>% 
  # Equal weighted, 
  mutate(
    equal_weighted = 1 / n(),
    rank_weighted = rank(abs(prediction))/ sum(rank(abs(prediction))),
    prediction_weighted = abs(prediction) / sum(abs(prediction))
  ) %>% 
  ungroup() %>% 
  dplyr::select(
    fund_code, date, pred_tercil, equal_weighted, 
    rank_weighted, prediction_weighted
  ) %>% 
  pivot_longer(
    !c(fund_code, date, pred_tercil), names_to = 'weight_type', values_to = 'weights'
  ) %>% 
  arrange(date, pred_tercil) %>% 
  group_by(date, pred_tercil, weight_type) %>% 
  group_map(
    ~data.frame(
      weight_type = .y$weight_type,
      pred_tercil = .y$pred_tercil,
      calculate_portfolio_retuns(nav_data, .x$fund_code, .x$weights, .y$date, 1)
    )
  ) %>% 
  bind_rows() %>% 
  arrange(date, pred_tercil)

# Long and Short (buy high predicted (100%), sell low predicted(-100%), buy risk free (100%))
long_short_return_by_weight <- tercil_return_by_weight %>% 
  dplyr::filter(pred_tercil != 2) %>% 
  group_by(weight_type) %>% 
  group_map(
    ~data.frame(
      weight_type = .y$weight_type,
      calculate_long_short_returns(.x, nefin)
    )
  ) %>% 
  bind_rows() %>% 
  arrange(date)

# Return Based
nefin_eval <- nefin %>% 
  dplyr::filter(
    date >= min(decile_return$date) & date <= max(decile_return$date)
  )

performance_metrics <- long_short_return_by_weight %>% 
  group_by(weight_type) %>% 
  group_map(
    ~performance_summary(
      .x$portfolio.returns, nefin_eval, .y$weight_type
    )
  ) %>% 
  bind_cols()

# Add market summary statistics
risk_free_metrics <- performance_summary(nefin_eval$Risk_free, nefin_eval, 'Risk_free') %>% 
  rename(`Risk Free` = `Decile Risk_free`)
risk_free_metrics$`Risk Free`[2:9] <- NA

performance_metrics_by_weight <- cbind(performance_metrics, risk_free_metrics)

##################################################################
##                 Figure 1 - Prediction - True                 ##
##################################################################

predictions_by_group <- predictions %>% 
  dplyr::filter(
    model %in% c('XGBRegressor', 'SVR', 'RandomForestRegressor', 'LinearRegression')
  ) %>% 
  group_by(model, date) %>%
  mutate(
    pred_rank_group = ceiling( rank(-prediction) / n() / 0.25 ),
    true_rank_group = ceiling( rank(-true_value) / n() / 0.25 )
  ) %>% 
  mutate(
    pred_rank_group = factor(pred_rank_group, levels = 1:max(pred_rank_group)),
    true_rank_group = factor(true_rank_group, levels = max(true_rank_group):1)
  ) 

pred_true_ranking <- predictions_by_group %>% 
  group_by(model, pred_rank_group, true_rank_group) %>% 
  summarise(freq = n()) %>% 
  ungroup() %>% 
  group_by(model, pred_rank_group) %>% 
  mutate(freq = freq / sum(freq)) %>% 
  mutate(
    model = ifelse(model == 'RandomForestRegressor', 'Random Forest', model),
    model = ifelse(model == 'LinearRegression', 'Linear Regression', model),
    model = ifelse(model == 'XGBRegressor', 'XGBoost', model),
    model = ifelse(model == 'SVR', 'Support Vector Regression', model),
  )

figure1 <- ggplot(pred_true_ranking, aes(fill=true_rank_group, y=freq, x=pred_rank_group)) + 
  geom_bar(position="stack", stat="identity") +
  geom_abline(slope = 0, intercept = 0.25) +
  geom_abline(slope = 0, intercept = 0.5) +
  geom_abline(slope = 0, intercept = 0.75) +
  ylab('Frequency') + xlab('Predicted Quartile') +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_viridis(discrete = T) +
  theme_classic() +
  guides(fill=guide_legend(title="True Quartile")) +
  facet_wrap(vars(model), ncol = 2)

rm(predictions_by_group, pred_true_ranking)

#################################################################
##     Figure 2 - Alpha / Running Time/ Prediction Quality     ##
#################################################################

running_time <- predictions %>% 
  group_by(model) %>% 
  summarise(
    running_time = mean(execution_time)
  )

tercile_returns_regres <- long_short_return %>% 
  left_join(nefin, by = 'date') %>% 
  group_by(model) %>% 
  summarise(
    alpha = coef(lm(I(portfolio.returns - Risk_free) ~ Rm_minus_Rf + SMB + HML + WML))[1]
  ) %>% 
  mutate(alpha = (alpha + 1) ^ 252 - 1)

# Test the quality of the out-of-sample predictions
out_of_sample_r2 <- predictions %>% 
  mutate(
    squared_pred_error = (true_value - prediction) ^ 2,
    squared_true_value = (true_value) ^ 2
  ) %>% 
  group_by(model) %>% 
  summarise(
    r2 = 1 - (sum(squared_pred_error) / sum(squared_true_value))
  )

plot3_data <- merge(
  merge(running_time, tercile_returns_regres, by = 'model'),
  out_of_sample_r2, by = 'model'
) %>% 
  mutate(
    model_group = c(
      'Linear', 'Linear', 'Ensemble',
      'Others', 'Linear', 'Ensemble',
      'Linear', 'Ensemble', 'Linear',
      'Others', 'Ensemble'
    )
  )

figure3 <- ggplot(plot3_data, aes(x = r2, y = alpha)) +
  geom_point(aes(size = running_time, color = model_group)) +
  geom_label_repel(label = plot3_data$model,  size=3.5) +
  scale_size_continuous(range = c(1, 15)) +
  scale_y_continuous(labels = scales::percent) +
  theme_classic() +
  xlab("Out-of-sample R^2") + ylab("Annual. Alpha") +
  labs(color = "Model", size = "Exe. Time") + 
  theme(axis.title.x = element_text(vjust=-0.5),
        axis.title.y = element_text(vjust=1.5))

rm(running_time, tercile_returns_regres, out_of_sample_r2, plot3_data)

#################################################################
##                Figure 3 - Feature Importance                ##
#################################################################

feature_importance <- read_csv(paste0(model_path, 'predictions/feature_importance.csv')) %>% 
  set_names(c('Feature', 'Gain')) %>% 
  arrange(-Gain) 

variable_names <- c(
  'IVol (STR)', 'Track Error (STR)', 'MIR (Mom.)',
  't-stat Market (STR)', 'IVol (STM)', 't-stat Market (Mom.)',
  'IVol (Mom.)', 't-stat Mom. (Mom.)', 't-stat Value (Mom.)',
  't-stat Value (STM)', 'MIR (STR)', 't-stat Alpha (Mom.)',
  'R^2 (Mom.)', 'Kurtosis (Mom.)', 't-stat Value (STR)',
  'Condo Type', 't-stat Size (Mom.)', 'CVaR (STR)',
  't-stat Alpha (STR)', 'Track Error (Mom.)', 't-stat Momentum (STR)',
  'R^2 (STR)', 'Alpha (STM)', 'Kurtosis (STR)', 'Age', '# Shareholders', 
  'Track Error (STM)', 'R^2 (STM)', 'CVaR (Mom.)', 't-stat Mom. (STM)', 
  'CVaR (STM)', 'Inflow', 'Kurtosis (STM)', 'Redemption Period', 
  't-stat Market (STM)', '% Flow', 't-stat Size (STR)', 'AUM', 
  't-stat Size (STM)', 'Exclusive', 'Outflow', 'MIR (STM)', 'FoF', 'Leverage'
)

ggplot(data=feature_importance, aes(x=reorder(Feature,Gain), y=Gain)) +
  geom_bar(stat="identity", fill = "darkblue") +
  scale_x_discrete(labels=rev(variable_names)) +
  xlab(element_blank()) +
  coord_flip() +
  theme_classic()
