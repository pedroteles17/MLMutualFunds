
clean_data_path <- 'clean_data/'

###########################################################################
###########################################################################
###                                                                     ###
###                       IMPORT AND PROCESS DATA                       ###
###                                                                     ###
###########################################################################
###########################################################################

registration_data <- readRDS(paste0(clean_data_path, 'registration_data.rds'))

nav_data <- readRDS(paste0(clean_data_path, 'nav_data.rds'))

nefin <- read_excel(paste0(clean_data_path, 'nefin.xlsx')) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  dplyr::filter(date < '2022-03-01') %>% 
  mutate(Market = Rm_minus_Rf + Risk_free)

registration_data <- registration_data %>%
  mutate(
    qualified_investor = ifelse(qualified_investor == "Não", 0, 1),
    leverage = ifelse(leverage == "Não", 0, 1),
    redemption_period = redemption_conversion_period + redemption_payment_period,
    condo_type = ifelse(condo_type == "Fechado", 0, 1),
    fund_of_funds = ifelse(fund_of_funds == "Não", 0, 1),
    exclusive_fund = ifelse(exclusive_fund == "Não", 0, 1)
  ) %>% 
  distinct(.keep_all = TRUE)

# This may be caused by joining fund_code and registration_data  
warning(str_glue(
  '{length(setdiff(registration_data$fund_code, nav_data$fund_code))}',
  ' funds lack registration data despite having NAV data.' 
))

warning(str_glue(
  '{length(setdiff(nav_data$fund_code, registration_data$fund_code))}',
  ' funds lack NAV data despite having registration data.' 
))

# We only keep the funds that are in both registration and NAV data frame. 
funds_regist_nav <- intersect(registration_data$fund_code, nav_data$fund_code)

nav_data <- nav_data %>% 
  dplyr::filter(fund_code %in% funds_regist_nav) %>% 
  dplyr::filter(date > '2003-12-31') %>% 
  group_by(date) %>% 
  mutate(nav_return = winsorize(nav_return)) %>% 
  ungroup()

registration_data <- registration_data %>% 
  dplyr::filter(fund_code %in% funds_regist_nav)

rm(funds_regist_nav, clean_data_path)

###########################################################################
###########################################################################
###                                                                     ###
###                                DATES                                ###
###                                                                     ###
###########################################################################
###########################################################################

predict_n_months <- 1
rebalance_frequency <- 1
estimate_n_months_ago <- 12

start_date <- as.Date("2005-01-01")
# We need to subtract to ensure that we will have data to evaluate the model
end_date <- as.Date("2022-03-01") - months(predict_n_months)
n_months <- interval(start_date, end_date) %/% months(rebalance_frequency)

# Base Dates because ew will consider these dates for multiple circumstances
base_dates <- start_date %m+% months(0:n_months) # Functions do not use last date

rm(start_date, end_date, n_months, rebalance_frequency)

############################################################################
############################################################################
###                                                                      ###
###                       FUNDS SELECTION CRITERIA                       ###
###                                                                      ###
############################################################################
############################################################################

# WARNING: Might take a long time to run (5 minutes depending on the machine)
funds_selection <- map(
  base_dates, 
  \(x) select_eligible_funds(
    nav_data, registration_data, nefin, x, estimate_n_months_ago, 10000, 0.05
  ),
  .progress = TRUE
)

names(funds_selection) <- base_dates

eligible_funds <- lapply(funds_selection, function(x) x[[1]])

rm(select_eligible_funds)

###########################################################################
###########################################################################
###                                                                     ###
###                         FEATURE ENGINEERING                         ###
###                                                                     ###
###########################################################################
###########################################################################

# WARNING: Might take a long time to run (5 minutes depending on the machine)
nav_features <- pmap(
  list(
    eligible_funds, list(nav_data), list(nefin), base_dates, estimate_n_months_ago
  ),
  generate_features,
  .progress = TRUE
) %>% 
  bind_rows()

registration_features <- registration_data %>% 
  dplyr::select(
    fund_code, qualified_investor, leverage,
    inception_date, redemption_period,
    minimum_first_investment, condo_type,
    fund_of_funds, exclusive_fund
  )

model_features <- merge(
  nav_features, registration_features, 
  by = 'fund_code', all.x = TRUE
) %>% 
  mutate(
    age = as.numeric(date - inception_date)
  ) %>% 
  dplyr::select(!inception_date)

rm(generate_features, nav_features, registration_features)

############################################################################
############################################################################
###                                                                      ###
###                          DEPENDENT VARIABLE                          ###
###                                                                      ###
############################################################################
############################################################################

abnormal_returns <- pmap(
  list(
    eligible_funds, list(nav_data), list(nefin), base_dates, estimate_n_months_ago, predict_n_months
  ),
  generate_dependent_variable,
  .progress = TRUE
) %>% 
  bind_rows()

rm(generate_dependent_variable, estimate_n_months_ago, predict_n_months, base_dates)

###########################################################################
###########################################################################
###                                                                     ###
###              MERGE DEPENDENT AND INDEPENDENT VARIABLES              ###
###                                                                     ###
###########################################################################
###########################################################################

model_data <- merge(
  abnormal_returns, model_features, by = c('date', 'fund_code'), all = TRUE
)

print('NAs:')
colSums(is.na(model_data))

print('Infinite')
apply(model_data, 2, function(x) sum(is.infinite(x)))

summary(model_data)

###########################################################################
###########################################################################
###                                                                     ###
###                             EXPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

model_folder <- '/model'

dir.create(file.path(getwd(), model_folder), showWarnings = FALSE)

write.csv(
  model_data, 
  paste0(getwd(), model_folder, '/model_data.csv'),
  row.names = FALSE
)
