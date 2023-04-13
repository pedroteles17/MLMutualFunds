
raw_data_path <- 'raw_data/'
clean_data_path <- 'clean_data/'

##################################################################
##                          NEFIN Data                          ##
##################################################################

nefin <- read_excel(paste0(clean_data_path, 'nefin.xlsx')) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date)

##################################################################
##                          Daily Data                          ##
##################################################################

# Data types in daily frequency
daily_data_types <- c(
  'nav', 'aum', 'flow', 'inflow', 
  'outflow', 'number_shareholders'
)

# Because we have extraction data limits, 10 funds were left behind
ten_remaining_funds <- read_excel(
  paste0(raw_data_path, 'ten_remaining_funds.xlsx'), na = '-', skip = 3, col_types = 'text'
) 

list_ten_remaining <- vector('list', length = 6)
for (i in seq_along(list_ten_remaining)) {
  list_ten_remaining[[i]] <- ten_remaining_funds[, c(1, (2 + 11 * (i - 1)):(1 + i * 11))]
}

ten_remaining <- lapply(list_ten_remaining, clean_economatica_data)
names(ten_remaining) <- daily_data_types

# All funds (active and canceled). 
## WARNING: Might take a long time to run (5 minutes depending on the machine)
daily_data <- pmap(
  list(
    daily_data_types, ten_remaining
  ),
  join_data,
  .progress = TRUE
)
names(daily_data) <- daily_data_types

# Data Quality check
## Manual Correction 1:
data_quality_hyp1(daily_data[['nav']])

daily_data[['nav']] <- daily_data[['nav']] %>% 
  mutate(
    nav = ifelse(nav == 0, NA, nav) # MC1
  ) %>% 
  mutate(
    nav = ifelse(date == '2021-11-11' & fund_code == '462934', NA, nav), # MC2
    nav = ifelse(date == '2021-11-11' & fund_code == '468932', NA, nav), # MC3
    nav = ifelse(date == '2017-07-13' & fund_code == '439649', NA, nav), # MC4
    nav = ifelse(date == '2017-07-14' & fund_code == '439649', NA, nav), # MC4
    nav = ifelse(date == '2014-09-01' & fund_code == '286461', NA, nav), # MC5
    nav = ifelse(date == '2008-07-23' & fund_code == '216879', NA, nav), # MC6
    nav = ifelse(date == '2006-12-06' & fund_code == '174718', NA, nav), # MC7 
    nav = ifelse(date == '2017-12-08' & fund_code == '448087', NA, nav), # MC8
    nav = ifelse(date == '2005-07-08' & fund_code == '147354', NA, nav), # MC9
    nav = ifelse(date == '2006-07-04' & fund_code == '168343', NA, nav), # MC10
    nav = ifelse(date == '2019-09-02' & fund_code == '506583', NA, nav), # MC11
    nav = ifelse(date == '2008-06-20' & fund_code == '211966', NA, nav), # MC12
    nav = ifelse(date == '2007-01-03' & fund_code == '177210', NA, nav), # MC13
  ) %>% 
  drop_na(nav) 

# Using the improved NAV data, we calculate the funds' returns.
daily_data[['nav_return']] <- daily_data[['nav']] %>% 
  arrange(date) %>% 
  right_join(select(nefin, date), by = 'date') %>% 
  group_by(fund_code) %>% 
  mutate(
    nav_return = append(NA, diff(nav)/nav[-length(nav)]), .after = 3
  ) %>% 
  dplyr::select(!nav) %>% 
  dplyr::filter(date < '2022-03-01') %>% 
  drop_na(nav_return)
  
daily_data <- Reduce(
  function(x, y) merge(x, y, all=TRUE, by = c('date', 'fund_code')), daily_data
)

rm(
  daily_data_types, ten_remaining_funds, list_ten_remaining, i, nefin,
  ten_remaining, clean_economatica_data, join_data, data_quality_hyp1
)

#################################################################
##                      Registration Data                      ##
#################################################################

# Each fund receives a unique code. We use this to avoid false duplicates. 
fund_code_active <- read_excel(
  paste0(raw_data_path, 'code_active.xlsx'), na = '-', skip = 3
) 

fund_code_canceled <- read_excel(
  paste0(raw_data_path, 'code_canceled.xlsx'), na = '-', skip = 3
) 

fund_code <- rbind(fund_code_active, fund_code_canceled) %>% 
  dplyr::select(-1) %>%
  dplyr::select(c('CNPJ', 'Nome', 'Código')) %>% 
  set_names(c('cnpj', 'fund_name', 'fund_code')) %>% 
  mutate(
    fund_name = str_to_lower(fund_name),
    fund_name = iconv(fund_name,from="UTF-8",to="ASCII//TRANSLIT")
  ) 

registration_data <- read_excel(
  paste0(raw_data_path, 'registration_data.xlsx'), na = '-', skip = 3
) %>% 
  dplyr::select(-c(1, 3)) %>% 
  set_names(c(
    'fund_name', 'home_country', 'asset_type', 'active_canceled', 'cnpj',
    'anbima_classification', 'portfolio_manager', 'asset_manager',
    'manager', 'benchmark', 'qualified_investor', 'leverage',
    'inception_date', 'closing_date', 'quota_issuance_period', 'redemption_conversion_period',
    'redemption_payment_period', 'minimum_first_investment', 'current_situation', 
    'current_situation_start_date', 'class', 'condo_type', 'fund_of_funds', 'exclusive_fund',
    'fund_type', 'cvm_classification', 'cvm_subclass'
  )) %>% 
  mutate(
    fund_name = str_to_lower(fund_name),
    fund_name = iconv(fund_name,from="UTF-8",to="ASCII//TRANSLIT")
  ) 

# Get full registration data
registration_data <- merge(registration_data, fund_code, by = c('cnpj', 'fund_name'))

registration_data <- registration_data %>% 
  mutate(across(
    c(inception_date, closing_date, current_situation_start_date), ~as.Date(.x)
  )) %>% 
  mutate(
    quota_issuance_period = ifelse(quota_issuance_period %in% c('D=0', 'd=0'), 'D+000', quota_issuance_period),
    quota_issuance_period = ifelse(quota_issuance_period == 'Até 12h, D0; depois disso, D+1', 'D+001', quota_issuance_period),
    quota_issuance_period = str_replace_all(quota_issuance_period, 'D\\+', '')
  ) %>% 
  mutate(
    redemption_conversion_period = ifelse(redemption_conversion_period == 'D+30 dias corridos', 'D+030', redemption_conversion_period),
    redemption_conversion_period = ifelse(redemption_conversion_period %in% c('D=1', 'Até 12h, D0; depois disso, D+1'), 'D+001', redemption_conversion_period),
    redemption_conversion_period = str_replace_all(redemption_conversion_period, 'D\\+', '')
  ) %>% 
  mutate(
    redemption_payment_period = ifelse(redemption_payment_period == '2 dias úteis da conversão', 'D+002', redemption_payment_period),
    redemption_payment_period = ifelse(redemption_payment_period %in% c('4 dias0', 'D=4'), 'D+004', redemption_payment_period),
    redemption_payment_period = str_replace_all(redemption_payment_period, 'D\\+', '')
  ) %>% 
  mutate(across(
    c(quota_issuance_period, redemption_conversion_period, redemption_payment_period, minimum_first_investment), ~as.numeric(.x)
  )) %>% 
  mutate(
    benchmark = gsub('IBRX', 'IBRX-100', benchmark)
  ) %>% 
  dplyr::filter(class == 'Fundo de Ações') %>% 
  dplyr::select(!c(
    'home_country', 'asset_type', 'active_canceled', 'manager', 'benchmark',
    'current_situation', 'current_situation_start_date', 'class',
    'cvm_subclass', 'cvm_classification', 'active_canceled'
  ))

rm(fund_code, fund_code_canceled, fund_code_active, raw_data_path)

#################################################################
##                        Save the data                        ##
#################################################################

# To read: readRDS('dados_tratados_mes.rds')
saveRDS(registration_data, file = paste0(clean_data_path, 'registration_data.rds'))
saveRDS(daily_data, file = paste0(clean_data_path, 'nav_data.rds'))

rm(registration_data, daily_data, clean_data_path)
