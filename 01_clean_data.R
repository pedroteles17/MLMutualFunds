
raw_data_path <- 'data/raw_data/'
clean_data_path <- 'data/clean_data/'

##################################################################
##                          NEFIN Data                          ##
##################################################################

nefin <- read_excel(paste0(clean_data_path, 'nefin.xls')) %>% 
  mutate(
    date = paste(year, month, day, sep = "-"),
    date = as.Date(date),
    .after=1
  ) %>% 
  dplyr::select(-c('year', 'month', 'day')) %>%
  arrange(date)

##################################################################
##                          Daily Data                          ##
##################################################################

# Data types in daily frequency
daily_data_types <- c(
  'nav', 'aum', 'net_flow', 'inflow', 
  'outflow', 'number_shareholders'
)

# All funds (separated by start date)
## WARNING: Might take a long time to run (5 minutes depending on the machine)
file_name_list <- c(
  'before_2011-12-31.xlsx', 
  'after_2011-12-30_before_2019-12-31.xlsx',
  'after_2019-12-30.xlsx'
)

daily_data <- map(
  daily_data_types,
  \(x) load_clean_join_data(x, file_name_list),
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
  drop_na(nav_return)
  
daily_data <- Reduce(
  function(x, y) merge(x, y, all=TRUE, by = c('date', 'fund_code')), daily_data
)

rm(
  daily_data_types, nefin, data_quality_hyp1, 
  load_clean_join_data, file_name_list
)

#################################################################
##                      Registration Data                      ##
#################################################################

registration_data <- load_economatica_data(paste0(raw_data_path, 'registration_data/full_period.xlsx')) %>% 
  dplyr::select(-c(1, 3)) %>% 
  set_names(c(
    'fund_name', 'home_country', 'asset_type', 'active_canceled', 'cnpj',
    'anbima_classification', 'portfolio_manager', 'fund_code', 'isin', 
    'manager_fee', 'manager_fee_lifetime', 'full_name', 'anbima_class',
    'anbima_category', 'anbima_subcategory', 'cvm_classification', 'cvm_subclass',
    'asset_manager', 'manager', 'benchmark', 'multimanager', 'qualified_investor',
    'nav_profile', 'restricted', 'leverage', 'disclosure_date', 'fund_type', 'foreing_investment',
    'pension_fund', 'respect_limits', 'inception_date', 'closing_date', 'charges_performance_fee',
    'performance_fee', 'quota_issuance_period', 'redemption_conversion_period', 'redemption_payment_period',
    'initial_lockup_period', 'cyclical_lockup_period','minimum_first_investment', 'minimum_additional_investment',
    'minimum_redemption', 'identifier', 'entry_fee', 'exit_fee', 'performance_benchmark', 'condo_type',  
    'fund_of_funds', 'exclusive_fund', 'portfolio_manager_cvm', 'name_in_other_funds'
  )) %>% 
  mutate(
    fund_name = str_to_lower(fund_name),
    fund_name = iconv(fund_name,from="UTF-8",to="ASCII//TRANSLIT")
  ) %>% 
  # Standardize some columns
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
  mutate(
    benchmark = gsub('IBRX', 'IBRX-100', benchmark),
    multimanager = ifelse(multimanager == 'Não se Aplica', NA, multimanager)
  ) %>%
  mutate(
    foreing_investment = ifelse(foreing_investment == 'Até 40 %', 40, foreing_investment),
    foreing_investment = ifelse(foreing_investment == 'Até 20 %', 20, foreing_investment),
    # Since we dont know the exact value, we will assume the smallest value (20)
    foreing_investment = ifelse(foreing_investment == 'Sim', 20, foreing_investment),
    foreing_investment = ifelse(foreing_investment == 'Não', 0, foreing_investment),
    foreing_investment = ifelse(foreing_investment == 'Não se Aplica', 0, foreing_investment),
    foreing_investment = ifelse(foreing_investment == 'Até 100%', 100, foreing_investment),
    foreing_investment = ifelse(foreing_investment == 'ND', 0, foreing_investment),
    foreing_investment = ifelse(foreing_investment == '> 67%', 67, foreing_investment),
  ) %>% 
  mutate(
    performance_fee = str_extract(performance_fee, "\\d+[,.]?\\d*%?"),
    performance_fee = str_replace_all(performance_fee, ',', '.'),
    performance_fee = str_replace_all(performance_fee, '%', ''),
    performance_fee = ifelse(charges_performance_fee == 'Não', 0, performance_fee),
    performance_fee = as.numeric(performance_fee)
  ) %>% 
  # Right data types
  mutate(across(
    c(
      quota_issuance_period, redemption_conversion_period, redemption_payment_period, 
      minimum_first_investment, initial_lockup_period, minimum_additional_investment,
      minimum_redemption
    ), 
    ~as.numeric(.x)
  )) %>% 
  mutate(across(
    c(inception_date, closing_date), ~as.Date(as.numeric(.x), origin="1899-12-30")
  )) %>%
  dplyr::select(c(
      "cnpj", "fund_name", "anbima_classification", "portfolio_manager", 
      "asset_manager", "qualified_investor", "leverage", "inception_date", 
      "closing_date", "quota_issuance_period", "redemption_conversion_period", 
      "redemption_payment_period", "minimum_first_investment", "condo_type", 
      "fund_of_funds", "exclusive_fund", "fund_type", "fund_code", 
      # The fields below were added to the research in 2024
      "multimanager", "foreing_investment", "pension_fund", "charges_performance_fee", 
      "performance_fee", "initial_lockup_period", "minimum_additional_investment", 
      "minimum_redemption"
  ))

rm(raw_data_path, load_economatica_data)

#################################################################
##                        Save the data                        ##
#################################################################

write_parquet(registration_data, paste0(clean_data_path, 'registration_data.parquet'))
write_parquet(daily_data, paste0(clean_data_path, 'nav_data.parquet'))

rm(registration_data, daily_data, clean_data_path)
