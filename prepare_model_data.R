
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
  dplyr::filter(date <= '2021-12-30') %>% 
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
  dplyr::filter(date > '2004-12-31')

registration_data <- registration_data %>% 
  dplyr::filter(fund_code %in% funds_regist_nav)

############################################################################
############################################################################
###                                                                      ###
###                       FUNDS SELECTION CRITERIA                       ###
###                                                                      ###
############################################################################
############################################################################

select_elegible_funds <- function(start_date, end_date){
  nav_period <- nav_data %>% 
    arrange(date) %>% 
    dplyr::filter(date >= start_date & date <= end_date)
  
  # First restriction
  funds_first_retriction <- registration_data %>%
    dplyr::filter(
      inception_date < start_date & (closing_date > end_date | is.na(closing_date))
    ) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  # Second restriction
  funds_second_restriction <- nav_period %>% 
    dplyr::filter(fund_code %in% funds_first_retriction) %>% 
    group_by(fund_code) %>% 
    summarise(avg_aum = mean(aum, na.rm = TRUE)) %>% 
    dplyr::filter(avg_aum > 10000) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  # Third Restriction
  ## First we get the number of trading days in this specified period based on NEFIN data
  number_traiding_days <- nefin %>% 
    dplyr::filter(date >= start_date & date <= end_date) %>% 
    summarise(number_traiding_days = length(unique(date))) %>% 
    pull(number_traiding_days)

  funds_third_restriction <- nav_period %>%
    dplyr::filter(fund_code %in% funds_second_restriction) %>% 
    drop_na(nav_return) %>% 
    group_by(fund_code) %>% 
    summarise(pct_not_missing = n() / number_traiding_days) %>% 
    dplyr::filter(pct_not_missing > 0.9) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  # Fourth Restriction
  funds_fourth_restriction <- registration_data %>% 
    dplyr::filter(
      fund_code %in% funds_third_restriction &
        fund_name %ni% fund_name[grepl('master', fund_name)]
    ) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  # Fifth Restriction
  all_asset_managers <- unique(funds_fourth_restriction$asset_manager)
  
  specific_asset_manager_funds <- registration_data %>% 
    dplyr::filter(fund_code %in% funds_fourth_restriction) %>% 
    dplyr::filter(asset_manager == 'Safra Asset Management Ltda') %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  nav_asset_manager_funds <- nav_period %>% 
    dplyr::filter(fund_code %in% specific_asset_manager_funds) %>% 
    pivot_wider(id_cols = c('date'), names_from = 'fund_code', values_from = 'nav_return') %>% 
    right_join(
      dplyr::filter(dplyr::select(nefin, 'date'), date >= start_date & date <= end_date), 
      by = 'date'
    ) %>% 
    dplyr::select(!date) %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  cor_asset_manager_funds <- cor(nav_asset_manager_funds)
  cor_asset_manager_funds[lower.tri(cor_asset_manager_funds, diag = TRUE)] <- NA
  cor_asset_manager_funds[cor_asset_manager_funds < 0.90] <- NA
  
  cor_asset_manager_funds <- cor_asset_manager_funds %>%
    as.data.frame() %>%
    mutate(var1 = rownames(.)) %>%
    gather(var2, value, -var1) %>%
    arrange(desc(value)) %>% 
    drop_na(value)
    
}




