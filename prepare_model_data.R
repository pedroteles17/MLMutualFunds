
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
  )

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

