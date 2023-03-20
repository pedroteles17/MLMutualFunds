
clean_data_path <- 'clean_data/'

###########################################################################
###########################################################################
###                                                                     ###
###                       IMPORT AND PROCESS DATA                       ###
###                                                                     ###
###########################################################################
###########################################################################

registration_data <- readRDS(paste0(clean_data_path, 'registration_data.rds'))

nav_data <- readRDS(paste0(clean_data_path, 'nav_data.rds')) %>% 
  dplyr::filter(date > '2004-12-31')

nefin <- read_excel(paste0(clean_data_path, 'nefin.xlsx')) %>% 
  mutate(date = as.Date(date)) %>% 
  arrange(date) %>% 
  dplyr::filter(date <= '2021-12-30') %>% 
  mutate(Market = Rm_minus_Rf + Risk_free)

# We winsorize outliers in our return data.
nav_data <- nav_data %>% 
  group_by(date) %>% 
  mutate(nav_return = winsorize(nav_return))
