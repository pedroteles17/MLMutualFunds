
source('99_functions.R')

###########################################################################
###########################################################################
###                                                                     ###
###                              SECTION 1:                             ###
###                             IMPORT DATA                             ###
###                                                                     ###
###########################################################################
###########################################################################

##################################################################
##                          Daily Data                          ##
##################################################################

folder_path <-'raw_data/'

# Data types in daily frequency
daily_data_types <- c(
  'nav', 'aum', 'flow', 'inflow', 
  'outflow', 'number_shareholders'
)

# Because we have extraction data limits, 10 funds were left behind
ten_remaining_funds <- read_excel(
  "raw_data/ten_remaining_funds.xlsx", na = "-", skip = 3, col_types = 'text'
) 

list_ten_remaining <- vector("list", length = 6)
for (i in seq_along(list_ten_remaining)) {
  list_ten_remaining[[i]] <- ten_remaining_funds[, c(1, (2 + 11 * (i - 1)):(1 + i * 11))]
}

ten_remaining <- lapply(list_ten_remaining, clean_economatica_data)
names(ten_remaining) <- daily_data_types

# All funds (active and canceled). 
## WARNING: Might take a long time to run (5 minutes depending on the machine)
daily_data <- Map(join_data, daily_data_types, ten_remaining)
names(daily_data) <- daily_data_types

daily_data <- Reduce(function(x, y) merge(x, y, all=TRUE, by = c('date', 'fund_code')), daily_data)

# Data Quality check
## Manual Correction 1:
data_quality_hyp1(daily_data)

daily_data <- daily_data %>%
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
  drop_na(nav) %>% 
  arrange(date) %>% 
  group_by(fund_code) %>% 
  mutate(
    nav_return = append(NA, diff(nav)/nav[-length(nav)]), .after = 3
  ) %>% 
  dplyr::select(!nav)

rm(daily_data_types, ten_remaining_funds, list_ten_remaining, i, ten_remaining)

#################################################################
##                      Registration Data                      ##
#################################################################

# Each fund receives a unique code. We use this to avoid false duplicates. 
fund_code_active <- read_excel(
  "raw_data/code_active.xlsx", na = "-", skip = 3
) 

fund_code_canceled <- read_excel(
  "raw_data/code_canceled.xlsx", na = "-", skip = 3
) 

fund_code <- rbind(fund_code_active, fund_code_canceled) %>% 
  dplyr::select(-1) %>%
  dplyr::select(c('CNPJ', 'Código')) %>% 
  set_names(c('cnpj', 'code'))

registration_data <- read_excel(
  "raw_data/registration_data.xlsx", na = "-", skip = 3
) %>% 
  dplyr::select(-c(1, 3))

colnames(registration_data) <- c(
  "nome", "pais_sede", "tipo_ativo", "ativo_cancelado", "cnpj",
  "classific_anbima", "gestor_carteira", "empresa_gestora",
  "administradora", "benchmark", "invest_qualificado", "alavancado",
  "data_inicio", "data_fim", "prazo_emis_cota", "prazo_conv_resg",
  "prazo_pag_resg", "aplic_min_inic", "situac_atual", "data_inicio_situac_atual",
  "classe", "forma_condominio", "fundo_cotas", "fundo_exclusivo",
  "fundo", "classific_cvm", "subclasse_cvm", "codigo"
)


# Get full registration data
registration_data <- merge(registration_data, fund_code, by = "cnpj")

rm(fund_code, fund_code_canceled, fund_code_active)


