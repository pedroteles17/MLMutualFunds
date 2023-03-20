
###########################################################################
###########################################################################
###                                                                     ###
###                           01_CLEAN_DATA.R                           ###
###                                                                     ###
###########################################################################
###########################################################################

clean_economatica_data <- function(dirty_db){ # Economatica is the data provider
  funds_code <- str_split_i(colnames(dirty_db)[-1], '\n', -1)
  
  clean_db <- dirty_db %>% 
    set_names(c('date', funds_code)) %>% 
    select(!2) %>% # Drop empty column
    mutate(across(everything(), ~as.numeric(.x))) %>%  # Everything as numeric 
    mutate(date = as.Date(date, origin="1899-12-30")) %>% # Date as date
    pivot_longer(!date, names_to = 'fund_code', values_to = 'value') %>% 
    drop_na(value)
  
  return(clean_db)
}

join_data <- function(data_type_name, ten_funds = NULL){
  active_funds <- clean_economatica_data(read_excel(
    paste0('raw_data/', data_type_name, '_active.xlsx'),
    na = "-", skip = 3, col_types = 'text'
  ))
  
  canceled_funds <- clean_economatica_data(read_excel(
    paste0('raw_data/', data_type_name, '_canceled.xlsx'),
    na = "-", skip = 3, col_types = 'text'
  ))
  
  all_funds <- rbind(active_funds, canceled_funds)
  
  if(!is.null(ten_funds)){
    all_funds <- rbind(all_funds, ten_funds)
  }
  
  all_funds <- all_funds %>% 
    arrange(date) %>% 
    set_names(c('date', 'fund_code', data_type_name))
  
  return(all_funds)
}

data_quality_hyp1 <- function(nav_data){
  hypothesis_1 <- nav_data %>% 
    dplyr::filter(nav == 0) %>% 
    pull(fund_code)
  
  count_hyp1 <- 0
  for (i in seq_along(hypothesis_1)) {
    verify_hyp1 <- nav_data %>% 
      dplyr::filter(fund_code == hypothesis_1[i]) %>% 
      arrange(date) %>% 
      tail(1) %>% 
      pull(nav)
    
    if(verify_hyp1 != 0){
      warning(str_glue("Fund {hypothesis_1[i]} doesen't comply with hypothesis 1."))
    } else {
      count_hyp1 <- count_hyp1 + 1
    }
  }
  
  print(str_glue("Hypothesis 1: From {length(hypothesis_1)} funds, {count_hyp1} comply."))
}

############################################################################
############################################################################
###                                                                      ###
###                         PREPARE_MODEL_DATA.R                         ###
###                                                                      ###
############################################################################
############################################################################



