
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

#################################################################
##                    Select Elegible Funds                    ##
#################################################################

select_elegible_funds <- function(nav_df, registration_df, start_date, end_date, min_avg_aum, max_pct_missing_nav){
  nefin_dates <- nefin %>% 
    arrange(date) %>% 
    dplyr::filter(date >= start_date & date <= end_date) %>% 
    pull(date)
  
  nav_period <- nav_df %>% 
    arrange(date) %>% 
    dplyr::filter(date %in% nefin_dates)
  
  # First restriction
  restriction_one <- .inception_closing_dates(
    unique(registration_df$fund_code), registration_df, start_date, end_date
  )
  
  # Second restriction
  restriction_two <- .minimum_avg_aum(
    restriction_one, nav_period, min_avg_aum # Thousand Reais
  )
  
  # Third Restriction
  restriction_three <- .maximum_percentage_missing_nav(
    restriction_two, nav_period, nefin_dates, max_pct_missing_nav
  )
  
  # Fourth Restriction
  restriction_four <- .remove_master_funds(
    restriction_three, registration_df
  )
  
  # Fifth Restriction
  restriction_five <- .only_one_feeder(
    restriction_four, registration_df, nav_period
  )
  
  all_restrictions <- list(
    restriction_one, restriction_two, 
    restriction_three, restriction_four, 
    restriction_five
  )
  
  elegible_data <- list(
    elegible_assets = restriction_five,
    n_assets = sapply(all_restrictions, length)
  )
  
  return(restriction_five)
}

# Choose funds that were present both before the start date and at (or after) the end date.
.inception_closing_dates <- function(fund_univ, registration_df, start_date, end_date){
  selected_funds <- registration_df %>%
    dplyr::filter(fund_code %in% fund_univ) %>% 
    dplyr::filter(
      inception_date < start_date & (closing_date > end_date | is.na(closing_date))
    ) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  return(selected_funds)
}

# Select funds with a minimum average AUM requirement.
.minimum_avg_aum <- function(fund_univ, nav_period_df, minimum_aum){
  selected_funds <- nav_period_df %>% 
    dplyr::filter(fund_code %in% fund_univ) %>% 
    group_by(fund_code) %>% 
    summarise(avg_aum = mean(aum, na.rm = TRUE)) %>% 
    dplyr::filter(avg_aum > minimum_aum) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  return(selected_funds)
}

# Select funds with a maximum percentage of missing values
.maximum_percentage_missing_nav <- function(fund_univ, nav_period_df, nefin_dates, maximum_pct_missing){
  selected_funds <- nav_period_df %>%
    dplyr::filter(fund_code %in% fund_univ) %>% 
    drop_na(nav_return) %>% 
    group_by(fund_code) %>% 
    summarise(pct_not_missing = n() / length(nefin_dates)) %>% 
    dplyr::filter(pct_not_missing > (1 - maximum_pct_missing)) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  return(selected_funds)
}

# Remove master funds (master-feeder structure), which only receive investments from other funds.
.remove_master_funds <- function(fund_univ, registration_df){
  selected_funds <- registration_df %>% 
    dplyr::filter(fund_code %in% fund_univ) %>%
    dplyr::filter(fund_type %ni% c('FF', 'FM')) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  return(selected_funds)
}

# Most complex restriction. 
## If an asset manager has multiple funds and some of them have a correlation
## above 0.99, we will choose the fund with the largest AUM and remove the others.
## In our filtering, a correlation bigger than 0.99 means that they share the same master fund.
.only_one_feeder <- function(fund_univ, registration_df, nav_period_df){
  selected_funds <- registration_df %>% 
    dplyr::filter(fund_code %in% fund_univ) %>%
    group_by(asset_manager) %>% 
    mutate(
      removed = .remove_high_corr(fund_code, nav_period_df, max_corr = 0.99)
    ) %>% 
    dplyr::filter(!removed) %>% 
    distinct(fund_code) %>% 
    pull(fund_code)
  
  return(selected_funds)
}

.remove_high_corr <- function(funds_codes, nav_period_df, max_corr){
  # Edge case: If an asset manager has only one fund, we will inform that we won't remove it
  if(length(funds_codes) == 1){
    return(FALSE)
  }
  
  nav_funds <- nav_period_df %>% 
    dplyr::filter(fund_code %in% funds_codes) %>% 
    pivot_wider(
      id_cols = c('date'), names_from = 'fund_code', values_from = 'nav_return'
    ) %>% 
    dplyr::select(!date) %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  corr_nav <- cor(nav_funds)
  corr_nav[lower.tri(corr_nav, diag = TRUE) | corr_nav < max_corr] <- NA
  
  corr_nav <- corr_nav %>%
    as.data.frame() %>%
    rownames_to_column(var = 'fund1') %>% 
    pivot_longer(
      !fund1, names_to = 'fund2', values_to = 'correlation'
    ) %>% 
    drop_na(correlation) %>% 
    dplyr::select(!correlation)
  
  removed_funds <- .remove_high_corr_low_aum(corr_nav, nav_period_df)
  
  removed_funds_logical <- funds_codes %in% removed_funds
  
  return(removed_funds_logical)
}

# Given a correlation dataframe, we will choose only one fund among those with high correlation with each other.
## As there may be multiple groups with high correlation, we may return more than one fund.
## Example:
### A, B and C have high correlation with each other
### D, E and, F have high correlation with each other
### However, A, B and C may be low correlated with D, E and F
### Meaning we may chose A and F, for example (remove B, C, D, E)
.remove_high_corr_low_aum <- function(corr_df, nav_period_df){
  removed_funds <- vector()
  while (nrow(corr_df) > 0) {
    # Get the two competing funds
    two_funds <- unlist(corr_df[1, c(1,2)]) 
    
    # Select the one with lowest AUM (to be removed)
    fund_to_be_removed <- nav_period_df %>% 
      dplyr::filter(fund_code %in% two_funds) %>% 
      group_by(fund_code) %>% 
      summarise(avg_aum = mean(aum, na.rm = TRUE)) %>% 
      slice_min(avg_aum, with_ties = FALSE) %>% 
      pull(fund_code)
    
    # Add the removed fund to the removed vector
    removed_funds <- c(removed_funds, fund_to_be_removed)
    
    # Remove all entries where this fund appear
    corr_df <- corr_df %>% 
      dplyr::filter(
        fund1 != fund_to_be_removed & fund2 != fund_to_be_removed
      )
    
  }
  
  return(removed_funds)
}

