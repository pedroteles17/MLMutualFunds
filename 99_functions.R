
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

winsorize <- function (x, fraction = .01){
  lim <- quantile(x, probs=c(fraction, 1-fraction), na.rm = TRUE)
  
  x[ x < lim[1]] <- lim[1]
  x[ x > lim[2]] <- lim[2]
  
  return(x)
}

#################################################################
##                    Select Eligible Funds                    ##
#################################################################

select_eligible_funds <- function(nav_df, registration_df, nefin_df, end_date, n_months_ago, min_avg_aum, max_pct_missing_nav){
  start_date <- end_date %m-% months(n_months_ago)
  
  nefin_dates <- nefin_df %>% 
    arrange(date) %>% 
    dplyr::filter(
      date >= start_date & date < end_date
    ) %>% 
    pull(date)
  
  nav_period <- nav_df %>% 
    arrange(date) %>% 
    dplyr::filter(
      date >= start_date & date < end_date
    )
  
  nav_period_nefin_dates <- nav_period %>% 
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
    restriction_two, nav_period_nefin_dates, max_pct_missing_nav
  )
  
  # Fourth Restriction
  restriction_four <- .remove_master_funds(
    restriction_three, registration_df
  )
  
  # Fifth Restriction
  restriction_five <- .only_one_feeder(
    restriction_four, registration_df, nav_period_nefin_dates
  )
  
  all_restrictions <- list(
    restriction_one, restriction_two, 
    restriction_three, restriction_four, 
    restriction_five
  )
  
  eligible_data <- list(
    eligible_assets = restriction_five,
    n_assets = sapply(all_restrictions, length)
  )
  
  return(eligible_data)
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
.maximum_percentage_missing_nav <- function(fund_univ, nav_period_df, maximum_pct_missing){
  selected_funds <- nav_period_df %>%
    dplyr::filter(fund_code %in% fund_univ) %>% 
    complete(fund_code, date) %>% 
    group_by(fund_code) %>% 
    summarise(
      pct_missing = sum(is.na(nav_return)) / n()
    ) %>% 
    dplyr::filter(pct_missing < maximum_pct_missing) %>% 
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
    arrange(date) %>% 
    dplyr::select(!date) %>% 
    mutate_all(~replace(., is.na(.), 0))
  
  corr_nav <- cor(nav_funds)
  corr_nav[lower.tri(corr_nav, diag = TRUE) | corr_nav < max_corr] <- NA
  
  # If all correlations are below the max_corr, we dont remove any fund
  if(all(is.na(corr_nav))){
    return(rep(FALSE, length(funds_codes)))
  }
  
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

# Given a correlation data frame, we will choose only one fund among those with high correlation with each other.
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
      arrange(date) %>% 
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

#################################################################
##                     Feature Engineering                     ##
#################################################################

generate_features <- function(eligible_funds, nav_df, nefin_df, end_estimation_date, n_months_ago){
  start_estimation_date <- end_estimation_date %m-% months(n_months_ago)
  
  nav_period <- nav_df %>% 
    dplyr::filter(
      date >= start_estimation_date & date < end_estimation_date & fund_code %in% eligible_funds
    )
  
  nav_nefin <- .merge_nav_nefin(
    nav_period, nefin_df, start_estimation_date, end_estimation_date
  ) %>%
    mutate(
      nav_return = replace_na(nav_return, 0)
    )
  
  # NAV Features
  nav_features <- nav_nefin %>%
    group_by(fund_code) %>% 
    group_map(
      ~data.frame(fund_code = .y, .join_nav_features(.))
    ) %>% 
    bind_rows()
  
  # AUM, Inflow, Outflow, Number of Shareholders, % Flow
  other_features <- nav_period %>% 
    group_by(fund_code) %>% 
    summarise(
      avg_aum = mean(aum, na.rm = TRUE),
      initial_aum = aum[!is.na(aum)][1],
      inflow = sum(inflow, na.rm = TRUE),
      outflow = sum(outflow, na.rm = TRUE),
      number_shareholders = rev(number_shareholders[!is.na(number_shareholders)])[1]
    ) %>% 
    ungroup() %>% 
    mutate(
      pct_flow = (inflow - outflow) / initial_aum
    ) %>% 
    dplyr::select(!initial_aum)
  
  all_features <- merge(nav_features, other_features, by = 'fund_code', all = TRUE) %>% 
    mutate(
      date = end_estimation_date, .before = 1
    )
  
  return(all_features)
  
}

.join_nav_features <- function(nav_nefin) {
  
  returns_xts <- xts(nav_nefin[,-1], nav_nefin$date)
  
  # Short term momentum (Lagged one-month abnormal return)
  f_r2_1_info <- .calculate_nav_features(
    first(last(returns_xts, '2 months'), '1 month')
  ) 
  colnames(f_r2_1_info) <- paste0('f_r2_1_', colnames(f_r2_1_info))
  
  # Momentum (Mean abnormal return from past 12 months)
  f_r12_2_info <- .calculate_nav_features(
    first(last(returns_xts, '12 months'), '10 month')
  )
  colnames(f_r12_2_info) <- paste0('f_r12_2_', colnames(f_r12_2_info))
  
  # Short-term reversal (Prior month abnormal return)
  f_st_rev_info <- .calculate_nav_features(
    last(returns_xts, '1 month')
  )
  colnames(f_st_rev_info) <- paste0('f_st_rev_', colnames(f_st_rev_info))
  
  return_features <- cbind(
    cbind(f_r2_1_info, f_r12_2_info), f_st_rev_info
  )
  
  return(return_features)
}

.calculate_nav_features <- function(fund_nefin_xts){
  
  # Carhart (1997) regression information
  regres_info <- .get_regression_info(fund_nefin_xts, tstat = TRUE)
  
  # Kurtosis 
  fund_kurtosis <- suppressMessages(kurtosis(fund_nefin_xts$nav_return))  
  
  fund_minus_market <- fund_nefin_xts$nav_return - fund_nefin_xts$Market
  
  # Israelsen Information Ratio (ttps://doi.org/10.1057/palgrave.jam.2240158)
  mir <- .israelsen_information_ratio(fund_minus_market) # Modified IR
  
  # CVaR
  cvar <- suppressMessages(CVaR(fund_nefin_xts$nav_return))
  colnames(cvar) <- 'cvar'
  
  # Tracking Error
  track_error <- sd(fund_minus_market)
  
  stats <- data.frame(mir, cvar, track_error, fund_kurtosis)
  
  return(cbind(stats, regres_info))
}

.israelsen_information_ratio <- function(fund_minus_market){
  ER <- mean(fund_minus_market)
  SD <- sd(fund_minus_market)
  mir <- ER / (SD^(ER / abs(ER))) # Modified IR
  
  return(mir)
}

.get_regression_info <- function(fund_nefin, tstat = FALSE) {
  
  if(tstat){
    summary_column_i <- 3
  } else {
    summary_column_i <- 1
  }
  
  # Carhart (1997) regression
  regres <- lm(
    I(nav_return - Risk_free) ~ Rm_minus_Rf + SMB + HML + WML, data = fund_nefin
  )
  
  # Get regression coefficients
  regres_coef <- summary(regres)$coefficients
  
  alpha <- regres_coef[1, summary_column_i]
  market <- regres_coef[2, summary_column_i]
  size <- regres_coef[3, summary_column_i]
  value <- regres_coef[4, summary_column_i]
  momentum <- regres_coef[5, summary_column_i]
  
  ## R^2
  r_squared <- summary(regres)$r.squared
  
  ## Idiosyncratic Volatility
  ivol <- sd(regres$residuals)
  
  regres_info <- data.frame(
    alpha, market, size, value,
    momentum, r_squared, ivol
  )
  
  return(regres_info)
  
}

.merge_nav_nefin <- function(nav_df, nefin_df, start_date, end_date){
  nefin_period <- nefin_df %>% 
    dplyr::filter(
      date >= start_date & date < end_date
    ) %>% 
    arrange(date)
  
  nav_nefin <- nav_df %>% 
    dplyr::select(
      date, fund_code, nav_return
    ) %>%
    complete(date, fund_code)  %>%
    right_join(nefin_period, by = 'date')
  
  return(nav_nefin)
}

##################################################################
##                      Dependent Variable                      ##
##################################################################

generate_dependent_variable <- function(eligible_funds, nav_df, nefin_df, base_date, n_months_ago, n_months_forward) {
  start_estimation_date <- base_date %m-% months(n_months_ago)
  end_evaluation_date <- base_date %m+% months(n_months_forward)
  
  nav_period <- nav_df %>% 
    dplyr::filter(
      fund_code %in% eligible_funds
    ) 
  
  nav_nefin_full_period <- .merge_nav_nefin(
    nav_period, nefin_df, start_estimation_date, end_evaluation_date
  ) %>%
    mutate(
      nav_return = replace_na(nav_return, 0)
    )
  
  # Estimation Period
  nav_nefin_estimation_period <- nav_nefin_full_period %>% 
    dplyr::filter(
      date >= start_estimation_date & date < base_date
    )
  
  regression_coefficients <- .get_regression_coefficients(
    nav_nefin_estimation_period
  )
    
  
  # Evaluation Period
  nav_nefin_evaluation_period <- nav_nefin_full_period %>% 
    dplyr::filter(
      date >= base_date & date < end_evaluation_date
    )
  
  nav_nefin_evaluation_returns <- .return_by_asset(
    nav_nefin_evaluation_period
  )
  
  # Calculate Abnormal Returns
  ## R^{abn} = R^{actual} - (Risk Free^{actual} + Beta^{past} * Factors^{actual}) 
  n_days_evaluation_period <- length(unique(nav_nefin_evaluation_period$date))
  
  abnormal_return <- .calculate_abnormal_return(
    nav_nefin_evaluation_returns, regression_coefficients
  )
  
  abnormal_return <- abnormal_return %>% 
    mutate(date = base_date, .before = 1)
  
  return(abnormal_return)
}

.get_regression_coefficients <- function(nav_nefin){
  regression_coefficients <- nav_nefin %>%
    group_by(fund_code) %>% 
    group_map(
      ~data.frame(fund_code = .y, .get_regression_info(.))
    ) %>% 
    bind_rows() %>% 
    dplyr::select(
      fund_code, market, 
      value, size, momentum
    ) %>% 
    pivot_longer(
      !fund_code, names_to = 'assets', values_to = 'coefficient'
    )
  
  return(regression_coefficients)
}

.return_by_asset <- function(nav_nefin){
  assets_returns <- nav_nefin %>% 
    group_by(fund_code) %>% 
    summarise(
      fund = prod(1 + nav_return) - 1,
      risk_free = prod(1 + Risk_free) - 1,
      market = prod(1 + Rm_minus_Rf) - 1,
      value = prod(1 + HML) - 1,
      size = prod(1 + SMB) - 1,
      momentum = prod(1 + WML) - 1,
    ) %>% 
    pivot_longer(
      !fund_code, names_to = 'assets', values_to = 'return'
    )
  
  return(assets_returns)
}

.calculate_abnormal_return <- function(nav_nefin_returns, regression_coefficients){
  abnormal_return <- nav_nefin_returns %>% 
    full_join(regression_coefficients, by = c('fund_code', 'assets')) %>% 
    mutate(
      coefficient = replace_na(coefficient, 1),
      return = ifelse(assets != 'fund', -return, return)
    ) %>% 
    group_by(fund_code) %>% 
    summarise(
      abnormal_return = sum(coefficient * return)
    )
}

###########################################################################
###########################################################################
###                                                                     ###
###                          BUILD_PORTFOLIO.R                          ###
###                                                                     ###
###########################################################################
###########################################################################

calculate_portfolio_retuns <- function(nav_df, funds_codes, start_date, n_months_ahead){
  end_date <- start_date %m+% months(n_months_ahead)
  
  nav_period <- nav_df %>% 
    dplyr::select(date, fund_code, nav_return) %>%
    dplyr::filter(
      fund_code %in% funds_codes
    ) %>% 
    complete(fund_code, date) %>% 
    dplyr::filter(
      date >= start_date & date < end_date
    ) %>% 
    pivot_wider(names_from = 'fund_code', values_from = 'nav_return') %>% 
    mutate(across(
      !date, ~replace_na(.x, 0)
    ))
  
  missing_data <- setdiff(funds_codes, colnames(nav_period))
  
  if(length(missing_data) != 0){
    print(str_glue('{start_date}: {missing_data}'))
  }
  
  nav_period_xts <- xts(nav_period[,-1], nav_period$date)
  
  portfolio_return <- Return.portfolio(
    nav_period_xts, 
    rep(1 / ncol(nav_period_xts), ncol(nav_period_xts))
  )
  
  portfolio_return <- data.frame(
    date = index(portfolio_return),
    portfolio_return, 
    row.names = NULL
  )
  
  return(portfolio_return)
}

calculate_long_short_returns <- function(tercil_return_df, nefin_df){
  portfolio_return <- tercil_return_df %>%
    pivot_wider(
      names_from = 'pred_tercil', values_from = 'portfolio.returns'
    ) %>% 
    left_join(
      select(nefin_df, date, Risk_free), 
      by = 'date'
    )
  
  if(nrow(portfolio_return) == 0){
    return(
      select(tercil_return_df, date, portfolio.returns)
    )
  }
  
  portfolio_return_xts <- xts(portfolio_return[,-1], portfolio_return$date)
  
  if(!all(colnames(long_short_return) == c('date', '1', '3', 'Risk_free'))){
    print(str_glue('Error at: {tercil_return_df$date[0]}'))
  }
  
  long_short <- Return.portfolio(
    portfolio_return_xts, weights = c(1, -1, 1), rebalance_on = 'months'
  )
  
  long_short <- data.frame(
    date = index(long_short), 
    long_short,
    row.names = NULL
  )
  
  return(long_short)
  
}

performance_summary <- function(portfolio_returns, nefin, column_name) {
  
  market_return <- nefin$Rm_minus_Rf + nefin$Risk_free
  
  # CAPM
  regres <- lm(I(portfolio_returns - Risk_free) ~ Rm_minus_Rf + SMB + HML + WML, data = nefin)
  # Get regression coefficients
  regres_coef <- summary(regres)$coefficients
  
  alpha <- (regres_coef[1, 1] + 1) ^ 252 - 1
  t_alpha <- regres_coef[1, 3]
  
  beta <- regres_coef[2, 1]
  
  cumulative_ret <- prod(portfolio_returns + 1)^(252 / length(portfolio_returns)) - 1
  # Portfolio volatility
  vol <- sd(portfolio_returns) * sqrt(252)
  
  # Israelsen Information Ratio (ttps://doi.org/10.1057/palgrave.jam.2240158)
  ER <- (1 + mean(portfolio_returns - market_return)) ^ 252 - 1
  SD <- sd(portfolio_returns - market_return) * sqrt(252)
  mir <- ER / (SD^(ER / abs(ER))) # Modified IR
  
  # Israelsen Sharpe Ratio (https://doi.org/10.1057/palgrave.jam.2240158)
  ER <- (1 + mean(portfolio_returns - nefin$Risk_free)) ^ 252 - 1
  SD <- sd(portfolio_returns - nefin$Risk_free) * sqrt(252)
  msr <- ER / (SD^(ER / abs(ER))) # Modified SR
  
  # Tracking Error
  track_error <- sd(portfolio_returns - market_return) * sqrt(252)
  
  # CVaR
  cvar <- as.numeric(CVaR(portfolio_returns))
  
  # Maximum Drawdown
  ## WARNING: Warning message because we are working with a vector instead of xts
  suppressWarnings(max_draw <- maxDrawdown(portfolio_returns))
  
  results <- data.frame(c(
    cumulative_ret, vol, 
    alpha, t_alpha, beta, 
    mir, msr, track_error,
    cvar, max_draw
  )) %>% 
    set_names(str_glue('Decile {column_name}'))
  
  rownames(results) <- c(
    "Annual. Return",
    "Std. Deviation", 
    "Alpha", "t(alpha)",
    "Beta", "Info. Ratio", 
    "Sharpe Ratio", "Track Error",
    "CVaR", "Max. Drawdown"
  )
  
  results[c(1, 2, 3, 8, 9, 10), 1] <- round(results[c(1, 2, 3, 8, 9, 10), 1]*100, 2)
  results[c(4, 5, 6, 7), 1] <- round(results[c(4, 5, 6, 7), 1], 2)
  
  return(results)
}


