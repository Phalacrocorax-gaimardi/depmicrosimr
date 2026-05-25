#technicals
#bill_values <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/bills.csv")
#sD <- readxl::read_xlsx("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/scenario_parameters.xlsx",sheet="Base")


#' scenario_params
#'
#' builds the complete parameter set at yeartime from scenario sD
#'
#' @param sD scenario parameters e.g. scenario_0
#' @param yeartime decimal time
#'
#' @return long form dataframe containing parameter names and values
#' @export
#'
#' @examples
scenario_params <- function(sD,yeartime){
  #fast params
  scen <- tibble::tibble(parameter="yeartime", value=  yeartime)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="dep_introduction", value=  dplyr::filter(sD, parameter=="dep_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="tou_introduction", value=  dplyr::filter(sD, parameter=="tou_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bess_labour_cost", value=  bess_labour_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_cost", value=  pv_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_margin", value=  dplyr::filter(sD, parameter=="pv_margin")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_sbos", value=  dplyr::filter(sD, parameter=="pv_sbos")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_labour", value=  pv_labour_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_inverter", value=  pv_inverter_cost_fun(sD,yeartime)))
  # scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_install_cost", value=  pv_install_cost_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pvbess_cost_synergy",  value=dplyr::filter(sD, parameter=="pvbess_cost_synergy")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="overhead",  value=dplyr::filter(sD, parameter=="overhead")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="day_tariff", value =  day_tariff_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="evening_tariff", value =  evening_tariff_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_tariff", value =  night_tariff_fun(sD,yeartime)))
  #  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_price_inflation", value =  electricity_price_inflation_fun(sD,yeartime)))
  #  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit_inflation", value =  fit_inflation_fun(sD,yeartime)))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit_price_inflation", value =  0))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="standing_charge", value =  standing_charge_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit", value =  fit_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="fit_tax_threshold", value =  fit_tax_threshold_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="resilience_premium", value =  resilience_premium_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="marginal_tax_rate", value =  dplyr::filter(sD, parameter=="marginal_tax_rate")$value))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="finance_rate", value =  finance_rate_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="term_of_loan", value =  dplyr::filter(sD, parameter=="term_of_loan")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="discount_rate", value =  dplyr::filter(sD, parameter=="discount_rate")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="system_lifetime", value =  dplyr::filter(sD, parameter=="system_lifetime")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="e_demand_factor", value =  electricity_demand_factor_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_threshold", value =  dplyr::filter(sD, parameter=="sol_lower_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_upper_threshold", value =  dplyr::filter(sD, parameter=="sol_upper_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_grant", value =  dplyr::filter(sD, parameter=="sol_lower_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_upper_grant", value =  dplyr::filter(sD, parameter=="sol_upper_grant")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="sol_lower_threshold", value =  dplyr::filter(sD, parameter=="sol_lower_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="grant_introduction_date", value =  dplyr::filter(sD, parameter=="grant_introduction_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="pv_grant_removal_date", value =  dplyr::filter(sD, parameter=="pv_grant_removal_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bess_grant_removal_date", value =  dplyr::filter(sD, parameter=="bess_grant_removal_date")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bess_threshold", value =  dplyr::filter(sD, parameter=="bess_threshold")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="bess_grant", value =  dplyr::filter(sD, parameter=="bess_grant")$value))
  #scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="self_sufficiency_effect", value =  self_sufficiency_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="kWp_per_m2", value =  kWp_per_m2_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="usable_roof_fraction", value =  dplyr::filter(sD, parameter=="usable_roof_fraction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="mean_shading_factor", value =  dplyr::filter(sD, parameter=="mean_shading_factor")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="latitude", value =  dplyr::filter(sD, parameter=="latitude")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="K_max", value =  dplyr::filter(sD, parameter=="K_max")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="K_min", value =  dplyr::filter(sD, parameter=="K_min")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lag_D", value =  dplyr::filter(sD, parameter=="lag_D")$value))
  # scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rho_solstice", value =  dplyr::filter(sD, parameter=="rho_solstice")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="acceleration_factor", value =  acceleration_fun(sD,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="beta.", value =  dplyr::filter(sD, parameter=="beta.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="lambda.", value =  dplyr::filter(sD, parameter=="lambda.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="p.", value =  dplyr::filter(sD, parameter=="p.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="nu.", value =  dplyr::filter(sD, parameter=="nu.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rho.", value =  dplyr::filter(sD, parameter=="rho.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="delta.", value =  dplyr::filter(sD, parameter=="delta.")$value))


  #return(scen)
  return(scen %>% fast_params())
}




#' survey_bills_to_kwh
#'
#' a function that converts highest and lower bi-monthly bills from survey to daily D_max and D_min assuming
#' a seasonal demand lag_D. D_max and D_min are used to estimate the financial return on pv bess investment.
#' Missing data are imputed by default. An issue that needs to be addressed is "level pay"
#'
#' @param data_in survey data e.g. dep_survey
#' @param lag_D seasonal lag in demand, default 30 days.
#'
#' @returns
#' @export
#'
#' @examples
#' survey_bills_to_kwh(dep_survey,lag_D=30)
#'
survey_bills_to_kwh <- function(data_in, lag_D=30){
  #
  #impute missing bills
  data_in <- data_in %>% dplyr::select(serial,q14,q15,qi)
  complete_data <- data_in %>% dplyr::filter(q14!=13,q15 != 13)
  complete_data <- complete_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q15"=response_code,"lowest_bill"=bill))
  complete_data <- complete_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q14"=response_code,"highest_bill"=bill))
  complete_data <- complete_data %>% dplyr::mutate(dplyr::across(dplyr::everything(),as.numeric))
  # regression model relating high and low bills
  high_model <- nls(highest_bill ~ a * lowest_bill + exp(b), start = list(a = 2, b = 1),
                    algorithm = "port", lower = c(0, -Inf), upper = c(5, Inf),data=complete_data)
  low_model <- nls(lowest_bill ~ a * highest_bill + exp(b), start = list(a = 2, b = 1),
                   algorithm = "port", lower = c(0, -Inf), upper = c(5, Inf),data=complete_data)

  #q14 missing but not q15
  missing_high_data <- data_in %>% dplyr::filter(q14==13,q15 != 13)
  missing_high_data <- missing_high_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q15"=response_code,"lowest_bill"=bill))
  missing_high_data$lowest_bill <- as.numeric(missing_high_data$lowest_bill)
  coefs <- summary(high_model)$coefficients[, "Estimate"]
  std_errors <- summary(high_model)$coefficients[, "Std. Error"]
  missing_high_data$a <- rnorm(nrow(missing_high_data), mean=coefs[1],sd=std_errors[1])
  missing_high_data$b <- rnorm(nrow(missing_high_data), mean=coefs[2],sd=std_errors[2])
  missing_high_data <- missing_high_data %>% dplyr::mutate(highest_bill = a*lowest_bill + exp(b))
  #q15 missing but not q15
  missing_low_data <- data_in %>% dplyr::filter(q14!=13,q15 == 13)
  missing_low_data <- missing_low_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q14"=response_code,"highest_bill"=bill))
  missing_low_data$highest_bill <- as.numeric(missing_low_data$highest_bill)
  coefs <- summary(low_model)$coefficients[, "Estimate"]
  std_errors <- summary(low_model)$coefficients[, "Std. Error"]
  missing_low_data$a <- rnorm(nrow(missing_low_data), mean=coefs[1],sd=std_errors[1])
  missing_low_data$b <- rnorm(nrow(missing_low_data), mean=coefs[2],sd=std_errors[2])
  missing_low_data <- missing_low_data %>% dplyr::mutate(lowest_bill = a*highest_bill + exp(b))

  #########################################
  #both high and low missing model
  #generate q14 as lognormally distributed
  ###########################################
  missing_both_data <- data_in %>% dplyr::filter(q14==13,q15 == 13) #143 rows
  #model by household profile
  logparams <- complete_data %>% dplyr::group_by(qi) %>% dplyr::summarise(logmean=mean(log(highest_bill)),logsd=sd(log(highest_bill)))
  missing_both_data <- missing_both_data %>% dplyr::inner_join(logparams) %>% dplyr::rowwise() %>% dplyr::mutate(highest_bill=rlnorm(1,logmean,logsd))
  #missing_both_data$highest_bill <- rlnorm(nrow(missing_both_data),logmean,sdmean)
  coefs <- summary(low_model)$coefficients[, "Estimate"]
  std_errors <- summary(low_model)$coefficients[, "Std. Error"]
  missing_both_data$a <- rnorm(nrow(missing_both_data), mean=coefs[1],sd=std_errors[1])
  missing_both_data$b <- rnorm(nrow(missing_both_data), mean=coefs[2],sd=std_errors[2])
  missing_both_data <- missing_both_data %>% dplyr::mutate(lowest_bill = a*highest_bill + exp(b)) %>% dplyr::select(-logmean,-logsd)

  complete_data <- complete_data %>% dplyr::bind_rows(missing_low_data,missing_high_data,missing_both_data) %>% dplyr::select(-a,-b)
  complete_data <- complete_data %>% dplyr::arrange(serial)
  #flip bills of miscreants where highest_bill < lowest_bill
  complete_data <- complete_data %>% dplyr::mutate(temp_high= dplyr::if_else(lowest_bill > highest_bill, lowest_bill,highest_bill))
  complete_data <- complete_data %>% dplyr::mutate(temp_low = dplyr::if_else(lowest_bill > highest_bill, highest_bill,lowest_bill))
  complete_data <- complete_data %>% dplyr::select(-highest_bill,-lowest_bill) %>% dplyr::rename("highest_bill" = temp_high, "lowest_bill" = temp_low)

  e_price_2023 <- seai_elec %>% dplyr::filter(year==2023) %>% dplyr::pull(price)/100*1.15 #15% correction for credits
  complete_data <- complete_data %>% dplyr::mutate(lowest_kwh=lowest_bill/e_price_2023,highest_kwh=highest_bill/e_price_2023)

  complete_data <- complete_data %>% tidyr::drop_na() %>% dplyr::rowwise() %>% dplyr::mutate(params = list(get_demand_params(highest_kwh, lowest_kwh,lag_D))) %>%
    dplyr::ungroup() %>% tidyr::unnest_wider(params)
  #model annual demand
  complete_data <- complete_data %>% dplyr::rowwise() %>% dplyr::mutate(annual_kwh=sum(demand_fun(1:365,D_max,D_min)))

  complete_data %>% dplyr::arrange(serial) %>% return()
}

#' demand_fun
#'
#' A simple sinusoidal model of mean daily demand
#'
#' @param day day of year
#' @param D_max maxiumum demand in kWh
#' @param D_min minimum demand in kWh
#' @param lag_D lag (in days) default 30
#'
#' @return daily kWh demand
#' @export
#'
#' @examples
#'
#' sapply(1:365,function(d) demand_fun(d,14,11,30 ))
demand_fun <- function(day, D_max,D_min,lag_D=30){

  #the demand function peaks in winter months
  phase_D=lag_D/360*2*pi

  (D_max + D_min)/2 + (D_max-D_min)/2*cos(2*pi*day/365-phase_D) %>% return()
}

#' get_demand_params
#'
#' utility function used by
#'
#' @param highest_kwh highest kWh bi-monthly usage inferred from 2023 bills
#' @param lowest_kwh lowest kWh bi-monthly usage inferred from 2023 bills
#' @param lag_D demand seasonal lag default 30 days
#'
#' @returns
#' @export
#'
#' @examples
get_demand_params <- function(highest_kwh,lowest_kwh,lag_D = 30){

  #if(lowest_kwh > highest_kwh) stop("lowest kWh is greater than highest kWh")
  max_kwh <- function(D) sum(demand_fun(1:61,D_max=D[1],D_min=D[2],lag_D)) #winter
  min_kwh <- function(D) sum(demand_fun(183:243,D_max=D[1],D_min=D[2],lag_D)) #summer
  obj_fun <- function(D)(lowest_kwh-min_kwh(D))^2 + (highest_kwh-max_kwh(D))^2
  #optim(c(10,10), obj_fun,method="L-BFGS-B", lower=c(0,0),upper=c(Inf,Inf))
  solution <- nloptr::nloptr(c(20,20),obj_fun, lb=c(0,0), ub=c(100,100),opts=list(algorithm="NLOPT_LN_BOBYQA",maxeval=1000))$solution
  names(solution) <- c("D_max","D_min")
  solution %>% return()
}



#' day_tariff_fun
#'
#' all tou tariffs are driven by the flat rate.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
#' flat_tariff_fun(sD,2030)
flat_tariff_fun <- function(sD,yeartime){
  #
  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known
  #cost_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_2022") %>% dplyr::pull(value)
  values <- sD %>% dplyr::filter(parameter %in% c("flat_tariff_2030","flat_tariff_2040")) %>% dplyr::pull(value)
  approx(x=c(seai_elec1$year+0.5,2030.5,2040.5), y=c(seai_elec1$price,values),xout=yeartime,rule=2)$y %>% return()
}



#' day_tariff_fun
#'
#' actual (currenly to mid 2023) and projected path of electricity prices. Data from seai_elec. For inflation expectations see electricity_price_inflation_fun.
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
#' day_tariff_fun(sD,2030)
day_tariff_fun <- function(sD,yeartime){

  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known
  #cost_2022 <- sD %>% dplyr::filter(parameter=="electricity_price_2022") %>% dplyr::pull(value)
  ratios <- sD %>% dplyr::filter(parameter %in% c("day_flat_ratio","day_flat_ratio_2030","day_flat_ratio_2040")) %>% dplyr::pull(value)
  values <- values <- sD %>% dplyr::filter(parameter %in% c("flat_tariff_2030","flat_tariff_2040")) %>% dplyr::pull(value)
  approx(x=c(seai_elec1$year+0.5,2030.5,2050.5), y=c(seai_elec1$price/100*ratios[1],values*ratios[2:3]),xout=yeartime,rule=2)$y %>% return()
}



#' night_tariff_fun
#'
#' Night ToU tariffs. Historical rates are set at 45% of the day rate
#'
#' @param sD scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
night_discount_fun <- function(sD,yeartime){
  #
  seai_elec1 <- seai_elec %>% dplyr::filter(year >=2008) #add more costs here if known
  ratios <- sD %>% dplyr::filter(parameter %in% c("night_flat_ratio","night_flat_ratio_2030","night_flat_ratio_2040")) %>% dplyr::pull(value)
  values <- values <- sD %>% dplyr::filter(parameter %in% c("flat_tariff_2030","flat_tariff_2040")) %>% dplyr::pull(value)
  approx(x=c(seai_elec1$year+0.5,2030.5,2050.5), y=c(seai_elec1$price/100*ratios[1],values*ratios[2:3]),xout=yeartime,rule=2)$y %>% return()
}


#' decompose_logprices
#'
#' Seasonal and Trend decomposition using Loess (STL) decomposition of historical wholesale prices.
#' \cr
#' A log-type transformation \deqn{y=\operatorname{arcsinh}(\frac{x}{scale})}
#' This is referred to as the "logprice", although it is linear for small x to handle negative prices
#' \cr
#' The seasonal decomposition is in to periodic daily, weekly and aperiodic yearly terms.
#' decompose_logprices() fills any gaps in the input data to form an hourly time series.
#'
#' @param price_data price data in format datetime,price e.g. sem_prices_2019_2025
#' @param scale scale used in asinh transformation
#'
#' @returns dataframe with STL decomposition of hourly price data
#' @export
#'
#' @examples
#'
#' decompose_logprices(sem_prices_2019_2025,scale=10)

decompose_logprices <- function(price_data,scale=10){

  hourly <- price_data %>% tsibble::as_tsibble(index = datetime) #%>% mutate(logprice=log(price+10))
  hourly <- hourly %>% tsibble::fill_gaps() #replaces with NAs
  hourly <- hourly %>% dplyr::mutate(price = imputeTS::na_seasplit(price, algorithm = "interpolation", find_frequency = TRUE))

  stopifnot(!tsibble::has_gaps(hourly))
  # scale has to be set
  #scale <- sd(hourly$price)/2
  hourly$logprice <- asinh(hourly$price/scale)
  dcmp <- hourly %>% fabletools::model(feasts::STL(logprice ~ trend(window = 2001)
                                           # + season(period = "1 day",window=30*24+1)
                                           + season(period = "1 day",window="periodic")
                                           + season(period = "1 week",window="periodic")
                                           + season(period=8766,window="periodic")
                                           ,robust=TRUE)) %>% fabletools::components()
  dcmp <- dcmp %>% dplyr::rename("season_week"=`season_1 week`) %>% dplyr::rename("season_year"=season_8766) %>% dplyr::rename("season_day"=`season_1 day`)
  dcmp <- dcmp %>% dplyr::select(-season_adjust,-.model)

  dcmp_clean <- dcmp %>%
    tibble::as_tibble() %>% # temporarily drop tsibble to do vector math safely
    dplyr::mutate(
      # 1. Smooth the spiky annual cycle using a rolling 2-month window (61 * 24 hours)
      # k = 337 is an odd number roughly equal to 2 weeks of hourly data
      season_year_smooth = zoo::rollmean(season_year, k = 1465, fill = "extend", align = "center"),

      # 2. Calculate the "hedge needles" that were trapped there
      annual_leakage = season_year - season_year_smooth,

      # 3. Add that leakage straight into your existing remainder
      remainder = remainder + annual_leakage
    )
  dcmp_clean <- dcmp_clean %>% dplyr::mutate(season_year=season_year_smooth) %>% dplyr::select(-season_year_smooth)
  dcmp_clean %>% return()
}

#' generate_logprice_hmm
#'
#' creates an n state hidden Markov model based on residuals extracted from decompose_logprices. The default is three states,
#' supposed to represent high RES/low demand, normal RES/demand and low RES/high demand regimes.\cr
#' \cr
#' generate_logprice_hmm uses a gaussian model at present. This should be replaced by fat-tailed distribution e.g. student-t in future.
#'
#' @param dcmp decomposed logprices from decompose_logprices()
#' @param n_states number of Markov states, default 3
#'
#' @returns
#' @export
#'
#' @examples
#'
generate_logprice_hmm <- function(dcmp,n_states=3){
  #
  model <- depmixS4::depmix(remainder ~ 1, nstates = n_states, data = data.frame(dcmp))
  fit_model <- fit(model)
  summary(fit_model)
  print(paste("BIC score", n_states, "states",BIC(fit_model)))
  return(fit_model)
}


#' simulate_prices
#'
#' Generates an hourly wholesale price simulation from Jan 1 2026 to31 Dec 2040.\cr
#' \cr
#' The projections derive from the product of three factors - trend, seasonal, and gaussian HMM components. Thus
#' projections reflect heteroskedasticity of
#'
#' @param dcmp decomposed logprice output from decompose_logprices()
#' @param fit_hmm the HMM fit to residuals from generate_logprice_hmm()
#' @param scale the asinh() price transformation scale, default 10euros/MWh
#' @param end_year the last year of the price simulation
#' @param trend_price_2030 the trend SEM price at the end of 2030
#' @param trend_price_2040 the trend SEM price at the end of 2040
#' @param drop_hmm option to drop the residuals
#' @param drop_season option to drop the seasonal components
#'
#' @returns
#' @export
#'
#' @examples
#'
#'
simulate_prices <- function(dcmp,fit_hmm,scale=10, end_year=2040, trend_price_2030,trend_price_2040, drop_hmm=FALSE,drop_season=FALSE){
  #
  t1 <- lubridate::ymd_hms("2026-01-01 00:00:00", tz = "UTC")
  t2 <- lubridate::ymd_hms(paste(end_year,"-12-31 23:00:00", tz = "UTC", sep=""))
  # 2. Generate the hourly equence using base R's seq() with lubridate's hours(1)
  hourly_sequence <- seq(from = t1, to = t2, by = "1 hour")

  sim_length <- length(hourly_sequence)
  dummy_df <- data.frame(detrended = rep(0, sim_length))

  #new model framework
  n_states <- fit_hmm@nstates
  extended_model <- depmixS4::depmix(detrended ~ 1, nstates = n_states, data = dummy_df)
  #copy the fitted parameters
  extended_model <- depmixS4::setpars(extended_model, depmixS4::getpars(fit_hmm))
  #
  sim <- depmixS4::simulate(extended_model, nsim = 1)
  # Extract your new long series
  sim_series <- sim@response[[1]][[1]]@y[,1]
  #sim_states <- sim@states
  # sim_data now contains:
  # 1. The sequence of "Hidden States" (State 1, 2, or 3)
  # 2. The "Observed" synthetic prices
  sim_logprices <-  tibble::tibble(datetime=hourly_sequence,sim=sim_series)
  #seasonal factors
  daily_lookup <- dcmp %>% tibble::as_tibble() %>%
    # Identify unique hour of the week (1 to 24)
    dplyr::mutate(hour_of_day = lubridate::hour(datetime)+1)  %>%
    dplyr::group_by(hour_of_day) %>%
    dplyr::summarise(season_daily = dplyr::first(season_day), .groups = "drop")


  weekly_lookup <- dcmp %>% tibble::as_tibble() %>%
    # Identify unique hour of the week (1 to 168)
    dplyr::mutate(hour_of_week = (lubridate::wday(datetime) - 1) * 24 + lubridate::hour(datetime) + 1) %>%
    dplyr::group_by(hour_of_week) %>%
    dplyr::summarise(season_weekly = dplyr::first(season_week), .groups = "drop")

  annual_lookup <- dcmp %>%
    tibble::as_tibble() %>%
    # Identify unique hour of the year (approx 1 to 8766)
    dplyr::mutate(
      # Use yday * 24 + hour to track the exact solar timeline position
      hour_of_year = (lubridate::yday(datetime) - 1) * 24 + lubridate::hour(datetime) + 1
    ) %>%
    dplyr::group_by(hour_of_year) %>%
    dplyr::summarise(season_annual = dplyr::first(season_year), .groups = "drop")
  #
  seasonal_logprices <- tibble::tibble(datetime=hourly_sequence) %>%
    # Calculate index keys for the future timestamps
    dplyr::mutate(
      hour_of_day = lubridate::hour(datetime)+1,
      hour_of_week = (lubridate::wday(datetime) - 1) * 24 + lubridate::hour(datetime) + 1,
      hour_of_year = (lubridate::yday(datetime) - 1) * 24 + lubridate::hour(datetime) + 1
    ) %>%
    # Left join the master lookup profiles
    dplyr::left_join(daily_lookup, by = "hour_of_day") %>%
    dplyr::left_join(weekly_lookup, by = "hour_of_week") %>%
    dplyr::left_join(annual_lookup, by = "hour_of_year") %>%
    # If a leap year creates an unmapped hour_of_year = 8784,
    # use tidyr::fill() or safely fallback to the closest winter profile
    tidyr::fill(season_annual, .direction = "down") %>%
    # Combine them into your total additive seasonal adjustment
    dplyr::mutate(
      season = season_weekly + season_annual + season_daily
    ) %>% dplyr::select(datetime, season)
  #price trend
  trend_logprices <- tibble::tibble(datetime=hourly_sequence,trend=NA)
  trend_price_2026 <- dcmp %>% dplyr::filter(datetime=="2025-12-01 23:00:00") %>% dplyr::pull(trend)
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=replace(trend, datetime=="2026-01-01 00:00:00",trend_price_2026))
  #wholesale price 100
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=replace(trend, datetime=="2030-12-31 23:00:00",asinh(trend_price_2030/scale)))
  #wholesale price 200
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=replace(trend, datetime=="2040-12-31 23:00:00",asinh(trend_price_2040/scale)))
  #linearly interp
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=zoo::na.approx(trend))

  sim_logprices <- sim_logprices %>% dplyr::inner_join(trend_logprices) %>% dplyr::inner_join(seasonal_logprices)

  if(!drop_hmm & !drop_season) sim_prices <- sim_logprices %>% dplyr::mutate(price=scale*sinh(sim+trend+season)) %>% dplyr::select(datetime,price)
  if(drop_hmm & !drop_season) sim_prices <- sim_logprices %>% dplyr::mutate(price=scale*sinh(trend+season)) %>% dplyr::select(datetime,price)
  if(!drop_hmm & drop_season) sim_prices <- sim_logprices %>% dplyr::mutate(price=scale*sinh(trend+sim)) %>% dplyr::select(datetime,price)
  if(drop_hmm & drop_season) sim_prices <- sim_logprices %>% dplyr::mutate(price=scale*sinh(trend)) %>% dplyr::select(datetime,price)

  sim_prices$regime <- "simulated"
  hist <- dcmp %>% dplyr::select(datetime,logprice) %>% dplyr::mutate(regime="historical")
  hist <- hist %>% dplyr::mutate(price= scale*sinh(logprice)) %>% dplyr::select(-logprice)
  hist %>% dplyr::bind_rows(sim_prices)


}
