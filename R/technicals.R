####################
#technicals
####################

#bill_values <- read_csv("~/Policy/CAMG/SolarPVReport/PVBESS_calibrater/bills.csv")
#sD <- readxl::read_xlsx("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/scenario_parameters.xlsx",sheet="Base")
#struct_model <- readr::read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/dep_struct_model.csv")
#smart_meter_rollout <- readr::read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/smart_meter_rollout.csv")
#tou_tariffs <- readr::read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/tariffs.csv")
#diurnal_inflex <- readr::read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/relative_inflexibile_load_share.csv")

#flex_scores <- readr::read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/flex_scores.csv")



#' scenario_params
#'
#' builds the complete parameter set at yeartime from scenario scen
#'
#' @param scenario scenario parameters e.g. scenario_0
#' @param yeartime decimal time
#'
#' @return long form dataframe containing parameter names and values
#' @export
#'
#' @examples
#' params <- scenario_params(sD,2026)
scenario_params <- function(scenario,yeartime){
  #fast params
  scen <- tibble::tibble(parameter="yeartime", value=  yeartime)
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="dep_introduction", value=  dplyr::filter(scenario, parameter=="dep_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="tou_introduction", value=  dplyr::filter(scenario, parameter=="tou_introduction")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="day_network_charge", value=  day_network_charge_fun(scenario,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="night_network_charge", value=  night_network_charge_fun(scenario,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="peak_network_charge", value=  peak_network_charge_fun(scenario,yeartime)))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="standing_charge_flat", value =  standing_charge_fun(scenario,yeartime,"flat")))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="standing_charge_tou", value =  standing_charge_fun(scenario,yeartime,"tou")))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="standing_charge_dyn", value =  standing_charge_fun(scenario,yeartime,"dyn")))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="nu.", value =  dplyr::filter(scenario, parameter=="nu.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="p.", value =  dplyr::filter(scenario, parameter=="p.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="nu.", value =  dplyr::filter(scenario, parameter=="nu.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="rho.", value =  dplyr::filter(scenario, parameter=="rho.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="delta.", value =  dplyr::filter(scenario, parameter=="delta.")$value))
  scen <- dplyr::bind_rows(scen,tibble::tibble(parameter="s.", value =  dplyr::filter(scenario, parameter=="s.")$value))


  #return(scen)
  return(scen %>% fast_params())
}

#' fast_params
#'
#' helper function to convert a long format dataframe to an environment object, used for fast access to scenario parameters
#'
#' @param params_long long format dataframe with columns "parameter" and "value"
#'
#' @return environment object
#' @export
#'
#' @examples
fast_params <- function(params_long){

  test <- as.list(params_long$value)
  names(test) <- params_long$parameter
  test <- list2env(test)
  return(test)
}


#' survey_bills_to_kwh
#'
#' a function that converts highest and lower bi-monthly bills from survey to daily D_max and D_min assuming
#' a seasonal demand lag_D. D_max and D_min are used to estimate the financial return on pv bess investment.
#' Missing data are imputed by default. An issue that needs to be addressed is "level pay". Stochastic.
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
  complete_data <- complete_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q15"=response_code,"lowest_bill"=bill),by="q15")
  complete_data <- complete_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q14"=response_code,"highest_bill"=bill),by="q14")
  complete_data <- complete_data %>% dplyr::mutate(dplyr::across(dplyr::everything(),as.numeric))
  # regression model relating high and low bills
  high_model <- nls(highest_bill ~ a * lowest_bill + exp(b), start = list(a = 2, b = 1),
                    algorithm = "port", lower = c(0, -Inf), upper = c(5, Inf),data=complete_data)
  low_model <- nls(lowest_bill ~ a * highest_bill + exp(b), start = list(a = 2, b = 1),
                   algorithm = "port", lower = c(0, -Inf), upper = c(5, Inf),data=complete_data)

  #q14 missing but not q15
  missing_high_data <- data_in %>% dplyr::filter(q14==13,q15 != 13)
  missing_high_data <- missing_high_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q15"=response_code,"lowest_bill"=bill),by="q15")
  missing_high_data$lowest_bill <- as.numeric(missing_high_data$lowest_bill)
  coefs <- summary(high_model)$coefficients[, "Estimate"]
  std_errors <- summary(high_model)$coefficients[, "Std. Error"]
  missing_high_data$a <- rnorm(nrow(missing_high_data), mean=coefs[1],sd=std_errors[1])
  missing_high_data$b <- rnorm(nrow(missing_high_data), mean=coefs[2],sd=std_errors[2])
  missing_high_data <- missing_high_data %>% dplyr::mutate(highest_bill = a*lowest_bill + exp(b))
  #q15 missing but not q15
  missing_low_data <- data_in %>% dplyr::filter(q14!=13,q15 == 13)
  missing_low_data <- missing_low_data %>% dplyr::inner_join(bill_values %>% dplyr::rename("q14"=response_code,"highest_bill"=bill),by="q14")
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
  missing_both_data <- missing_both_data %>% dplyr::inner_join(logparams,by="qi") %>% dplyr::rowwise() %>% dplyr::mutate(highest_bill=rlnorm(1,logmean,logsd))
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
  complete_data <- complete_data %>% dplyr::rowwise() %>% dplyr::mutate(kWh=sum(demand_fun(1:365,D_max,D_min)))

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

#' night_network_charge_fun
#'
#' Night network base price.
#'
#' @param scen scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
#' night_network_charge_fun(sD,2028.6)
night_network_charge_fun <- function(scen,yeartime){
  #
  charges <- scen %>% dplyr::filter(parameter %in% c("night_network_charge_2020","night_network_charge_2026","night_network_charge_2030","night_network_charge_2040")) %>% dplyr::pull(value)
  approx(x=c(2020.5,2026.5,2030.5,2040.5), y=charges,xout=yeartime,rule=2)$y %>% return()
}

#' day_network_charge_fun
#'
#' day network base price.
#'
#' @param scen scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
#' day_network_charge_fun(sD,2028.6)
day_network_charge_fun <- function(scen,yeartime){
  #
  charges <- scen %>% dplyr::filter(parameter %in% c("day_network_charge_2020","day_network_charge_2026","day_network_charge_2030","day_network_charge_2040")) %>% dplyr::pull(value)
  approx(x=c(2020.5,2026.5,2030.5,2040.5), y=charges,xout=yeartime,rule=2)$y %>% return()
}


#' peak_network_charge_fun
#'
#' peak network charges + supplier ToU uplift. The base price of dynamic pricing.
#'
#' @param scen scenario dataframe
#' @param yeartime decimal time
#'
#' @return price per kWh in euros
#' @export
#'
#' @examples
#' peak_network_charge_fun(sD,2028.6)
peak_network_charge_fun <- function(scen,yeartime){
  #
  charges <- scen %>% dplyr::filter(parameter %in% c("peak_network_charge_2020","peak_network_charge_2026","peak_network_charge_2030","peak_network_charge_2040")) %>% dplyr::pull(value)
  approx(x=c(2020.5,2026.5,2030.5,2040.5), y=charges,xout=yeartime,rule=2)$y %>% return()
}

#' standing_charge_fun
#'
#' household standing charge (expectations & historical). The current setup assumes that the standing charges for flat and tou pricing are the same.
#' The dynamic standing charge may differ.
#'
#' @param scen scenario dataframe
#' @param yeartime decimal time
#' @param tariff_plan tariff plan (flat, tou or dynamic)
#'
#' @return standing charge
#' @export
#'
#' @examples
#' standing_charge_fun(sD,2036,"dynamic")
standing_charge_fun <- function(scen,yeartime,tariff_plan){

  if(tariff_plan %in% c("flat","tou"))
  {values <- scen %>% dplyr::filter(parameter %in% c("standing_charge_2015","standing_charge_2022","standing_charge_2025","standing_charge_2030","standing_charge_2040")) %>% dplyr::pull(value) #add more costs here if known
  res <- approx(x=c(2015.5,2022.5,2025.5,2030.5,2040.5), y=values,xout=yeartime,rule=2)$y}
  else
  {values <- scen %>% dplyr::filter(parameter %in% c("dynamic_standing_charge_2026","dynamic_standing_charge_2030","dynamic_standing_charge_2040")) %>% dplyr::pull(value) #add more costs here if known
  res <- approx(x=c(2026.5,2030.5,2040.5), y=values,xout=yeartime,rule=2)$y}
  return(res)

}


#' decompose_logprices
#'
#' Seasonal and Trend decomposition using Loess (STL) decomposition of historical wholesale prices.
#' \cr
#' A log-type transformation \deqn{y=\operatorname{arcsinh}(\frac{x}{scale})}
#' This is referred to as the "logprice" and is close to a log transformation when x is larger than \eqn{scale}. However, it crosses over to a linear function for small xand therefore handle negative wholesale prices smoothly.
#' \cr
#' The seasonal decomposition is in to periodic daily, weekly and aperiodic yearly terms.
#' decompose_logprices() also fills any gaps in the input data to form an hourly time series.\cr
#' \cr
#' The output of decompose_logprices() is the key input to generate_logprice_hmm() and to simulate_prices()
#'
#' @param price_data price data in format datetime,price e.g. sem_prices_2019_2025
#'
#' @returns dataframe with STL decomposition of hourly price data
#' @export
#'
#' @examples
#' decompose_logprices(sem_prices_2019_2025)
#'
decompose_logprices <- function(price_data){

  hourly <- price_data %>% tsibble::as_tsibble(index = datetime) #%>% mutate(logprice=log(price+10))
  hourly <- hourly %>% tsibble::fill_gaps() #replaces with NAs
  hourly <- hourly %>% dplyr::mutate(price = imputeTS::na_seasplit(price, algorithm = "interpolation", find_frequency = TRUE))

  scale0 <- sD %>% dplyr::filter(parameter=="s.") %>% dplyr::pull(value)

  stopifnot(!tsibble::has_gaps(hourly))
  # scale has to be set
  #scale <- sd(hourly$price)/2
  hourly$logprice <- asinh(hourly$price/scale0)
  dcmp <- hourly %>% fabletools::model(feasts::STL(logprice ~ trend(window = 2001)
                                           # + season(period = "1 day",window=30*24+1)
                                           + season(period = "1 day",window="periodic")
                                           + season(period = "1 week",window="periodic")
                                           + season(period=8766,window="periodic")
                                           ,robust=TRUE)) %>% fabletools::components()
  dcmp <- dcmp %>% dplyr::rename("season_week"=`season_1 week`) %>% dplyr::rename("season_year"=season_8766) %>% dplyr::rename("season_day"=`season_1 day`)
  dcmp <- dcmp %>% dplyr::select(-season_adjust,-.model)
  #smoothing of annual seasonal components, adding new residuals to existing residual
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
#' generate_logprice_hmm uses a gaussian model at present. In principle should be replaced with fat-tailed distribution e.g. student-t in future
#' because outliers lead to an overestimation of standard deviation. The workaround used at present is to winsorise the
#' logprice residuals.\cr
#' \cr
#' It is not necessary to run this function at the beginning of each simulation run. The 2019-2025 calibration is in
#' the model hmm_fit.
#'
#' @param dcmp decomposed logprices from decompose_logprices()
#' @param n_states number of Markov states, default 3
#' @param winsor winsorisation (clipping) level e.g if winsor=0.01 quantile bounds are 1-99
#'
#' @returns a depmixS4 model object
#' @export
#'
#' @examples
#' dcmp <- decompose_logprices(sem_prices_2019_2025)
#' generate_logprice_hmm(dcmp,n_state=3)
#'
generate_logprice_hmm <- function(dcmp, n_states = 3, winsor = 0.01) {

  # 1. Outlier caps (Winsorization)
  lower_bound <- quantile(dcmp$remainder, winsor, na.rm = TRUE)
  upper_bound <- quantile(dcmp$remainder, 1 - winsor, na.rm = TRUE)

  dcmp_clipped <- dcmp %>%
    dplyr::mutate(remainder = dplyr::case_when(
      remainder > upper_bound ~ upper_bound,
      remainder < lower_bound ~ lower_bound,
      TRUE ~ remainder
    ))

  x <- dcmp_clipped$remainder

  # 2. Define Initial Parameters for HiddenMarkov
  # Initial state transition matrix (high diagonal probability = state persistence)
  Pi <- matrix((1 - 0.8) / (n_states - 1), nrow = n_states, ncol = n_states)
  diag(Pi) <- 0.8

  # Initial state probabilities (uniform)
  delta <- rep(1 / n_states, n_states)

  # Initial distribution parameters (means evenly spread across data quantiles)
  probs <- seq(1 / (n_states + 1), n_states / (n_states + 1), length.out = n_states)
  init_means <- as.numeric(quantile(x, probs = probs, na.rm = TRUE))
  init_sds <- rep(sd(x, na.rm = TRUE), n_states)

  pm <- list(mean = init_means, sd = init_sds)

  # 3. Build & Fit Model
  model <- HiddenMarkov::dthmm(
    x = x,
    Pi = Pi,
    delta = delta,
    distn = "norm",
    pm = pm
  )

  # BaumWelch is the EM algorithm
  hmm_fit <- HiddenMarkov::BaumWelch(model)

  Pi <- hmm_fit$Pi
  delta <- hmm_fit$delta
  mu <- hmm_fit$pm$mean
  sigma <- hmm_fit$pm$sd
  #return(fit_model)
  return(list("pi_mat"=Pi,"delta"=delta,"mu"=mu,"sigma"=sigma))

}

#' simulate hmm
#'
#' Utility function to simulate hidden markov time-series in base R. Used by dynamic_prices()
#'
#' @param n_steps length of time-series
#' @param hmm_fit listof HMM parameters
#'
#' @returns vector of values of length n_steps
#' @export
#'
#' @examples
#' simulate_hmm(100,hmm_fit)
#'
simulate_hmm <- function(n_steps, hmm_fit) {
  # 1. Pre-allocate the state vector for speed
  states <- integer(n_steps)

  n_states <- dim(hmm_fit$pi_mat)[1]
  # 2. Draw the first state using initial probabilities (delta)
  states[1] <- sample(1:n_states, size = 1, prob = hmm_fit$delta)

  # 3. Loop to draw subsequent states using the transition matrix (Pi)
  # Base R loops are very fast for sequences of this size (~130k hours)
  for (t in 2:n_steps) {
    states[t] <- sample(1:n_states, size = 1, prob = hmm_fit$pi_mat[states[t-1], ])
  }
  # 4. Vectorized draw of emissions (simulated prices) based on the state sequence
  sim_series <- rnorm(n_steps, mean = hmm_fit$mu[states], sd = hmm_fit$sigma[states])

  return(sim_series)
}

#' dynamic_prices
#'
#' returns a simulation of sem electricity prices to end_year. The period 2019-2025 uses historic prices. \cr
#' \cr
#' This is based on the wholesale price decomposition in sem_logprices_2019_2025_decomp for observed seasonality (daily, weekly and annual),
#' and the pre-fit gaussian HMM for logprice residuals. Projections are based on trend price scenarios for 2030 and 2040.\cr
#' \cr
#' #' This function generates an hourly wholesale price simulation from Jan 1 2026 to 31 Dec 2040.\cr
#' \cr
#' The projections derive from the product of three factors - a price trend, a seasonal component, and a hidden markov gaussian noise components. Thus
#' projections reflect heteroskedasticity of electricity prices and and is achieved through a log-type transformation of the price data.
#' \cr
#' In practice the transformation used is \eqn{ \asinh{\frac{price}{scale}}}. This handles negative wholesale prices but is similar to
#' a log transformation for prices greater than \eqn{scale}.
#' pre 2026
#'
#' @param scen scenario
#' @param end_year end year
#'
#' @returns dataframe with columns datetime, price (euros/kWh)
#' @export
#'
#' @examples
#' dynamic_prices(sD)
dynamic_prices <- function(scen,end_year=2040){
  #
  scale0 <- scen %>% dplyr::filter(parameter=="s.") %>% dplyr::pull(value)
  t1 <- lubridate::ymd_hms("2026-01-01 00:00:00", tz = "UTC")
  t2 <- lubridate::ymd_hms(paste(end_year,"-12-31 23:00:00", tz = "UTC", sep=""))
  # 2. Generate the hourly equence using base R's seq() with lubridate's hours(1)
  hourly_sequence <- seq(from = t1, to = t2, by = "1 hour")

  sim_length <- length(hourly_sequence)

  sim_series <- simulate_hmm(sim_length, depmicrosimr::hmm_fit)

  sim_logprices <-  tibble::tibble(datetime=hourly_sequence,sim=sim_series)
  #seasonal factors
  daily_lookup <- sem_logprices_2019_2025_decomp %>% tibble::as_tibble() %>%
    # Identify unique hour of the week (1 to 24)
    dplyr::mutate(hour_of_day = lubridate::hour(datetime)+1)  %>%
    dplyr::group_by(hour_of_day) %>%
    dplyr::summarise(season_daily = dplyr::first(season_day), .groups = "drop")


  weekly_lookup <- sem_logprices_2019_2025_decomp %>% tibble::as_tibble() %>%
    # Identify unique hour of the week (1 to 168)
    dplyr::mutate(hour_of_week = (lubridate::wday(datetime) - 1) * 24 + lubridate::hour(datetime) + 1) %>%
    dplyr::group_by(hour_of_week) %>%
    dplyr::summarise(season_weekly = dplyr::first(season_week), .groups = "drop")

  annual_lookup <- sem_logprices_2019_2025_decomp %>%
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
  ###########
  #trend prices
  ############
  sem_trend_price_2030 <- scen %>% dplyr::filter(parameter=="sem_price_2030") %>% dplyr::pull(value)*1000
  sem_trend_price_2040 <- scen %>% dplyr::filter(parameter=="sem_price_2040") %>% dplyr::pull(value)*1000

  trend_logprices <- tibble::tibble(datetime=hourly_sequence,trend=NA)
  trend_logprice_2026 <- sem_logprices_2019_2025_decomp %>% dplyr::filter(datetime=="2025-12-01 23:00:00") %>% dplyr::pull(trend)
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=replace(trend, datetime=="2026-01-01 00:00:00",trend_logprice_2026))
  #wholesale price 100
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=replace(trend, datetime=="2030-12-31 23:00:00",asinh(sem_trend_price_2030/scale0)))
  #wholesale price 200
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=replace(trend, datetime=="2040-12-31 23:00:00",asinh(sem_trend_price_2040/scale0)))
  #linearly interp
  trend_logprices <- trend_logprices %>% dplyr::mutate(trend=zoo::na.approx(trend))

  sim_logprices <- sim_logprices %>% dplyr::inner_join(trend_logprices,by="datetime") %>% dplyr::inner_join(seasonal_logprices,by="datetime")

  sim_prices <- sim_logprices %>% dplyr::mutate(price=scale0*sinh(sim+trend+season)/1000) %>% dplyr::select(datetime,price)
  sim_prices$regime <- "simulated"
  #scale all price by dynamic scale
  hist <- sem_logprices_2019_2025_decomp %>% dplyr::select(datetime,logprice) %>% dplyr::mutate(regime="historical")
  hist <- hist %>% dplyr::mutate(price= scale0*sinh(logprice)/1000) %>% dplyr::select(-logprice)
  hist %>% dplyr::bind_rows(sim_prices)

}

#dyn <- simulate_prices(dcmp,hmm_fit, trend_price_2030=trend_price_2030, trend_price_2040=trend_price_2040)



#' get_sem_prices
#'
#' creates future hourly retail electricity prices for flat, day/night/peak and dynamic tariff plans up to end_year. In the dynamic case of
#' The dynamic simulated prices set at the beginning of each model run (dyn_prices) (no need to recalculate during a run).
#'
#' @param scen scenario
#' @param start_year default 2019
#' @param end_year default 2040
#'
#' @returns 3 column tibble datetime,tariff_plan, price
#' @export
#'
#' @examples
#' prices_scen <- get_sem_prices(sD)
get_sem_prices <- function(scen,start_year=2019,end_year=2040){
  #
  start <- lubridate::date_decimal(start_year)
  end <- lubridate::date_decimal(end_year+1)
  ts <- tibble::tibble(datetime=seq(start,end,by="hour"))
  tou <- tou_tariffs %>% dplyr::rename("hour"=start) %>% dplyr::rename("tou"=tariff) %>% dplyr::select(-end)
  ts <- ts %>% dplyr::mutate(hour=lubridate::hour(datetime)) %>% dplyr::inner_join(tou,by="hour") %>% dplyr::select(-hour)

  dynamic <- dynamic_prices(scen,end_year) %>% dplyr::select(-regime)
  ts %>% dplyr::inner_join(dynamic,by="datetime")
  #impose a price cap
  #cap_scale <- scen %>% dplyr::filter(parameter=="dynamic_price_cap_scale") %>% dplyr::pull(value)
  #dynamic <- dynamic %>% dplyr::mutate(price_cap = cap_scale*flat_tariff_fun(sD,lubridate::decimal_date(datetime)))
  #dynamic <- dynamic %>% dplyr::mutate(price = pmin(price_cap, price)) %>% dplyr::select(-price_cap)

  #dynamic$tariff_plan <- "dynamic"

}


#' flex_score_cube
#'
#' utility function used by match_flex_params(). fex_score_cube() returns a table of flexibilty scores for a range \eqn{\phi, \gamma, \tau} triples.
#' It is based on the 3-hour flexibility score.
#' @param eta eta parameter assumption
#' @param gamma gamm parameter assumption
#' @returns
#' @export
#'
#' @examples
#' flex_score_cube()
flex_score_cube <- function(eta=0.2,gamma=10){

  #flex_scores1 <- flex_scores %>% dplyr::filter(flex_score <= max_flex)
  surface_model <- mgcv::gam(
    flex_3hr ~ s(phi, tau, k = 15),
    family = gaussian(link = "log"), # Forces non-negative predictions
    data = flex_scores %>% dplyr::filter(tariff_plan=="dynamic",eta==.$eta,gamma==.$gamma) %>% dplyr::select(phi,gamma,eta,tau,flex_3hr)
  )
  # fine-graining
  phi_grid   <- seq(min(flex_scores$phi),   max(flex_scores$phi),   length.out = 100)
  #gamma_grid <- seq(min(flex_scores$gamma), max(flex_scores$gamma), length.out = 80)
  tau_grid <- seq(min(flex_scores$tau), max(flex_scores$tau), length.out = 100)

  # Expand into a dense 2D grid matrix (40,000 points)
  dense_grid <- tidyr::expand_grid(phi = phi_grid,tau=tau_grid)
  # Predict the continuous flex_scores across the entire surface
  dense_grid$flex_score <- predict(surface_model, newdata = dense_grid, type = "response")
  dense_grid %>% return()

}

#' match_flex_params
#'
#' This function returns a flexibility \eqn{tau,phi} parameter couple given a value for the MAE 3-hourly load-shifting
#' quantity x. Inflexible agents have zero loadshift MAE while highly flexible agents have loadshifting of about 30%.\cr
#' \cr
#' The utility function flex_score_cube() must be called before using this function (see examples). This recasts the data in
#' flex_scores.\cr
#' For ToU parameter inference tau is set at
#'
#'
#' @param x target MAD loadshifting score defined as MAD hourly dev
#' @param score_cube flex parameter/score table
#'
#' @returns single row dataframe
#' @export
#'
#' @examples
#' score_cube <- flex_score_cube(0.5,40)
#' match_flex_params(25.4,score_cube)
#'
match_flex_params <- function(x,score_cube){
  #
  tol <- 0.1
  #coordinates of the contour line at height x
  xmax <- max(score_cube$flex_score)
  xmin <- min(score_cube$flex_score)
  x <- max(xmin,x)
  x <- min(x,xmax)
  matching_triples <- score_cube %>% dplyr::filter(abs(flex_score - x) <= tol) %>% dplyr::select(phi,tau)
  matching_triples %>% dplyr::slice_sample()

}


#' roundr
#'
#' stochastic round
#' @param x real
#'
#' @returns integer
#' @export
#'
#' @examples
#' replicate(100,roundr(2.5)) |> mean()
roundr <- function (x)
{
  x1 <- trunc(x)
  weights = c(1 + x1 - x, x - x1)
  return(sample(c(x1, x1 + 1), size = 1, prob = weights))
}


#' sem_trend_price
#'
#' The projected trend sem price. Not used much except to model the likely dynamic price cao set by CRU.
#'
#' @param scen input scenario
#' @param yeartime decimal time
#'
#' @returns euros per kWh
#' @export
#'
#' @examples
#' sem_trend_price(sD,2028)
sem_trend_price <- function(scen,yeartime){
  #
  values <- scen %>% dplyr::filter(parameter %in% c("sem_price_2026","sem_price_2030","sem_price_2040")) %>% dplyr::pull(value) #add more costs here if known
  res <- approx(x=c(2026,2030.5,2040.5), y=values,xout=yeartime,rule=2)$y
  return(res)
}


#' get_profile
#'
#' returns the flexible profile from 1-Jan to 31 Dec of year in a given price scenario.\cr
#' \cr
#' This function is used in get_aggregate_profile()
#'
#' @param year integer year
#' @param kWh annual usage
#' @param tariff_plan tariff plan
#' @param phi inflexible fraction
#' @param gamma cost penality
#' @param eta ramping penalty
#' @param tau loadshift horizon
#' @param natural_profile natural profile
#' @param prices_scen price scenario
#'
#' @returns data frame
#' @export
#'
#' @examples
#' prices_scen <- set_prices(sD)
#' get_profile(2030,3000,"dynamic",0.25,1,0.2,48,"LP1",prices_scen)
#'
get_profile <- function(year, kWh, tariff_plan, phi=0.5, gamma=0.25, eta=0.1, tau=24, natural_profile="LP1",prices_scen) {
  #
  stopifnot(tariff_plan %in% c("flat","tou","tou_old","dynamic"))
  profile <- tolower(natural_profile)
  load <- load_profiles_generalised %>% dplyr::select(datetime,any_of(profile))
  #prices <- prices %>% dplyr::select(datetime,tariff_plan,profile)
  prices_scen_1 <- prices_scen %>% dplyr::inner_join(load,by=c("datetime",profile)) %>% dplyr::filter(tariff_plan==.env$tariff_plan)
  # 1. Fast date boundary calculation
  start_time <- lubridate::date_decimal(year)
  end_time   <- lubridate::date_decimal(year + 1)

  # 2. Extract matching records
  df <- prices_scen_1 %>% dplyr::filter(datetime >= start_time,
                                        datetime <= end_time)

  # 3. Vectorized baseline load adjustment
  df$load <- df[[profile]] * kWh
  df <- df %>% dplyr::select(datetime,load,price) %>% dplyr::arrange(datetime)

  #Scale parameter by the flexible load
  gamma_scaled <- gamma * (8760 / kWh)
  eta_scaled   <- eta * (8760 / kWh)
  if (tariff_plan != "flat"){
    df <- get_flex(df, phi, gamma_scaled, eta_scaled, tau)
    df <- df |> dplyr::select(datetime,price,load,load_opt) |> dplyr::rename("natural_load"=load,"optimised_load"=load_opt)}
  else{

    df$optimised_load <- df$load
    df <- df %>% rename("natural_load"=load)
  }
  # 6. Return the final dataframe cleanly (No pipes on the return statement!)
  return(df)
}


#' get_aggregate_profile
#'
#' @param year integer year
#' @param abm abm output dataframe
#' @param prices_scen  price scenario
#'
#' @returns
#' @export
#'
#' @examples
get_aggregate_profile <- function(year,abm,prices_scen){

  abm_y <- abm %>% filter(date==ymd(paste(year,"01","01",sep="-")))

  func <- function(kWh, tariff_plan, phi, gamma, eta, tau, natural_profile) get_profile(year,kWh, tariff_plan, phi, gamma, eta, tau, natural_profile)

  abm_y <- abm_y %>% select(j,kWh,tariff_plan, phi, gamma, eta, tau, natural_profile)
  #aggregate by tariff_plan

  res <- tibble()
  for(tariff_plan1 in c("flat","tou","dynamic"))
  {
    abm_s <- abm_y %>% filter(tariff_plan==tariff_plan1)
    res1 <- pmap(abm_s, func) |> list_rbind() |> group_by(datetime) |> summarise(natural_load=sum(natural_load),optimised_load = sum(optimised_load, na.rm = TRUE), .groups = "drop")
    res1$tariff_plan <- tariff_plan1
    res <- res %>% bind_rows(res1)
  }

  res <- abm_s |> dplyr::mutate(data = pmap(pick(-j), func)) |> tidyr::unnest(cols = data) |> summarise(
    natural_load   = sum(natural_load),
    optimised_load = sum(optimised_load, na.rm = TRUE),
    .by = c(j, tariff_plan,datetime) # Group by j (and datetime if needed)
  )

  return(res)

}


#' get_full_annual_cost
#'
#' This functional calculates the projected annual electricity cost at yeartime. The behavioural costs are included.
#'
#' @param yeartime start of annual evaluation period
#' @param kWh annual kWh
#' @param tariff_plan tariff plan
#' @param phi inflexible fraction
#' @param gamma dimensionless cost penality
#' @param eta dimensionless ramping penalty
#' @param tau energy recorovery horizon
#' @param kernel chocie of kerel, default "exp"
#' @param natural_profile L{1 or LP3 at the moment}
#' @param prices_scen price scenario
#'
#' @returns
#' @export
#'
#' @examples
#' prices_scen <- set_prices(sD)
#' get_full_annual_cost(2030,8760,"flat",0.2,10,0.1,24,"exp","LP1",prices_scen)
#' get_full_annual_cost(2030,8760,"tou",0.2,10,0.1,24,"exp","LP1",prices_scen)
#' get_full_annual_cost(2030,8760,"dynamic",0.2,10,0.1,24,"exp","LP1",prices_scen)
#'
get_full_annual_cost <- function(yeartime=2030, kWh=8760, tariff_plan, phi=0.5, gamma=10, eta=1, tau=24,kernel="exp",natural_profile="LP1", prices_scen) {
  #
  stopifnot(tariff_plan %in% c("flat","tou","tou_old","dynamic"))
  profile <- tolower(natural_profile)
  load <- depmicrosimr::load_profiles_generalised %>% dplyr::select(datetime,any_of(profile))
  #prices <- prices %>% dplyr::select(datetime,tariff_plan,profile)
  prices_scen_1 <- prices_scen %>% dplyr::inner_join(load,by=c("datetime",profile)) %>% dplyr::filter(tariff_plan==.env$tariff_plan)
  # 1. Fast date boundary calculation
  start_time <- lubridate::date_decimal(yeartime)
  end_time   <- lubridate::date_decimal(yeartime + 1)

  # 2. Extract matching records
  df <- prices_scen_1 %>% dplyr::filter(datetime >= start_time,
                                        datetime <= end_time)

  # 3. Vectorized baseline load adjustment
  df$load <- df[[profile]] * kWh
  df <- df %>% dplyr::select(datetime,load,price) %>% dplyr::arrange(datetime)


  W <- ceiling(5 * tau) #might not be great for cauchy kernel
  lags <- 0:W
  plags <- sqrt(5) * lags / (0.8385*tau) #used to ensure matern integrates to tau
  #scale_parameter <- tau/gamma(1+1/shape_parameter)
  #kernel_values <- exp(-(lags / scale_parameter)^shape_parameter)
  kernel_values <- if(kernel=="matern") {
    (1 + plags + (plags^2) / 3) * exp(-plags)
  } else if(kernel=="cauchy") {
    1/(1+(lags/(0.6366*tau))^2)
  } else if(kernel=="exp"){
    exp(-(lags /tau))
  } else {
    exp(-(lags/(1.128*tau))^2)
  }
  #frobenius scaling L2
  frob_sq <- sum(kernel_values^2) + sum(kernel_values[-1]^2)

  #Scale parameter by the flexible load to get correct dimenions
  p_ref <- median(df$price)
  L_ref <- mean(df$load)*(1-phi)

  # 2. Convert Dimensionless (gamma, eta) to Dimensionful Parameters
  # Units of dim_scale are [Currency / kW^2]
  parameter_scaling <- p_ref / L_ref
  gamma_scaled <- gamma*parameter_scaling/frob_sq
  eta_scaled   <- eta*parameter_scaling
  #compute x K^T K x
  compute_behavioural_cost <- function(x, tau) {
    N <- length(x)
    r <- exp(-1 / tau)

    # Forward pass: sum_{k <= t} r^(t - k) * x_k
    a <- numeric(N)
    a[1] <- x[1]
    for (t in 2:N) {
      a[t] <- x[t] + r * a[t - 1]
    }

    # Backward pass: sum_{k > t} r^(k - t) * x_k
    b <- numeric(N)
    b[N] <- 0
    if (N > 1) {
      for (t in (N - 1):1) {
        b[t] <- r * (b[t + 1] + x[t + 1])
      }
    }

    # y = K %*% x
    y <- a + b

    # Return x^T K^T K x = sum(y^2)
    return(sum(y^2))
  }

  # 5. Calculate flexible loads conditionally
  if (tariff_plan != "flat") {
    df <- get_flex(df, phi, gamma, eta, tau,kernel)
    bill_inflex <- sum(df$price * df$load)
    bill_flex   <- sum(df$price * df$load_opt)
    ramping_cost <- eta_scaled * sum(diff(df$x)^2)
    behavioural_cost <- gamma_scaled*compute_behavioural_cost(df$x,tau)
  } else {
    # For flat tariffs, inflexible and flexible loads are identical
    bill_inflex <- sum(df$price * df$load)
    bill_flex   <- bill_inflex
    ramping_cost <- 0
    behavioural_cost <- 0
  }


  # 6. Return the final dataframe cleanly (No pipes on the return statement!)
  return(
    data.frame(
      tariff_plan            = tariff_plan,
      kWh=kWh,
      phi = phi,
      gamma = gamma,
      eta=eta,
      tau=tau,
      annual_bill_inflexible = bill_inflex,
      annual_bill_flexible   = bill_flex,
      fin_gain                   = round(bill_flex - bill_inflex),
      penalty = behavioural_cost,
      kinetic = ramping_cost,
      real_gain =  round(bill_flex +behavioural_cost+ramping_cost- bill_inflex)

    )
  )
}


#' get_flex_scores
#'
#' Evaluates a mesure of how much load has been shifted (flexibility) in response a price scenario, tariff plan. Flexibility is defined as
#' \deqn{ \frac{1}{2} \frac{\sum_t |L_{optimised}-L_{natural}|}{\sum_t L_{natural}}}The evaluation period is one year.\cr
#' \cr
#' The main use of this function is to map out the relationship flexibility parameters and stated flexibility scores.
#'
#' @param scen scenario
#' @param year year of evaluation
#' @param kWh annual consumption
#' @param tariff_plan tariff plan
#' @param phi inflexibility parameter
#' @param gamma cost parameter
#' @param eta kinetic cost parameter
#' @param tau load-shift time horizon
#' @param kernel defaut to "exp"
#' @param profile LP1 or LP3
#' @param prices_scen price scenario
#'
#' @returns
#' @export
#'
#' @examples
get_flex_scores <- function(scen,year,kWh,tariff_plan,phi,gamma,eta,tau,kernel,profile="LP1",prices_scen){
  #
  demand <- prices_scen %>% dplyr::filter(lubridate::year(datetime)==year)
  demand <- demand %>% dplyr::inner_join(load_profiles_generalised %>% dplyr::select(datetime,tolower(profile)))
  demand <- demand %>% dplyr::mutate(load=kWh*lp1) %>% dplyr::select(-lp1)
  demand <- demand %>% dplyr::filter(tariff_plan==.env$tariff_plan) %>% dplyr::select(datetime,price,load)
  flex <- get_flex(demand,phi,gamma,eta,tau,kernel,precision = 1e-3)
  flex_1hr <- 100*sum(abs((flex$load_opt-flex$load)))/(2*kWh) #Total variation distance factor of 2
  flex1 <- flex %>% group_by(period = floor_date(datetime, unit = "3 hours"))%>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_3hr <- 100*sum(abs((flex1$load_opt-flex1$load)))/(2*kWh)
  flex1 <- flex %>% group_by(period = floor_date(datetime, unit = "6 hours"))%>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_6hr <- 100*sum(abs((flex1$load_opt-flex1$load)))/(2*kWh)
  flex1 <- flex %>% group_by(period = floor_date(datetime, unit = "12 hours"))%>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_12hr <- 100*sum(abs((flex1$load_opt-flex1$load)))/(2*kWh)
  flex1 <- flex %>% group_by(period = floor_date(datetime, unit = "24 hours"))%>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_24hr <- 100*sum(abs((flex1$load_opt-flex1$load)))/(2*kWh)

  flex1 <- flex %>% dplyr::group_by(lubridate::yday(datetime)) %>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_interday <- 100*sum(abs(flex1$load_opt-flex1$load))/kWh
  flex1 <- flex %>% dplyr::group_by(lubridate::week(datetime)) %>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_interweek <- 100*sum(abs(flex1$load_opt-flex1$load))/kWh
  tibble::tibble(tariff_plan=tariff_plan,profile=profile,phi=phi,gamma=gamma,eta=eta,tau=tau,flex_1hr=flex_1hr,flex_3hr=flex_3hr,flex_6hr=flex_6hr,flex_12hr=flex_12hr,flex_24hr=flex_24hr,flex_day=flex_interday,flex_week=flex_interweek)
}




