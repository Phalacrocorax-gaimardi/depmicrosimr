# Copyright 2026 University College Dublin
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.

#sem_prices_2023_2025 <- hourly_2023_2025
#sem_prices_2023_2025 <- tibble(sem_prices_2023_2025) %>% select(-Price)
#use_data(sem_prices_2023_2025,overwrite = T)
#load_profiles <- lp
#use_data(load_profiles,overwrite=T)




#' get_flex
#'
#' get_flex solves for the flexible load pattern \eqn{L_t} that optimises the cost to a household over the hourly period 1:T. Here, the "cost" represents
#' the direct billing cost as well as penalties associated with load-shifting behaviour. get_flex() assumes that total consumption over the period 1:T is unchanged irrespective
#' of prices \eqn{p(t)} \cr
#' \cr
#' The "natural" household load profile without load-shifting is \eqn{L_t^0}, corresponding to a flat electricity price. Load-shifting behaviour may arise in response to varying prices \eqn{p_t}.
#' The billing cost depends on the prices \eqn{\sum_{t=1}^T p_t L_t} where \eqn{L_t} is the load-shifted hourly consumption. Two additional cost penalties arise:
#' \deqn{C = \sum_{t=1}^T p_t L_t + \eta \sum_t (L_{t+1}-L_t)^2 + \gamma_\tau \sum_t \sum_k \exp(-|k|/\tau) \left(L_{t+k}-L_{t+k}^0\right)^2}{C = sum(p*L) + eta*sum(diff(L)^2) + gamma*sum((L-L0)^2)}
#' An additional constraint \eqn{\sum(L_t)=\sum(L_t^0)} is imposed i.e. total household energy consumption is inelastic.
#' \cr
#' The \eqn{\eta} term prevents excessive load-shifting in response to small differences in prices (regularisation). The \eqn{\gamma_\tau} term is permits
#' is a stiffness term that limits load-shifting over a timescale \eqn{\tau} but inhibits it on longer timescales.\cr
#' \cr
#' The cost parameters  are defined for a standard annual load of 8760kWh. The scales inversely with the mean load and
#'
#'
#' @param demand a 3 column dataframe of datetime, hourly prices and natural_load
#' @param phi baseload (inflexible) fraction of load
#' @param gamma unscaled quandatric cost penalty weight
#' @param eta unscaled weight of the kinetic term (regularisation)
#' @param tau flexibility time horizon in hours
#' @param P_max the maximum permitted load
#' @param precision desired solver precision
#'
#' @returns a 2-column dataframe of datetime and perturbed load
#' @export
#'
#' @examples
#'
#' demand <- load_profiles %>% dplyr::select(datetime,"lp1")
#' demand <- demand %>% dplyr::mutate(datetime = update(datetime, year=2030))
#' demand <- demand %>% dplyr::mutate(load=8760*lp1) %>% dplyr::select(-lp1)
#' demand <- demand %>% dplyr::inner_join(get_tariff_prices(sD) %>% dplyr::filter(tariff_plan=="tou") %>% dplyr::select(datetime,price))
#' get_flex(demand,phi=0.5,gamma=0.5,eta=1,tau=48)
#'
get_flex <- function(demand, phi = 0.5, gamma = 0.5, eta = 1,tau = 24,  P_max = 10,precision=1e-6) {
  # T = Total horizon in hours
  #if(dim(prices)[1] != dim(loads)[1]) stop("dimensions of prices and natural loads do not agree ")
  #print()
  #demand <- prices %>% dplyr::inner_join(loads)
  T_total <- nrow(demand)
  fload <- (1-phi)*demand$load
  #print(paste("mean load =", mean(fload)))
  price <- demand$price
  if(length(price) != length(fload)) stop("load and price data mismatch")
  # 1. Bandwidth for the Exponential Kernel
  W <- ceiling(5 * tau)
  lags <- 0:W
  kernel_values <- exp(-lags / tau)
  #frobenius scaling
  frob_sq <- sum(kernel_values^2) + sum(kernel_values[-1]^2)
  frob_norm <- sqrt(frob_sq)
  # scaled gamma
  gamma_scaled <- gamma / frob_norm

  # 2. Build Quadratic Matrix P
  # Discomfort Kernel (Banded Sparse)
  P_kern <- Matrix::bandSparse(T_total, k = lags,
                       diag = lapply(kernel_values, function(v) rep(v, T_total)),
                       symmetric = TRUE)

  # Kinetic/Ramping Penalty (Finite Difference Matrix)
  # Represents eta * sum((x_t+1 - x_t)^2)
  D <- Matrix::sparseMatrix(i = rep(1:(T_total-1), each = 2),
                    j = as.vector(rbind(1:(T_total-1), 2:T_total)),
                    x = rep(c(-1, 1), T_total-1))
  P_kin <- Matrix::t(D) %*% D

  # Combined P matrix
  P <- (gamma_scaled * P_kern) + (eta * P_kin)
  #symmetrise
  P <- 0.5 * (P + Matrix::t(P))
  # 3. Linear Vector q (Prices)
  q <- price

  # 4. Constraints (A*x)
  # We stack: 1. Sum(x) = 0  (Equality)
  #           2. x_t         (Identity for box constraints)

  A_sum <- Matrix::Matrix(1, nrow = 1, ncol = T_total, sparse = TRUE)
  A_box <- Matrix::Diagonal(T_total)
  A <- rbind(A_sum, A_box)

  # Lower and Upper Bounds
  # Equality constraint: l=0, u=0
  # Capacity constraint: -L0 <= x <= P_max - L0
  l <- c(0, -fload)
  u <- c(0, P_max - fload)

  # 5. Solve using OSQP
  settings <- osqp::osqpSettings(eps_abs = precision, eps_rel = precision, verbose = TRUE)
  model <- osqp::osqp(P = P, q = q, A = A, l = l, u = u, pars = settings)
  res <- model@Solve()

  # 6. Post-processing
  x_opt <- res$x
  demand$x <- x_opt
  demand$load_opt <- demand$load + x_opt

  # Split the resulting load into components
  demand$baseload <- phi*demand$load
  demand$flex_load <- (1 - phi) * demand$load
  demand$flex_opt <- (1 - phi) * demand$load + x_opt

  return(demand)
}


#
#' get_annual_cost
#'
#' evaluates the annual electricity cost of tariff scheme at yeartime for an agent with known natural load profile and flexibility parameters.\cr
#' \cr
#' At present the tariff scheme are flat, day/night/peak and dynamic and the characteristic load profile is LP1 or LP3. Future price assumptions
#' are typically the output of get_tariff_prices(scenario)
#'
#' @param yeartime decimal time
#' @param kWh annual consumption kWh (assumed inflexible)
#' @param tariff flat, tou or dynamic
#' @param phi inflexible fraction
#' @param gamma cost penalty parameter
#' @param eta kinetic regularisation (currently fixed at 0.1)
#' @param tau energy consumption mean reversion time
#' @param profile household usage profile e.g. LP1
#' @param prices_scen tariff price scenario
#'
#' @returns a real number, euros
#' @export
#'
#' @examples
#'
#' prices_scen <- get_tariff_prices(sD)
#' get_annual_cost(2030,4000,"tou",phi=0.5,gamma=10,eta=0.1,tau=48,"LP1",prices_scen)
#'
get_annual_cost <- function(yeartime,kWh,tariff,phi=0.5,gamma=0.25,eta=0.1,tau=24,profile="LP1",prices_scen){

  profile <- tolower(profile)
  load <- load_profiles %>% dplyr::select(datetime,dplyr::any_of(profile)) %>% dplyr::rename("load":=all_of(profile))
  #normalise to kWh annual
  load <- load %>% dplyr::mutate(load = load*kWh)

  start_time <- lubridate::date_decimal(yeartime) %>% lubridate::floor_date(unit="hour")
  end_time <- lubridate::date_decimal(yeartime+1) %>% lubridate::ceiling_date(unit="hour")
  datetimes <- seq(start_time,end_time,by="hour")
  prices <- prices_scen %>% dplyr::filter(datetime %in% datetimes,tariff_plan==tariff)
  #do an inner join by month, day, time by intrdoucinga  dummy year (2000, a leap year)
  df <- prices %>% dplyr::mutate(match_time = update(datetime, year = 2000))
  df <- df %>% dplyr::inner_join(load %>% dplyr::mutate(match_time = update(datetime, year = 2000)) %>% dplyr::select(-datetime),by = "match_time")
  df <- df %>% dplyr::select(-match_time)
  #scaled cost parameters
  gamma <- gamma*(8760/kWh)
  eta <- eta*(8760/kWh)

  #calculate the flexible load in response to price variations
  #flexibilties
  ifelse(tariff=="flat", df <- get_flex(df,phi,gamma,eta,tau) %>% dplyr::select(datetime,price,load,load_opt),
      ifelse(tariff=="tou",df <- get_flex(df,phi,gamma,eta,tau) %>% dplyr::select(datetime,price,load,load_opt),
         df <- get_flex(df,phi,gamma,eta,tau) %>% dplyr::select(datetime,price,load,load_opt)))
  df$tariff_plan <- tariff

  #df <- dplyr::bind_rows(df_flat,df_tou,df_dyn)

  df_summary <- df %>% dplyr::summarise(annual_bill_inflexible=sum(price*load),
                                                           annual_bill_flexible = sum(price*load_opt))
  df_summary %>% dplyr::mutate(gain=round(annual_bill_flexible-annual_bill_inflexible))

}



#

#' get_price_load_scen
#'
#' generates a data frame of hourly prices and load profiles from start_year to end_year\cr
#' \cr
#' get_price_load_scen() can be run once at the beginning of each ABM run to generate new dynamic prices
#'
#' @param sD scenario
#' @param start_year initial year
#' @param end_year final year
#'
#' @returns dataframe
#' @export
#'
#' @examples
#' get_price_load_scen(sD)
get_price_load_scen <- function(sD,start_year=2019,end_year=2040){

  load_profiles <- load_profiles %>%
    dplyr::mutate(mdh = format(datetime, "%m-%d-%H")) %>%
    dplyr::select(-datetime,-day_note)

  prices_scen <- get_tariff_prices(sD,start_year,end_year)
  prices_scen %>%
    dplyr::mutate(mdh = format(datetime, "%m-%d-%H")) %>%
    dplyr::inner_join(load_profiles, by = "mdh") %>%
    dplyr::select(-mdh)
}

#' get_annual_cost_fast
#'
#' evaluates the annual electricity cost of tariff scheme at yeartime for an agent with known natural load profile and flexibility parameters.\cr
#' \cr
#' At present the tariff scheme are flat, day/night/peak and dynamic and the characteristic load profile is LP1 or LP3. Future price assumptions
#' are typically the output of get_tariff_prices(scenario)
#'
#' @param yeartime decimal time
#' @param kWh annual consumption kWh (assumed inflexible)
#' @param tariff flat, tou or dynamic
#' @param phi inflexible fraction
#' @param gamma cost penalty parameter
#' @param eta kinetic regularisation (currently fixed at 0.1)
#' @param tau energy consumption mean reversion time
#' @param profile household usage profile e.g. LP1
#' @param prices_scen tariff price scenario
#'
#' @returns a real number, euros
#' @export
#'
#' @examples
#'
#' prices_scen <- get_price_load_scen(sD)
#' get_annual_cost_fast(2030,4000,"tou",phi=0.5,gamma=10,eta=0.1,tau=96,prices_scen=prices_scen)
#'
get_annual_cost_fast <- function(yeartime, kWh, tariff, phi=0.5, gamma=0.25, eta=0.1, tau=24, profile="LP1", prices_scen) {

  stopifnot(tariff %in% c("flat","tou","dynamic"))
  profile <- tolower(profile)

  # 1. Fast date boundary calculation
  start_time <- lubridate::date_decimal(yeartime)
  end_time   <- lubridate::date_decimal(yeartime + 1)

  # 2. Extract matching records
  df <- prices_scen %>%
    dplyr::filter(
      tariff_plan == tariff,
      datetime >= start_time,
      datetime <= end_time
    )

  # 3. Vectorized baseline load adjustment
  df$load <- df[[profile]] * kWh

  # 4. Scaled parameters
  gamma_scaled <- gamma * (8760 / kWh)
  eta_scaled   <- eta * (8760 / kWh)

  # 5. Calculate flexible loads conditionally
  if (tariff != "flat") {
    df <- get_flex(df, phi, gamma_scaled, eta_scaled, tau)
    bill_inflex <- sum(df$price * df$load)
    bill_flex   <- sum(df$price * df$load_opt)
  } else {
    # For flat tariffs, inflexible and flexible loads are identical
    bill_inflex <- sum(df$price * df$load)
    bill_flex   <- bill_inflex
  }

  # 6. Return the final dataframe cleanly (No pipes on the return statement!)
  return(
    data.frame(
      annual_bill_inflexible = bill_inflex,
      annual_bill_flexible   = bill_flex,
      gain                   = round(bill_flex - bill_inflex),
      tariff_plan            = tariff
    )
  )
}



#' get_annual_cost_base
#'
#' evaluates the annual electricity cost of tariff scheme at yeartime for an agent based on standard profiles i.e. no flexibility modelling. This function
#' is used in initialise_agents()
#' \cr
#' At present the tariff scheme are flat, day/night/peak and dynamic and the characteristic load profile is LP1 or LP3. Future price assumptions
#' are typically the output of get_tariff_prices(scenario)
#'
#' @param yeartime decimal time
#' @param kWh annual consumption kWh (assumed inflexible)
#' @param tariff flat, tou or dynamic
#' @param profile household usage profile e.g. LP1
#' @param prices_scen tariff price scenario
#'
#' @returns a real number, euros
#' @export
#'
#' @examples
#'
#' prices_scen <- get_price_load_scen(sD)
#' get_annual_cost_fast(2030,4000,"tou",phi=0.5,gamma=10,eta=0.1,tau=96,prices_scen=prices_scen)
#'
get_annual_cost_base <- function(yeartime, kWh, tariff, profile="LP1", prices_scen) {

  stopifnot(tariff %in% c("flat","tou","dynamic"))
  profile <- tolower(profile)
  stopifnot(profile %in% c("lp1","lp2","lp3","lp4"))
  #one year
  start_time <- lubridate::date_decimal(yeartime)
  end_time   <- lubridate::date_decimal(yeartime + 1)

  #datetime range
  df <- prices_scen %>%
    dplyr::filter(
      tariff_plan == tariff,
      datetime >= start_time,
      datetime <= end_time
    )

  #scale
  df$load <- df[[profile]] * kWh

  return(sum(df$price * df$load))

}

