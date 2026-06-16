# Copyright 2026 University College Dublin
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.

#sem_prices_2023_2025 <- hourly_2023_2025
#sem_prices_2023_2025 <- tibble(sem_prices_2023_2025) %>% select(-Price)
#use_data(sem_prices_2023_2025,overwrite = T)
#load_profiles <- lp
#use_data(load_profiles,overwrite=T)




#' make_demand_response_data
#'
#' this is a utility function that returns a dataframe of hourly household loads and retail prices for input into get_flex
#'
#' @param profile load index column name from load_profiles
#' @param mean_daily_load mean daily load
#' @param years year(s) of price data, from sem_prices_2023_2025
#'
#' @returns dataframe of hourly data for year. load in kWh, price in euro/kWh
#' @export
#'
#' @examples
#' make_demand_response_data("lp1",mean_daily_load=20, years=2023:2025)

make_demand_response_data <- function(profile="lp1",mean_daily_load=20, years=2023:2025){

  load <- load_profiles %>% dplyr::select(datetime,any_of(profile))
  load <- load %>% dplyr::rename("load"=profile) %>% dplyr::mutate(load=mean_daily_load*365*load)

  load_e<- lapply(years, function(y) {load %>% dplyr::mutate(datetime=timechange::time_update(datetime,year = y))}) %>% dplyr::bind_rows()

  sem_prices_2023_2025 %>% dplyr::mutate(price=price/1000) %>% dplyr::inner_join(load_e)

  }


#' get_flex
#'
#' get_flex solves for the flexible load \eqn{L_t} that optimises the cost to a household over the period 1:T. With flat price,
#' the household load is \eqn{L_t^0}. In addition, to
#' the financial cost that depends on the prices \eqn{p(t)}
#' the t with a kinetic regularisation term. The model is
#' \deqn{C = \sum_{t=1}^T p_t L_t + \eta \sum_t (L_{t+1}-L_t)^2 + \gamma_t \sum_t \sum_k \exp(-|k|/\tau) \left(L_{t+k}-L_{t+k}\right)^2}{C = sum(p*L) + eta*sum(diff(L)^2) + gamma*sum((L-L0)^2)}
#' The constraint \eqn{\sum(L_t)=\sum(L_t^0)} is imposed i.e. total household energy consumption is inelastic.
#' \cr
#'
#'
#'
#' @param demand a 3 column dataframe of datetime, hourly prices and natural_load
#' @param phi baseload (inflexible) fraction of load
#' @param gamma the quandatric cost penalty weight
#' @param eta weight of the kinetic term (regularisation)
#' @param tau flexibility time horizon in hours
#' @param P_max the maximum permitted load
#' @param precision desired solver precision
#'
#' @returns a 2-column dataframe of datetime and perturbed load
#' @export
#'
#' @examples
#'
#'
get_flex <- function(demand, phi = 0.5, gamma = 0.5, eta = 0.1,tau = 24,  P_max = 10,precision=1e-6) {
  # T = Total horizon in hours
  #if(dim(prices)[1] != dim(loads)[1]) stop("dimensions of prices and natural loads do not agree ")
  #print()
  #demand <- prices %>% dplyr::inner_join(loads)
  T_total <- nrow(demand)
  fload <- (1-phi)*demand$load
  print(paste("mean load =", mean(fload)))
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
#' get_annual_cost(2030,4000,phi=0.5,gamma=10,eta=0.1,tau=48,prices_scen=prices_scen)
#'
get_annual_cost <- function(yeartime, kWh,phi=0.5,gamma=0.25,eta=0.1,tau=24,profile="LP1",prices_scen){

  profile <- tolower(profile)
  load <- load_profiles %>% dplyr::select(datetime,dplyr::any_of(profile)) %>% dplyr::rename("load":=all_of(profile))
  #normalise to kWh annual
  load <- load %>% dplyr::mutate(load = load*kWh)

  start_time <- lubridate::date_decimal(yeartime) %>% lubridate::floor_date(unit="hour")
  end_time <- lubridate::date_decimal(yeartime+1) %>% lubridate::ceiling_date(unit="hour")
  datetimes <- seq(start_time,end_time,by="hour")
  prices <- prices_scen %>% dplyr::filter(datetime %in% datetimes)
  #do an inner join by month, day, time by intrdoucinga  dummy year (2000, a leap year)
  df <- prices %>% dplyr::mutate(match_time = update(datetime, year = 2000))
  df <- df %>% dplyr::inner_join(load %>% dplyr::mutate(match_time = update(datetime, year = 2000)) %>% dplyr::select(-datetime),by = "match_time")
  df <- df %>% dplyr::select(-match_time)
  #calculate the flexible load in response to price variations
  df_tou <- df %>% dplyr::filter(tariff_plan == "tou") %>% dplyr::select(datetime,load,price)
  df_dyn <- df %>% dplyr::filter(tariff_plan == "dynamic") %>% dplyr::select(datetime,load,price)
  df_flat <- df %>% dplyr::filter(tariff_plan == "flat") %>% dplyr::select(datetime,load,price)
  #flexibilties
  df_tou <- get_flex(df_tou,phi,gamma,eta,tau) %>% dplyr::select(datetime,price,load,load_opt) %>% dplyr::mutate(tariff_plan="tou")
  df_dyn <- get_flex(df_dyn,phi,gamma,eta,tau) %>% dplyr::select(datetime,price,load,load_opt) %>% dplyr::mutate(tariff_plan="dynamic")
  df_flat <- get_flex(df_flat,phi,gamma,eta,tau) %>% dplyr::select(datetime,price,load,load_opt) %>% dplyr::mutate(tariff_plan="flat")
  df <- dplyr::bind_rows(df_flat,df_tou,df_dyn)

  df_summary <- df %>% dplyr::group_by(tariff_plan) %>% dplyr::summarise(annual_bill_inflexible=sum(price*load),
                                                           annual_bill_flexible = sum(price*load_opt))
  df_summary %>% dplyr::mutate(gain=round(annual_bill_flexible-annual_bill_inflexible))

}
