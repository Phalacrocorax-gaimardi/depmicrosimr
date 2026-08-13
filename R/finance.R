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
#' get_flex solves for the flexible hourly household load pattern \eqn{L_t} that optimises the cost to a household over the hourly period 1:T. Here, the "cost" represents
#' the direct billing cost as well as penalties associated with load-shifting behaviour. get_flex() assumes that total consumption over the period 1:T is unchanged irrespective
#' of prices \eqn{p(t)} \cr
#' \cr
#' The "natural" household load profile without load-shifting is \eqn{L_t^0}, corresponding to a flat electricity price profile of the household. Load-shifting arises in response to time-varying prices \eqn{p_t}.
#' The billing cost is is linear \eqn{\sum_{t=1}^T p_t L_t} where \eqn{L_t} is the load-shifted hourly consumption. Two additional cost penalties arise:
#' \deqn{\delta C = \sum_{t=1}^T p_t x_t + \eta \sum_t (x_{t+1}-x_t)^2 + \gamma \sum_t \left( \sum_k g(k-t) x_k \right)^2}{C = sum(p*L) + eta*sum(diff(L)^2) + gamma*sum((L-L0)^2)}
#' \cr
#' The second term is "ramping" cost penalty that limits unrealistic rapid \eqn{x_t} variations in response to minor price variations. The third term is teh square of
#' a convolution of \eqn{x_t} withe a kernel \eqn{g_t} that decays on some timescale \eqn{\tau}. This decay timescale represents the willingness of the
#' household to defer or advance loads in response to anticipated price changes.A simple choice is \eqn{g(t)= \sqrt{\frac{2}{\tau}} \exp(-t/\tau)}
#'
#' \cr
#' The \eqn{\eta} term prevents excessive load-shifting in response to small differences in prices (regularisation). The \eqn{\gamma_\tau} term is permits
#' is a stiffness term that limits load-shifting over a timescale \eqn{\tau} but inhibits it on longer timescales.\cr
#' \cr
#' The cost parameters  are defined for a standard annual load of 8760kWh.\cr
#' \cr
#' get_flex() currently uses a Matern 5/2 kernel, which can be regarded as the limit of a large number of devices with distinct gaussian flexibility cost penalties.
#'
#' Three additional constraints are imposed (1) \eqn{\sum(x_t)=0} is i.e. total household energy consumption is inelastic (2) the maximum import capacity MIC cannot be exceeded
#' \eqn{L^0_t + \sum(x_t) <= MIC} (iii) consumption load cannot be negative \eqn{L^0_t + \sum(x_t) >= 0}.\cr
#' \cr
#' The inflexible fraction \eqn{\phi} is modulated by a time-of-day factor $f_t$.
#'
#'
#'
#' @param demand a 3 column dataframe of datetime, hourly prices and natural_load
#' @param phi baseload (inflexible) fraction of load
#' @param gamma unscaled quandatric cost penalty weight
#' @param eta unscaled weight of the kinetic term (regularisation)
#' @param tau flexibility time horizon in hours
#' @param kernel choice of "matern", "exponential", "cauchy","gauss" (both have single lifetime tau)
#' @param precision desired solver precision
#'
#'
#' @returns a 2-column dataframe of datetime and perturbed load
#' @export
#'
#' @examples
#'
#' demand <- set_prices(sD)
#' demand <- demand %>% dplyr::filter(lubridate::year(datetime)==2026)
#' demand <- demand %>% dplyr::inner_join(load_profiles_generalised %>% dplyr::select(datetime,lp1))
#' demand <- demand %>% dplyr::mutate(load=8760*lp1) %>% dplyr::select(-lp1)
#' demand <- demand %>% dplyr::filter(tariff_plan=="tou") %>% dplyr::select(datetime,price,load)
#' test <- get_flex(demand,phi=0.5,gamma=10,eta=0.5,tau=48,kernel="exp")
#' 100*sum(abs(test$load_opt-test$load))/8760
get_flex <- function(demand, phi = 0.5, gamma = 0.5, eta = 1,tau = 24,kernel="exp",precision=1e-4) {
  # T = Total horizon in hours
  #if(dim(prices)[1] != dim(loads)[1]) stop("dimensions of prices and natural loads do not agree ")
  #print()
  #maximum import capacity
  stopifnot(kernel %in% c("exp","gauss","cauchy","matern"))

  P_max <- sD %>% dplyr::filter(parameter=="mic") %>% dplyr::pull(value)
  demand <- demand %>% dplyr::arrange(datetime) %>% dplyr::mutate(hour=lubridate::hour(datetime))
  demand <- demand %>% dplyr::inner_join(tou_tariffs %>% dplyr::select(start,f_inflexibility) %>% dplyr::rename("hour"=start),by="hour")
  T_total <- nrow(demand)
  #define the flexible load, including time of day modulatio
  demand <- demand %>% dplyr::mutate(phi_t=pmin(1,phi*f_inflexibility)) %>% dplyr::mutate(fload=(1-phi_t)*load)
  fload <- demand$fload #the part of the load that is flexible and modelled
  #print(paste("mean load =", mean(fload)))
  price <- demand$price
  if(length(price) != length(fload)) stop("load and price data mismatch")
  # 1. Bandwidth for the Exponential Kernel
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
  # scale by price and load so that gamma and eta are dimensionaless
  p_ref <- median(demand$price)
  L_ref <- mean(fload)

  # 2. Convert Dimensionless (gamma, eta) to Dimensionful Parameters
  # Units of dim_scale are [Currency / kW^2]
  parameter_scaling <- p_ref / L_ref

  eta_scaled <- eta * parameter_scaling
  gamma_scaled <- gamma / frob_sq * parameter_scaling
  #print(paste("gamma normalisation",frob_sq))
  # L1 Scaling (integrates to 1)
  #kernel_sum <- sum(kernel_values) + sum(kernel_values[-1])
  #gamma_scaled <- gamma / (kernel_sum)^2

  #gamma_scaled <- gamma/tau
  # kernel matrix P_kern
  K <- Matrix::bandSparse(
    T_total,
    k = lags,
    diag = lapply(kernel_values, function(v) rep(v, T_total)),
    symmetric = TRUE
  )
  # temporal penalty: K^T %*% K
  P_kern <- Matrix::crossprod(K)

  # kinetic term (penalise ramping)
  # eta * sum((L_t+1 - L_t)^2)
  D <- Matrix::sparseMatrix(
    i = rep(1:(T_total-1), each = 2),
    j = as.vector(rbind(1:(T_total-1), 2:T_total)),
    x = rep(c(-1, 1), T_total-1),
    dims = c(T_total-1, T_total) # Explicit dimensions for safety
  )
  # Ramping penalty matrix: D^T %*% D
  P_kin <- Matrix::crossprod(D)
  # Combined P matrix
  P <- (gamma_scaled * P_kern) + (eta_scaled * P_kin)
  #symmetrise
  P <- 0.5 * (P + Matrix::t(P))
  #linear term in x*L^0 ??
  #q_kin <- as.vector(2 * eta * (P_kin %*% fload))
  q_kin <- 0
  # Total q vector: price + natural load ramping penalty
  q <- price + q_kin

  # the constraints (A*x)
  # We stack: 1. Sum(x) = 0  (Equality)
  #           2. x_t         (Identity for box constraints)

  A_sum <- Matrix::Matrix(1, nrow = 1, ncol = T_total, sparse = TRUE)
  A_box <- Matrix::Diagonal(T_total)
  A <- rbind(A_sum, A_box)

  # Lower and Upper Bounds
  # Equality constraint: l=0, u=0
  # Capacity constraint: -L0 <= x <= P_max - L0
  l <- c(0, -fload)
  u <- c(0, P_max - demand$load)

  # 5. Solve using OSQP
  settings <- osqp::osqpSettings(eps_abs = precision, eps_rel = precision, verbose = TRUE,max_iter = 10000)
  model <- osqp::osqp(P = P, q = q, A = A, l = l, u = u, pars = settings)
  res <- model@Solve()

  # 6. Post-processing
  x_opt <- res$x
  demand$x <- x_opt
  demand$load_opt <- demand$load + x_opt

  # Split the resulting load into components
  demand$baseload <- demand$phi_t*demand$load
  #demand$flex_load <- (1 - phi) * demand$load
  #demand$flex_opt <- demand$fload + x_opt
  demand <- demand %>% dplyr::select(-hour,-f_inflexibility,-phi_t)
  return(demand)
}



#' get_price_load_scen
#'
#' generates a data frame of hourly prices and load profiles from start_year to end_year\cr
#' \cr
#' get_price_load_scen() is called by set_prices()
#'
#' @param scen scenario
#' @param start_year initial year
#' @param end_year final year
#'
#' @returns dataframe
#' @export
#'
#' @examples
#' get_price_load_scen(sD)
get_price_load_scen <- function(scen,start_year=2019,end_year=2040){

  load_profiles_1 <- depmicrosimr::load_profiles %>% dplyr::mutate(mdh = format(datetime, "%m-%d-%H")) %>% dplyr::select(-datetime,-day_note)

  prices_scen <- get_sem_prices(scen,start_year,end_year)
  prices_scen %>%
    dplyr::mutate(mdh = format(datetime, "%m-%d-%H")) %>%
    dplyr::inner_join(load_profiles_1, by = "mdh") %>%
    dplyr::select(-mdh)
}

#' get_annual_cost
#'
#' evaluates the annual electricity cost of tariff scheme at yeartime for an agent with known natural load profile and flexibility parameters.\cr
#' \cr
#' At present the tariff scheme are flat, day/night/peak and dynamic and the characteristic load profile is LP1 or LP3. Future price assumptions
#' are typically the output of get_tariff_prices(scenario)
#'
#' @param yeartime decimal time
#' @param kWh annual consumption kWh (assumed inflexible)
#' @param tariff_plan flat, tou or dynamic
#' @param phi inflexible fraction
#' @param gamma cost penalty parameter
#' @param eta kinetic regularisation (currently fixed at 0.1)
#' @param tau energy consumption mean reversion time
#' @param natural_profile characteristic household usage profile e.g. LP1
#' @param prices_scen tariff price scenario
#'
#' @returns a real number, euros
#' @export
#'
#' @examples
#'
#' prices_scen <- set_prices(sD)
#' get_annual_cost(2026,8760,"tou",0.5,1,1,48,"LP1",prices_scen)
#' get_annual_cost(2030,8760,"dynamic",0.25,1,0.2,96,"LP3",prices_scen) #
#' get_annual_cost_simple(2030,8760,"dynamic","LP3",prices_scen) #
get_annual_cost <- function(yeartime, kWh, tariff_plan, phi=0.5, gamma=0.25, eta=0.1, tau=24,natural_profile="LP1", prices_scen) {
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

  #Scale parameter by the flexible load
  gamma_scaled <- gamma * (8760 / kWh)
  eta_scaled   <- eta * (8760 / kWh)

  # 5. Calculate flexible loads conditionally
  if (tariff_plan != "flat") {
    df <- get_flex(df, phi, gamma_scaled, eta_scaled, tau)
    bill_inflex <- sum(df$price * df$load)
    bill_flex   <- sum(df$price * df$load_opt)
    print(paste("flexibility score", 100*mad(df$load_opt/df$load-1)))
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
      tariff_plan            = tariff_plan
    )
  )
}


#' get_annual_cost_simple
#'
#' evaluates the annual electricity cost of tariff scheme at yeartime for an agent based on standard profiles i.e. no flexibility modelling. This function
#' is used by initialise_agents() and is also useful
#' \cr
#' At present the tariff scheme are flat, day/night/peak and dynamic and the characteristic load profile is LP1 or LP3. Future price assumptions
#' are typically the output of get_tariff_prices(scenario)
#'
#' @param yeartime decimal time
#' @param kWh annual consumption kWh (assumed inflexible)
#' @param tariff_plan flat, tou or dynamic
#' @param profile household usage profile e.g. LP1
#' @param prices_scen tariff price scenario
#'
#' @returns a real number, euros
#' @export
#'
#' @examples
#'
#' prices_scen <- set_prices(sD)
#' get_annual_cost_simple(2019,4000,"tou_old","LP2",prices_scen=prices_scen)
#' get_annual_cost_simple(2030,8760,"tou_old","LP2",prices_scen=prices_scen)
#' get_annual_cost_simple(2030,8760,"tou","LP1",prices_scen=prices_scen)
#' get_annual_cost_simple(2030,8760,"flat","LP1",prices_scen=prices_scen)
#' get_annual_cost_simple(2030,8760,"dynamic","LP1",prices_scen=prices_scen)
get_annual_cost_simple <- function(yeartime, kWh, tariff_plan, profile="LP1", prices_scen) {

  stopifnot(tariff_plan %in% c("flat","tou","tou_old","dynamic"))
  profile <- tolower(profile)
  stopifnot(profile %in% c("lp1","lp2","lp3","lp4"))
  #one year
  start_time <- lubridate::date_decimal(yeartime)
  end_time   <- lubridate::date_decimal(yeartime + 1)
  tariff_plan <- if(tariff_plan=="tou_old") {"tou"} else (tariff_plan)
  #datetime range
  df <- prices_scen %>%
    dplyr::filter(
      tariff_plan == .env$tariff_plan,
      datetime >= start_time,
      datetime <= end_time
    )

  #scale
  df$load <- df[[profile]] * kWh

  return(sum(df$price * df$load))

}


#' set_prices
#'
#' set_prices() is a *retail* electricity tariff pricing model based on wholesale prices and network charges. The purpose of
#' this simple model is to have a consistent set of flat, day/night/peak and dynamic prices in future projections and also to reproduce historic
#' prices with reasnable accuracy. Without this sub-model, with ad-hoc assumptions for future tariff plan pricing, the ABM projections would not be meaningful because they would depend
#' on specific assumptions. This is equivalent to the assumption that network supplier set their prices so that the average price paid by
#' customers on flat, ToU and dynamic prices are similar across standard profiles.\cr
#' \cr
#' Regulated network charges are ToU dependent and yeartime dependent. Charges at yeartime in a scenario are obtained from
#' peak_network_charge_fun(scen,yeartime) etc. There is a supplier uplift on top of the regulated network charge. The model assumes that
#' wholesale prices + VAT are passed through.\cr
#' \cr
#' set_prices() is run at the beginning of each run.\cr
#' \cr
#' Optionally, the price cap (currently 0.5) can be turned off.
#'
#'
#'
#' @param scen scenario
#' @param cru_cap Boolean, defaults to TRUE
#'
#' @returns
#' @export
#'
#' @examples
#' set_prices(sD)
set_prices <- function(scen,cru_cap=TRUE){
  #
  midyear <- function(year,tariff) {
    #a function to identify the decimal date "mid_year" for day/night/peak hours
    dplyr::case_when(tariff=="night"~lubridate::decimal_date(lubridate::ymd_hms(paste(year,"-07-02 00:00:00",sep=""))),
                                                     tariff=="day"~lubridate::decimal_date(lubridate::ymd_hms(paste(year,"-07-02 08:00:00",sep=""))),
                                                     tariff=="peak"~lubridate::decimal_date(lubridate::ymd_hms(paste(year,"-07-01 17:00:00",sep="")))
  )}
  #sem prices
  prices <- get_price_load_scen(scen) %>% dplyr::select(-tou)

  prices <- prices %>% dplyr::mutate(hour=lubridate::hour(datetime)) %>% dplyr::inner_join(tou_tariffs %>% dplyr::rename("hour"=start),by="hour") %>% dplyr::rename("sem"=price)

  prices <- prices %>% dplyr::mutate(y=lubridate::decimal_date(datetime),network_price=dplyr::case_when(tariff=="night"~night_network_charge_fun(scen,y),
                                                      tariff=="day"~day_network_charge_fun(scen,y),
                                                      tariff=="peak"~peak_network_charge_fun(scen,y)))
  #assume price cap scales with trend sem price
  if(cru_cap) {
    prices <- prices %>% dplyr::mutate(dynamic_cap=0.5*sem_trend_price(scen, lubridate::decimal_date(datetime))/sem_trend_price(scen,2026.5))
    prices <- prices %>% dplyr::mutate(price=pmin(dynamic_cap,sem)+network_price) #%>% dplyr::select(-dynamic_cap)
  } else{
    prices <- prices %>% dplyr::mutate(price=sem+network_price)
  }

  #dynamic prices
  dyn_prices <- prices %>% dplyr::select(datetime,tariff,price) %>% dplyr::mutate(tariff_plan="dynamic")
  #################################
  # a somewhat speculative guess about how network suppliers arrive at their flat tariff (commodity swap price)
  # mean flat prices paid by year
  #################################
  flat_prices <- prices %>% dplyr::group_by(year=lubridate::year(datetime)) %>% dplyr::summarise(dynamic_lp1=sum(lp1*price),dynamic_lp2=sum(lp2*price),
                                                                        dynamic_lp3=sum(lp3*price),dynamic_lp4=sum(lp4*price))
  #average over load profiles
  flat_prices <- flat_prices %>% tidyr::pivot_longer(-year,names_to="profile",values_to="price")
  flat_prices <- flat_prices %>% dplyr::group_by(year) %>% dplyr::summarise(price=mean(price))
  flat_prices <- flat_prices %>% dplyr::inner_join(tidyr::expand_grid(year=flat_prices$year,tariff=c("day","night","peak")),by="year")
  flat_prices <- flat_prices %>% dplyr::mutate(yeartime=midyear(year,tariff)) %>% dplyr::ungroup() %>% dplyr::select(-year)

  #tou prices
  tou_prices <- prices %>% dplyr::group_by(year=lubridate::year(datetime),tariff) %>% dplyr::summarise(dynamic_lp1=sum(lp1*price)/sum(lp1),dynamic_lp2=sum(lp2*price)/sum(lp2),
                                                                              dynamic_lp3=sum(lp3*price)/sum(lp3),dynamic_lp4=sum(lp4*price)/sum(lp4))
  #
  tou_prices <- tou_prices %>% tidyr::pivot_longer(c(-year,-tariff),names_to="profile",values_to="price")
  tou_prices <- tou_prices %>% dplyr::group_by(year,tariff) %>% dplyr::summarise(price=mean(price)) %>% dplyr::ungroup()
  #key yeartimes (taking July 2 as middle day)

  tou_prices <- tou_prices %>% dplyr::mutate(yeartime=midyear(year,tariff)) %>% dplyr::ungroup() %>% dplyr::select(-year)

  ts <- prices %>% dplyr::select(datetime,tariff) %>% dplyr::mutate(yeartime=lubridate::decimal_date(datetime))
  #
  flat_prices <- ts %>% dplyr::left_join(flat_prices,by=c("tariff","yeartime")) %>% dplyr::mutate(price = zoo::na.approx(price, rule = 2))
  flat_prices$tariff_plan <- "flat"
  flat_prices <- flat_prices %>% dplyr::select(-yeartime)
  #
  #ts <- prices %>% select(datetime,tariff) %>% mutate(yeartime=decimal_date(datetime))
  tou_prices <- ts %>% dplyr::left_join(tou_prices,by=c("tariff","yeartime")) %>% dplyr::arrange(tariff,yeartime) %>% dplyr::group_by(tariff) %>% dplyr::mutate(price = zoo::na.approx(price, rule = 2))
  tou_prices$tariff_plan <- "tou"
  tou_prices <- tou_prices %>% dplyr::select(-yeartime)

  result <- dplyr::bind_rows(flat_prices,tou_prices,dyn_prices) %>% dplyr::inner_join(depmicrosimr::load_profiles_generalised,by=c("tariff","datetime"))
  #apply VAT to all prices
  result <- result %>% dplyr::inner_join(ts %>% dplyr::select(-tariff),by="datetime")
  result %>% dplyr::mutate(price=(1+vat_rate_fun(scen,yeartime))*price)
}


#' net_prices
#'
#' net_price(scen) returns the network charge assumptions
#'
#' Regulated network charges are ToU dependent and yeartime dependent. Charges at yeartime in a scenario are obtained from
#' peak_network_charge_fun(scen,yeartime) etc. There is a supplier uplift on top of the regulated network charge. The model assumes that
#' wholesale prices + VAT are passed through.
#'
#'
#' @param scen scenario
#'
#' @returns
#' @export
#'
#' @examples
#' net_prices(sD)
net_prices <- function(scen){
  #
  midyear <- function(year,tariff) {
    #a function to identify the decimal date "mid_year" for day/night/peak hours
    dplyr::case_when(tariff=="night"~lubridate::decimal_date(lubridate::ymd_hms(paste(year,"-07-02 00:00:00",sep=""))),
                     tariff=="day"~lubridate::decimal_date(lubridate::ymd_hms(paste(year,"-07-02 08:00:00",sep=""))),
                     tariff=="peak"~lubridate::decimal_date(lubridate::ymd_hms(paste(year,"-07-01 17:00:00",sep="")))
    )}
  #sem prices
  prices <- depmicrosimr::load_profiles_generalised %>% dplyr::select(datetime,tariff)

  prices <- prices %>% dplyr::mutate(y=lubridate::decimal_date(datetime),network_price=dplyr::case_when(tariff=="night"~night_network_charge_fun(scen,y),
                                                                                                        tariff=="day"~day_network_charge_fun(scen,y),
                                                                                                        tariff=="peak"~peak_network_charge_fun(scen,y)))
  #
  prices %>% dplyr::select(-y)

}


#' vat_rate_fun
#'
#' @param scen the scenario
#' @param yeartime decimal time
#'
#' @returns a scalar
#' @export
#'
#' @examples
#' vat_rate_fun(sD,2023)
vat_rate_fun <- function(scen,yeartime){
  #
  rates <- scen %>% dplyr::filter(parameter %in% c("old_vat_rate","new_vat_rate","post_2030_vat_rate")) %>% dplyr::pull(value)
  approx(x=c(2019,2022.4,2031), y=rates,xout=yeartime,method="constant",rule=2)$y %>% return()
}


#' tariff_plan_bills
#'
#' @param kWh annual load
#' @param phi \eqn{\phi}
#' @param gamma \eqn{\gamma}
#' @param eta \eqn{\eta}
#' @param tau \eqn{\tau}
#' @param natural_profile the characteristic profile of the household (currently LP1 or LP3)
#' @param yeartime current decimal time
#' @param smart_rollout smart meter rollout time
#' @param prices_scen price scenario
#'
#' @returns dataframe
#' @export
#'
#' @examples
#' prices_scen <- set_prices(sD)
#' tariff_plan_bills(8760,0.25,1,0.2,48,"LP1",2030,2025,prices_scen)
#' tariff_plan_bills(8760,0.25,1,0.2,48,"LP1",2025.5,2025,prices_scen)
#' tariff_plan_bills(8760,0.25,1,0.2,48,"LP1",2020,2025,prices_scen)
tariff_plan_bills <- function(kWh,phi,gamma,eta,tau,natural_profile="LP1",yeartime,smart_rollout,prices_scen){

  stopifnot(tolower(natural_profile) %in% c("lp1","lp3"))
  plans <- if (yeartime >= 2026.5) {
    c("flat", "tou", "dynamic")
    } else if (yeartime < smart_rollout) {
    c("flat")
    } else {
    c("flat", "tou")
  }
  df <- tibble::tibble()
  for(plan in plans){
   df0 <- get_annual_cost(yeartime, kWh, plan, phi, gamma, eta, tau, natural_profile, prices_scen) |> dplyr::select(tariff_plan,annual_bill_flexible)
   df <- df |> dplyr::bind_rows(df0)
  }
  df <- df %>% dplyr::rename("annual_bill"=annual_bill_flexible)
  return(df %>% dplyr::arrange(annual_bill))
}

