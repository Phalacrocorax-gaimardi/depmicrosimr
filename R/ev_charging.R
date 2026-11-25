#ev_surv <- readr::read_csv("inst/ext_data/ev_surv.csv")

# home charging for depmicrosimr

# EV charging is modelled as fully flexible while the vehicle is at home: there is no "natural" EV
# profile and no behavioural (kernel/ramping) penalty. The household minimises its bill subject to
#   state of charge:  e[t+1] = e[t] + c_eta * q[t] - u[t]
#   battery:          0 <= e[t] <= battery_kwh
#   charger/home:     0 <= q[t] <= charger_kw * home[t]   (and household import limit MIC)
#   energy:           e returns to its starting level at the end of every week-long block
# where u[t] is the driving energy drawn from the battery (only in hours when the car is away).
# Households on a flat tariff have no price signal and charge on arrival (uncontrolled charging).

#' ev_home_hours
#'
#' Logical vector, TRUE when the vehicle is at home and can charge.
#' The car is away between \code{dep} and \code{arr} on weekdays and, if \code{wkend_home} is FALSE,
#' also at weekends in the same pattern. If \code{wkend_home} is TRUE it is home all day on Saturday and Sunday.
#'
#' @param datetime hourly POSIXct vector
#' @param arr hour of day (0-23) the vehicle arrives home
#' @param dep hour of day (0-23) the vehicle departs
#' @param wkend_home logical, at home all day at weekends
#'
#' @returns logical vector
#' @noRd
ev_home_hours <- function(datetime, arr, dep, wkend_home = FALSE) {
  hod <- lubridate::hour(datetime)
  wd <- lubridate::wday(datetime, week_start = 1)
  home <- (hod >= arr) | (hod < dep)
  if (wkend_home) home[wd >= 6] <- TRUE
  home
}

#' ev_drive_energy
#'
#' Driving energy removed from the battery in each hour. The annual grid energy \code{ev_kwh} is spread
#' evenly over the away hours of each block (default one week), scaled by block length, and multiplied
#' by the charging efficiency \code{c_eta} (battery side).
#'
#' @param home logical home vector from \code{ev_home_hours()}
#' @param ev_kwh annual EV electricity use at the grid (kWh)
#' @param c_eta charging efficiency
#' @param block integer block index per hour (see \code{ev_blocks()})
#'
#' @returns numeric vector (kWh per hour)
#' @noRd
ev_drive_energy <- function(home, ev_kwh, c_eta, block) {
  u <- numeric(length(home))
  for (b in unique(block)) {
    idx <- which(block == b)
    away <- idx[!home[idx]]
    if (length(away) > 0) u[away] <- c_eta * ev_kwh * length(idx) / 8760 / length(away)
  }
  u
}

#' ev_blocks
#'
#' Block index for each of n hours. The weekly energy requirement applies within each block.
#' @param n number of hours
#' @param block_hours hours per block, default 168 (one week)
#' @returns integer vector
#' @noRd
ev_blocks <- function(n, block_hours = 168) (seq_len(n) - 1) %/% block_hours + 1

#' ev_charge_uncontrolled
#'
#' Charge on arrival: the vehicle charges at full power whenever it is at home and below its starting
#' charge \code{e0}. This is the response to a flat tariff (no price signal).
#'
#' @param home logical home vector
#' @param u driving energy per hour
#' @param charger_kw charger power (kW)
#' @param c_eta charging efficiency
#' @param e0 starting (target) state of charge in kWh at the start of each block
#' @param q_cap optional hourly upper limit on charging (kW), e.g. household import limit minus other load
#' @param block block index per hour
#'
#' @returns charging power (kW) per hour
#' @noRd
ev_charge_uncontrolled <- function(home, u, charger_kw, c_eta, e0, q_cap = NULL, block) {
  n <- length(home)
  if (is.null(q_cap)) q_cap <- rep(Inf, n)
  q <- numeric(n)
  for (b in unique(block)) {
    e <- e0
    for (t in which(block == b)) {
      if (home[t]) q[t] <- min(charger_kw, q_cap[t], max(0, (e0 - e) / c_eta))
      e <- max(0, e + c_eta * q[t] - u[t])
    }
  }
  q
}



# get_flex2: household load-shifting with a fully flexible EV in one QP
# Licensed under the Apache License, Version 2.0

# internal: penalty matrix for the non-EV shift x (identical to the one built in get_flex())
flex_penalty_matrix <- function(T_total, load_total, gamma, eta, tau, kernel) {
  W <- ceiling(5 * tau)
  lags <- 0:W
  plags <- sqrt(5) * lags / (0.8385 * tau)
  kernel_values <- if (kernel == "matern") {
    (1 + plags + (plags^2) / 3) * exp(-plags)
  } else if (kernel == "cauchy") {
    1 / (1 + (lags / (0.6366 * tau))^2)
  } else if (kernel == "exp") {
    exp(-(lags / tau))
  } else {
    exp(-(lags / (1.128 * tau))^2)
  }
  frob_sq <- sum(kernel_values^2) + sum(kernel_values[-1]^2)
  parameter_scaling <- 8760 / load_total
  eta_scaled <- eta * parameter_scaling
  gamma_scaled <- gamma / frob_sq * parameter_scaling
  K <- Matrix::bandSparse(T_total, k = lags,
                          diag = lapply(kernel_values, function(v) rep(v, T_total)),
                          symmetric = TRUE)
  P_kern <- Matrix::crossprod(K)
  Dm <- Matrix::sparseMatrix(i = rep(1:(T_total - 1), each = 2),
                             j = as.vector(rbind(1:(T_total - 1), 2:T_total)),
                             x = rep(c(-1, 1), T_total - 1),
                             dims = c(T_total - 1, T_total))
  P <- gamma_scaled * P_kern + eta_scaled * Matrix::crossprod(Dm)
  0.5 * (P + Matrix::t(P))
}

#' get_flex2
#'
#' Like \code{get_flex()}, but for a household that also owns an electric vehicle. The non-EV load shift
#' \eqn{x_t} and the EV charging power \eqn{q_t} are optimised \emph{jointly} in one quadratic programme (OSQP), so
#' the household import limit applies exactly to their sum:
#' \deqn{\min_{x,q,e}\ \sum_t p_t (x_t + q_t) + \tfrac12 x^\top P x + \tfrac{\epsilon}{2}\sum_t q_t^2}
#' subject to
#' \deqn{\sum_t x_t = 0,\qquad -(1-\phi_t)L^0_t \le x_t,\qquad 0 \le q_t \le \bar q\, h_t,\qquad L^0_t + x_t + q_t \le P_{max}}
#' \deqn{e_t = e_{t-1} + \eta_{ev}\, q_t - u_t,\qquad 0 \le e_t \le \bar E,\qquad e_{\rm block\ end} = e_0.}
#' \eqn{P} is the same ramping plus kernel penalty as in \code{get_flex()}, applied to the non-EV load only.
#' EV charging has no natural profile and no behavioural penalty: it is fully flexible while the vehicle is
#' at home (\eqn{h_t=1}). \eqn{u_t} is the driving energy drawn from the battery in hours when the vehicle is
#' away. The battery returns to its starting charge \eqn{e_0} at the end of every block (default one week), so
#' the annual EV energy is exactly \code{ev$kwh}. The small term \eqn{\epsilon} (\code{eps_rel} times the mean
#' absolute price divided by the charger power) is a numerical regularisation, not a behavioural cost. It makes
#' the response unique and continuous in the prices, which matters when the prices include the D_max
#' multiplier in \code{get_capped_profiles()}.
#'
#' If \code{ev} is \code{NULL} the call is passed straight to \code{get_flex()}. If the EV problem is infeasible
#' (battery or charger too small for the driving pattern) a warning is given and the household falls back to
#' \code{get_flex()} plus charging on arrival.
#'
#' @param demand a 3 column dataframe of datetime, hourly prices and natural_load (as for \code{get_flex()}); the
#'   natural load excludes the EV
#' @param ev \code{NULL} or a list with elements \code{kwh} (annual EV grid energy), \code{charger_kw},
#'   \code{battery_kwh}, \code{arr} and \code{dep} (hours of day the vehicle arrives home and leaves),
#'   and optionally \code{wkend_home} (default FALSE), \code{eta} (charging efficiency, default 0.9),
#'   \code{e0_frac} (starting and end-of-block state of charge as a fraction of the battery, default 0.8) and
#'   \code{eps_rel} (default 0.02)
#' @param phi baseload (inflexible) fraction of non-EV load
#' @param gamma unscaled quadratic cost penalty weight
#' @param eta unscaled weight of the kinetic term (regularisation)
#' @param tau flexibility time horizon in hours
#' @param kernel choice of "matern", "exp", "cauchy","gauss"
#' @param precision desired solver precision
#' @param block_hours hours per energy-balance block, default 168 (one week)
#'
#' @returns the dataframe returned by \code{get_flex()} (datetime, price, load, fload, x, load_opt, baseload;
#'   \code{load_opt} is the optimised non-EV load) plus \code{ev_charge} (kW), \code{ev_soc} (kWh, end of hour)
#'   and \code{load_total = load_opt + ev_charge}
#' @export
#'
#' @examples
#' demand <- set_prices(sD) |> dplyr::filter(tariff_plan == "dynamic", lubridate::year(datetime) == 2026)
#' demand <- demand |> dplyr::inner_join(load_profiles_generalised |> dplyr::select(datetime, lp1))
#' demand <- demand |> dplyr::mutate(load = 4200 * lp1) |> dplyr::select(datetime, price, load)
#' ev <- list(kwh = 2500, charger_kw = 7, battery_kwh = 60, arr = 18, dep = 7)
#' res <- get_flex2(demand, ev, phi = 0.4, gamma = 5, eta = 0.3, tau = 48)
get_flex2 <- function(demand, ev = NULL, phi = 0.4, gamma = 5, eta = 0.3, tau = 24, kernel = "exp",precision = 1e-4, block_hours = 168) {

  if (is.null(ev)) return(get_flex(demand, phi, gamma, eta, tau, kernel, precision))
  stopifnot(kernel %in% c("exp", "gauss", "cauchy", "matern"))
  stopifnot(length(phi) == 1, !is.na(phi))
  need <- c("kwh", "charger_kw", "battery_kwh", "arr", "dep")
  if (!all(need %in% names(ev))) stop("ev must contain: ", paste(need, collapse = ", "))
  ev_eta   <- if (is.null(ev$eta)) 0.9 else ev$eta
  wkend    <- if (is.null(ev$wkend_home)) FALSE else ev$wkend_home
  e0_frac  <- if (is.null(ev$e0_frac)) 0.8 else ev$e0_frac
  eps_rel  <- if (is.null(ev$eps_rel)) 0.02 else ev$eps_rel
  stopifnot(ev$charger_kw > 0, ev$battery_kwh > 0, ev$kwh >= 0, e0_frac > 0, e0_frac <= 1)

  P_max <- sD %>% dplyr::filter(parameter == "mic") %>% dplyr::pull(value)
  demand <- demand %>% dplyr::arrange(datetime) %>% dplyr::mutate(hour = lubridate::hour(datetime))
  demand <- demand %>% dplyr::inner_join(diurnal_inflex, by = "hour")
  n <- nrow(demand)

  fully_inflexible <- dplyr::near(phi, 1) || phi > 1
  if (fully_inflexible) {
    demand <- demand %>% dplyr::mutate(phi_t = 1, fload = 0)
  } else {
    demand <- demand %>% dplyr::mutate(phi_t = phi - (1 - phi) * log(1 - phi) * (f - 1),
                                       fload = (1 - phi_t) * load)
  }
  price <- demand$price
  load  <- demand$load

  # EV availability, driving energy, blocks
  block <- ev_blocks(n, block_hours)
  home  <- ev_home_hours(demand$datetime, ev$arr, ev$dep, wkend)
  u     <- ev_drive_energy(home, ev$kwh, ev_eta, block)
  e0    <- e0_frac * ev$battery_kwh
  q_ub  <- ev$charger_kw * home
  is_start <- c(TRUE, block[-1] != block[-n])
  is_end   <- c(block[-1] != block[-n], TRUE)

  # variables z = (x[1:n], q[1:n], e[1:n])
  ix <- 1:n; iq <- n + 1:n; ie <- 2 * n + 1:n
  P_x <- flex_penalty_matrix(n, sum(load), gamma, eta, tau, kernel)
  eps <- eps_rel * max(mean(abs(price)), 1e-3) / ev$charger_kw
  zero_nn <- Matrix::sparseMatrix(i = integer(0), j = integer(0), x = numeric(0), dims = c(n, n))
  P <- Matrix::bdiag(P_x, Matrix::Diagonal(n, eps), zero_nn)
  qvec <- c(price, price, rep(0, n))

  I_n <- Matrix::Diagonal(n)
  Z <- zero_nn
  soc <- Matrix::sparseMatrix(
    i = c(1:n, (1:n)[!is_start], 1:n),
    j = c(ie, ie[-n][!is_start[-1]], iq),
    x = c(rep(1, n), rep(-1, sum(!is_start)), rep(-ev_eta, n)),
    dims = c(n, 3 * n))
  A <- rbind(
    Matrix::sparseMatrix(i = rep(1, n), j = ix, x = 1, dims = c(1, 3 * n)),  # sum x = 0
    cbind(I_n, Z, Z),                                                         # x box
    cbind(Z, I_n, Z),                                                         # q box
    cbind(I_n, I_n, Z),                                                       # import limit
    soc,                                                                      # SoC dynamics
    cbind(Z, Z, I_n)                                                          # e box
  )
  l <- c(0,
         -demand$fload,
         rep(0, n),
         rep(-Inf, n),
         -u + e0 * is_start,
         ifelse(is_end, e0, 0))
  ub <- c(0,
          if (fully_inflexible) rep(0, n) else rep(Inf, n),
          q_ub,
          P_max - load,
          -u + e0 * is_start,
          ifelse(is_end, e0, ev$battery_kwh))
  if (fully_inflexible) l[1 + ix] <- 0

  settings <- osqp::osqpSettings(eps_abs = precision, eps_rel = precision, verbose = FALSE, max_iter = 20000)
  res <- osqp::osqp(P = P, q = qvec, A = A, l = l, u = ub, pars = settings)@Solve()
  status <- res$info$status

  if (!(status %in% c("solved", "solved inaccurate"))) {
    warning("get_flex2: EV problem not solved (", status, "); using get_flex() plus charging on arrival")
    out <- get_flex(demand %>% dplyr::select(datetime, price, load), phi, gamma, eta, tau, kernel, precision)
    q <- ev_charge_uncontrolled(home, u, ev$charger_kw, ev_eta, e0,
                                q_cap = pmax(P_max - out$load_opt, 0), block = block)
    out$ev_charge <- q
    out$ev_soc <- NA_real_
    out$load_total <- out$load_opt + q
    return(out)
  }

  x_opt <- res$x[ix]
  q_opt <- pmin(pmax(res$x[iq], 0), q_ub)
  demand$x <- x_opt
  demand$load_opt <- load + x_opt
  demand$baseload <- demand$phi_t * load
  demand$ev_charge <- q_opt
  demand$ev_soc <- res$x[ie]
  demand$load_total <- demand$load_opt + q_opt
  demand %>% dplyr::select(-hour, -f, -phi_t)
}



# Aggregate residential capacity constraint (D_max) for depmicrosimr
#############

#' get_capped_profiles
#'
#' Hourly aggregate residential load when the sum over all simulated households is constrained by
#' a local-network capacity \eqn{D_{max}} (an aggregate constraint on residential load).\cr
#' \cr
#' Households are no longer independent, so the problem is
#' \deqn{\min \sum_i C_i \quad s.t. \quad \sum_i L_{i,t} \le D_{max}\ \forall t}
#' where \eqn{C_i} is the household cost (the non-EV load-shifting cost optimised by \code{get_flex()}
#' plus the EV charging bill). It is solved by dual decomposition. A multiplier \eqn{\mu_t \ge 0}
#' (euro/kWh) is added to the price seen by every price-responsive household, each household re-solves
#' its problem, and the multiplier is updated by projected (sub)gradient ascent
#' \deqn{\mu_t \leftarrow \max\left(0,\ \mu_t + s\, \bar p\, (D_t - D_{max})/D_{max}\right)}
#' until \eqn{D_t \le D_{max}(1+\mathrm{tol})}.\cr
#' \cr
#' \eqn{\mu_t} is purely a computational device for enforcing the hard constraint (zero cost below the
#' cap, infinite above it). It is never billed: households pay the original prices, and \eqn{\mu_t} is
#' returned only as a diagnostic of where and how strongly the cap binds.\cr
#' \cr
#' \strong{EVs.} If \code{agents} has an \code{ev} column, EV owners also charge. EV charging is fully
#' flexible while the vehicle is at home (see \code{ev_charge_optimal()}), with no natural profile and
#' no behavioural penalty. EV owners on a flat tariff charge on arrival. Each household's total import
#' is limited by the \code{mic} parameter: the EV is optimised after the non-EV load and can use only
#' the remaining capacity.\cr
#' \cr
#' Only households on "tou", "tou_old" or "dynamic" plans respond. Flat-tariff load is fixed, so if
#' that load alone exceeds \eqn{D_{max}} the constraint cannot be met; a warning is given and the
#' residual violation is reported in \code{diagnostics}.
#'
#' @param year integer year (the horizon is the one year used by \code{get_profile()})
#' @param agents dataframe with columns kWh, tariff_plan, phi, gamma, eta, tau, natural_profile.
#'   Optional EV columns: ev (logical), ev_kwh (annual grid energy for the vehicle), ev_charger_kw,
#'   ev_battery_kwh, ev_arr, ev_dep (hours of day), ev_wkend_home (logical), ev_eta (efficiency, default 0.9)
#' @param prices_scen price scenario from \code{set_prices()}
#' @param d_max_per_hh network capacity per simulated household (kW). The aggregate cap is
#'   \code{d_max_per_hh * nrow(agents)}, so results do not depend on the sample size.
#' @param n_iter maximum number of dual iterations
#' @param step dual step size (dimensionless, relative to the mean absolute price)
#' @param tol relative tolerance on the capacity violation
#' @param mu_start optional starting multiplier (e.g. last year's), a vector with one value per hour
#' @param ev_e0_frac starting (and end-of-week) state of charge as a fraction of battery capacity
#' @param use_parallel use \code{parallel::mclapply} (macOS/Linux)
#' @param n_cores number of cores
#' @param quiet if FALSE print the iteration history
#'
#' @returns a list with \code{profile} (datetime, natural, unconstrained, capped aggregate load in kW,
#'   cap and multiplier mu; "natural" has no price response and EVs charging on arrival), and
#'   \code{diagnostics}
#' @export
#'
#' @examples
#' #prices_scen <- set_prices(sD)
#' #homogeneoous agents
#' agents <- data.frame(kWh = rep(4200, 50), tariff_plan = "dynamic", phi = 0.4, gamma = 5,
#'                     eta = 0.3, tau = 48, natural_profile = "LP1", ev = rep(c(TRUE, FALSE), 25),
#'                     ev_kWh = 2500, ev_charger_kw = 7, ev_battery_kwh = 60, ev_arr = 18, ev_dep = 7,
#'                     ev_wkend_home = FALSE)
#' #res <- get_capped_profiles(2026, agents, prices_scen, d_max_per_hh = 1.5, use_parallel = FALSE, quiet = FALSE)
get_capped_profiles <- function(year, agents, prices_scen, d_max_per_hh,
                                n_iter = 40, step = 0.5, tol = 1e-3, mu_start = NULL,
                                ev_e0_frac = 0.8, use_parallel = TRUE,
                                n_cores = max(1, parallel::detectCores() - 1),
                                quiet = TRUE) {

  stopifnot(length(d_max_per_hh) == 1, d_max_per_hh > 0)
  agents <- as.data.frame(agents)
  n <- nrow(agents)
  stopifnot(n > 0, all(c("kWh", "tariff_plan", "phi", "gamma", "eta", "tau", "natural_profile") %in% names(agents)))
  stopifnot(all(agents$tariff_plan %in% c("flat", "tou", "tou_old", "dynamic")))
  cap <- d_max_per_hh * n
  P_max <- sD %>% dplyr::filter(parameter == "mic") %>% dplyr::pull(value)

  # --- EV attributes ---------------------------------------------------------------------------
  is_ev <- rep(FALSE, n)
  if ("ev" %in% names(agents)) {
    need <- c("ev_kwh", "ev_charger_kw", "ev_battery_kwh", "ev_arr", "ev_dep", "ev_wkend_home")
    stopifnot(all(need %in% names(agents)))
    is_ev <- !is.na(agents$ev) & agents$ev
    if (!("ev_eta" %in% names(agents))) agents$ev_eta <- 0.9
  }

  # --- base (kWh = 1) hourly demand for each distinct (plan, profile), built once ---------------
  base_demand <- function(plan, natural_profile) {
    profile <- tolower(natural_profile)
    lp <- load_profiles_generalised %>% dplyr::select(datetime, dplyr::any_of(profile))
    df <- prices_scen %>%
      dplyr::inner_join(lp, by = c("datetime", profile)) %>%
      dplyr::filter(tariff_plan == plan)
    df <- df %>%
      dplyr::filter(datetime >= lubridate::date_decimal(year),
                    datetime <= lubridate::date_decimal(year + 1)) %>%
      dplyr::arrange(datetime)
    data.frame(datetime = df$datetime, price = df$price, lp = df[[profile]])
  }
  key <- paste(agents$tariff_plan, tolower(agents$natural_profile), sep = "|")
  bases <- lapply(unique(key), function(k) {
    parts <- strsplit(k, "|", fixed = TRUE)[[1]]
    base_demand(parts[1], parts[2])
  })
  names(bases) <- unique(key)
  T_h <- nrow(bases[[1]])
  stopifnot(all(vapply(bases, nrow, integer(1)) == T_h), T_h > 0)
  datetime <- bases[[1]]$datetime
  block <- ev_blocks(T_h)

  load_nat <- function(i) agents$kWh[i] * bases[[key[i]]]$lp

  # --- EV: home hours, driving energy, uncontrolled (arrival) charging ----------------------------
  ev_home <- ev_u <- ev_e0 <- ev_asap <- vector("list", n)
  for (i in which(is_ev)) {
    ev_home[[i]] <- ev_home_hours(datetime, agents$ev_arr[i], agents$ev_dep[i], agents$ev_wkend_home[i])
    ev_u[[i]]    <- ev_drive_energy(ev_home[[i]], agents$ev_kwh[i], agents$ev_eta[i], block)
    ev_e0[i]     <- ev_e0_frac * agents$ev_battery_kwh[i]
    ev_asap[[i]] <- ev_charge_uncontrolled(ev_home[[i]], ev_u[[i]], agents$ev_charger_kw[i],
                                           agents$ev_eta[i], ev_e0[[i]],
                                           q_cap = pmax(P_max - load_nat(i), 0), block = block)
  }
  load_ref <- function(i) load_nat(i) + if (is_ev[i]) ev_asap[[i]] else 0   # no price response

  responsive <- agents$tariff_plan != "flat"
  natural <- Reduce(`+`, lapply(seq_len(n), load_ref))
  fixed   <- Reduce(`+`, c(list(rep(0, T_h)), lapply(which(!responsive), load_ref)))
  if (max(fixed) > cap)
    warning("load of flat-tariff households alone exceeds the cap in some hours; the cap cannot be met")

  resp_idx <- which(responsive)
  if (length(resp_idx) == 0) {
    D <- natural
    return(list(profile = tibble::tibble(datetime = datetime, natural = natural, unconstrained = D,
                                         capped = D, cap = cap, mu = 0),
                diagnostics = list(iterations = 0, max_violation = max(0, max(D / cap - 1)),
                                   binding_hours = 0, responsive_share = 0)))
  }

  p_ref <- mean(abs(unlist(lapply(unique(key[resp_idx]), function(k) bases[[k]]$price))))
  p_ref <- max(p_ref, 1e-3)

  solve_one <- function(i, mu) {
    pr <- bases[[key[i]]]$price + mu
    d <- data.frame(datetime = datetime, price = pr, load = load_nat(i))
    ev <- if (is_ev[i]) list(kwh = agents$ev_kwh[i], charger_kw = agents$ev_charger_kw[i],
                             battery_kwh = agents$ev_battery_kwh[i], arr = agents$ev_arr[i],
                             dep = agents$ev_dep[i], wkend_home = agents$ev_wkend_home[i],
                             eta = agents$ev_eta[i], e0_frac = ev_e0_frac) else NULL
    res <- get_flex2(d, ev, phi = agents$phi[i], gamma = agents$gamma[i], eta = agents$eta[i],
                     tau = agents$tau[i])
    if (is_ev[i]) res$load_total else res$load_opt
  }
  total_response <- function(mu) {
    res <- if (use_parallel && .Platform$OS.type != "windows") {
      parallel::mclapply(resp_idx, solve_one, mu = mu, mc.cores = n_cores, mc.preschedule = FALSE)
    } else {
      lapply(resp_idx, solve_one, mu = mu)
    }
    bad <- vapply(res, function(r) !is.numeric(r) || length(r) != T_h, logical(1))
    if (any(bad)) stop("household optimisation failed for ", sum(bad), " household(s)")
    fixed + Reduce(`+`, res)
  }

  mu <- if (is.null(mu_start)) rep(0, T_h) else mu_start
  stopifnot(length(mu) == T_h, all(mu >= 0))

  D_unc <- NULL
  for (k in seq_len(n_iter)) {
    D <- total_response(mu)
    if (k == 1 && all(mu == 0)) D_unc <- D
    viol <- D / cap - 1
    if (!quiet) message(sprintf("iter %2d: max violation %.4f, binding hours %d, max mu %.4f",
                                k, max(viol), sum(viol > tol), max(mu)))
    if (max(viol) <= tol) break
    mu <- pmax(0, mu + step * p_ref * viol)
  }
  if (is.null(D_unc)) D_unc <- total_response(rep(0, T_h))

  list(profile = tibble::tibble(datetime = datetime, natural = natural,
                                unconstrained = D_unc, capped = D, cap = cap, mu = mu),
       diagnostics = list(iterations = k, max_violation = max(D / cap - 1),
                          binding_hours = sum(D / cap - 1 > -tol), peak_unconstrained = max(D_unc),
                          peak_capped = max(D), cap = cap,
                          responsive_share = length(resp_idx) / n, ev_share = mean(is_ev)))
}


# Bass-type EV adoption for depmicrosimr
# Licensed under the Apache License, Version 2.0

#' bass_ev_params
#'
#' Bass diffusion parameters for the share of \emph{car-owning} households that have an EV (ceiling 100%),
#' calibrated to two points of the adoption curve: share \code{share_start} in \code{year_start} and share
#' \code{share_end} in \code{year_end}.\cr
#' \cr
#' The Bass curve with launch time \eqn{t_0} (zero adoption) is
#' \deqn{F(t) = \frac{1-e^{-(p+q)(t-t_0)}}{1+(q/p)\,e^{-(p+q)(t-t_0)}}}
#' where \eqn{p} is the innovation (external influence) coefficient and \eqn{q} the imitation (internal influence)
#' coefficient, both per year. Writing \eqn{r=q/p} and \eqn{s=p+q}, each calibration point gives
#' \eqn{s\,(t-t_0)=\ln\frac{1+rF}{1-F}}, so eliminating \eqn{s} leaves one equation in \eqn{r}, solved with
#' \code{uniroot()}. Two points fix only two parameters, so the launch time \eqn{t_0} must be chosen. It changes
#' the shape between the two points, not the points themselves.
#' A solution with \eqn{q>0} exists only if \code{share_start} is below the share a pure-innovation curve through
#' the end point would give; otherwise an error is raised.\cr
#' \cr
#' Use the observed share in the data for \code{share_start}, e.g. \code{mean(agents$has_ev[agents$has_car])}.
#'
#' @param share_end share of car-owning households with an EV in \code{year_end}; no default
#' @param share_start share in \code{year_start}, default 0.032
#' @param year_start year of \code{share_start}, default 2025
#' @param year_end year of \code{share_end}, default 2041
#' @param t0 launch time of the technology (share zero), default 2010
#'
#' @returns a list with elements \code{p}, \code{q}, \code{t0}
#' @export
#'
#' @examples
#' params <- bass_ev_params(share_end = 0.5)   # 3.2% in 2025, 50% in 2041
#' params
bass_ev_params <- function(share_end, share_start = 0.032, year_start = 2025, year_end = 2041, t0 = 2010) {
  stopifnot(share_start > 0, share_start < share_end, share_end < 1,
            t0 < year_start, year_start < year_end)
  t1 <- year_start - t0
  t2 <- year_end - t0
  h <- function(lr) {
    r <- exp(lr)
    t2 * log((1 + r * share_start) / (1 - share_start)) - t1 * log((1 + r * share_end) / (1 - share_end))
  }
  if (h(-20) >= 0)
    stop("share_start is too high for a Bass curve with q > 0: it must be below ",
         round(1 - (1 - share_end)^(t1 / t2), 4), " for t0 = ", t0)
  r <- exp(stats::uniroot(h, c(-20, 20), tol = 1e-12)$root)
  s <- log((1 + r * share_end) / (1 - share_end)) / t2
  p <- s / (1 + r)
  list(p = p, q = r * p, t0 = t0)
}

# Probability that a non-owner adopts during a step of length dt, when the current share of owners is f_cur.
# Exact for the Bass ODE dF/dt = (p + qF)(1 - F) started at F = f_cur: the share f_cur is mapped to the
# equivalent point on the Bass curve, advanced by dt, and converted to a conditional adoption probability.
#' @noRd
bass_step_prob <- function(f_cur, dt, params) {
  if (f_cur >= 1) return(0)
  s <- params$p + params$q
  r <- params$q / params$p
  E <- (1 - f_cur) / (1 + r * f_cur) * exp(-s * dt)
  f_next <- (1 - E) / (1 + r * E)
  1 - (1 - f_next) / (1 - f_cur)
}

# Placeholder distributions for EV characteristics; replace with data
#' @noRd
draw_ev_attributes <- function(n) {
  data.frame(
    ev_kwhp100km         = rnorm(n,15,1),     #mean is 15
    ev_charger_kw  = 7,
      #sample(c(3.7, 7, 11), n, replace = TRUE, prob = c(0.2, 0.7, 0.1)),
    ev_battery_kwh = sample(c(40, 60, 80), n, replace = TRUE, prob = c(0.3, 0.5, 0.2)),
    ev_arr         = pmin(23, pmax(15, round(stats::rnorm(n, 18, 1.5)))),
    ev_dep         = pmax(5, pmin(10, round(stats::rnorm(n, 7.5, 1)))),
    ev_wkend_home  = stats::runif(n) < 0.5,
    ev_eta         = 0.9
  )
}


#' update_ev_adoption
#'
#' Sets EV ownership at \code{yeartime}, consistent with ownership observed in \code{year_obs}. It can be called
#' for every ABM step from the start year (e.g. 2019), before or after \code{year_obs}. \code{yeartime} is the end
#' of the step and \code{dt} its length in years (bi-monthly steps: \code{1/6}).\cr
#' \cr
#' \strong{Data.} \code{agents} must have logical columns \code{has_car} and \code{has_ev}. On the \emph{first}
#' call \code{has_ev} must be the ownership observed in \code{year_obs} (default 2025). The function then
#' stores an adoption year \code{ev_year} for every owner and thereafter \code{has_ev} means ownership at the
#' time of the call.\cr
#' \cr
#' \strong{Before \code{year_obs} (backcast).} Adoption is permanent, so owners in an earlier year are a subset
#' of the owners in \code{year_obs}. Treating adoption times as drawn from the Bass distribution, the adoption
#' time of an owner observed in \code{year_obs} is distributed with cdf \eqn{F(t)/F_{obs}}, where \eqn{F_{obs}} is
#' the observed share. Each owner is given such an adoption year (drawn by inverse transform), and
#' \code{has_ev} at \code{yeartime} is \code{ev_year <= yeartime}. The probability that an owner in \code{year_obs}
#' already owned an EV earlier is therefore \eqn{F(t)/F_{obs}}. If the observed share differs from the calibrated
#' one, the Bass curve is shifted in time to pass through the observed share.\cr
#' \cr
#' \strong{After \code{year_obs} (forward).} Only households with a car and no EV can adopt, and owners never
#' discard their EV. Households without a car stay without one. The share is the share of \emph{car-owning}
#' households with an EV. The adoption probability over a step is the same for every eligible household and is
#' computed from the current share of owners among car owners by integrating the Bass equation exactly, so the
#' path reaches the calibrated end share only if \code{share_start} was calibrated to the observed share.\cr
#' \cr
#' Every household that ever owns an EV in the simulation, including owners observed in \code{year_obs}, is given
#' annual energy, battery, charger, arrival/departure, weekend presence and efficiency. The distributions in
#' \code{draw_ev_attributes()} are placeholders and should be replaced with data.
#'
#' @param agents agents dataframe with logical columns \code{has_car} and \code{has_ev}
#' @param yeartime decimal year at the end of the step
#' @param dt step length in years, default 1/6
#' @param bass_params output of \code{bass_ev_params()}
#' @param year_obs year in which \code{has_ev} and \code{has_car} are observed, default 2025
#'
#' @returns \code{agents} with \code{has_ev} at \code{yeartime}, \code{ev_year}, and for owners the characteristics
#'   \code{ev_kwhp100km}, \code{ev_charger_kw}, \code{ev_battery_kwh}, \code{ev_arr}, \code{ev_dep},
#'   \code{ev_wkend_home}, \code{ev_eta}
#' @export
#'
#' @examples
#' prices_scen <- set_prices(sD)
#' social_network <- make_artificial_society(dep_society_1,homophily,nu=4.5)
#' agents <- initialise_agents(sD, 2019, prices_scen, social_network)
#' share_obs   <- mean(!is.na(agents$ev_year[agents$has_car]))   # 39/994, not mean(has_ev)
#' bass_params <- bass_ev_params(share_end = 0.5, share_start = share_obs)
#' for (yr in seq(2019 + 1/6, 2041, by = 1/6)) agents <- update_ev_adoption(agents, yr, 1/6, bass_params)
#' mean(agents$has_ev[agents$has_car])                              # about 0.5
update_ev_adoption <- function(agents, yeartime, dt = 1 / 6, bass_params, year_obs = 2025) {
  stopifnot(all(c("has_ev", "has_car") %in% names(agents)))
  n <- nrow(agents)

  # first call: has_ev is the ownership observed in year_obs; reconstruct each owner's adoption year
  if (!("ev_year" %in% names(agents))) {
    owns_obs <- !is.na(agents$has_ev) & agents$has_ev
    car_obs <- (!is.na(agents$has_car) & agents$has_car) | owns_obs
    f_obs <- sum(owns_obs) / max(sum(car_obs), 1)
    s <- bass_params$p + bass_params$q
    r <- bass_params$q / bass_params$p
    Finv <- function(u) log((1 + r * u) / (1 - u)) / s        # time since (shifted) launch at share u
    agents$ev_year <- NA_real_
    if (any(owns_obs))
      agents$ev_year[owns_obs] <- year_obs + Finv(stats::runif(sum(owns_obs), 0, f_obs)) - Finv(f_obs)
  }

  if (yeartime <= year_obs) {
    agents$has_ev <- !is.na(agents$ev_year) & agents$ev_year <= yeartime        # backcast
  } else {
    owns <- !is.na(agents$has_ev) & agents$has_ev
    car  <- (!is.na(agents$has_car) & agents$has_car) | owns                    # an EV owner has a car
    dt_eff <- min(dt, yeartime - year_obs)
    f_cur <- if (sum(car) > 0) sum(owns) / sum(car) else 0
    prob <- bass_step_prob(f_cur, dt_eff, bass_params)
    adopt <- car & !owns & (stats::runif(n) < prob)
    agents$has_ev <- owns | adopt
    agents$ev_year[adopt] <- yeartime - dt_eff * stats::runif(sum(adopt))
    #agents$ev_kwh
  }

  attr_cols <- c("ev_kwhp100km", "ev_charger_kw", "ev_battery_kwh", "ev_arr", "ev_dep", "ev_wkend_home", "ev_eta")
  if (!all(attr_cols %in% names(agents))) agents[setdiff(attr_cols, names(agents))] <- NA
  need <- which(!is.na(agents$ev_year) & is.na(agents$ev_kwhp100km))
  if (length(need) > 0) agents[need, attr_cols] <- draw_ev_attributes(length(need))
  agents
}
