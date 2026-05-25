library(tidyverse)
library(ggthemes)
library(patchwork)
library(tsibble)
library(feasts)
library(Matrix)
library(osqp)


test <- read_csv("~/Policy/CAMG/Dynamic Pricing/historical-irish-electricity-prices.csv")
#
sem_prices_2019_2025 <- test %>% filter(year(Timestamp_UTC) %in% c(2019:2025) )
names(sem_prices_2019_2025) <- c("datetime","price")

convert_to_hourly <- function(half_hourly_data) {

  hourly_aggregated <- half_hourly_data %>%
    # 1. Strip the 30-minute mark, rounding down to the top of the hour
    mutate(datetime = floor_date(datetime, unit = "1 hour")) %>%

    # 2. Group by the new hourly index
    group_by(datetime) %>%

    # 3. Calculate the mean price, ignoring any isolated NA values
    summarise(price = mean(price, na.rm = TRUE)) %>%

    # 4. Re-establish a clean tsibble index
    as_tsibble(index = datetime)

  return(hourly_aggregated)
}
sem_prices_2019_2025 <- convert_to_hourly(sem_prices_2019_2025)
prices_2025 <- prices_2025 %>% group_by(date=as.Date(Timestamp_UTC)) %>% summarise(price=mean(Price))


####################################
# HMM on hourly data
#######################################


prices_2025 %>% ggplot(aes(date,price)) + geom_line()
install.packages("depmixS4")
library(depmixS4)
df <- data.frame(price=sem_prices_2023_2025$price)
model <- depmix(price ~ 1, nstates = 2, data = df )

# Fit the model using the EM algorithm
fit_model <- fit(model)
summary(fit_model)

#install.packages("dsa")
install.packages("feasts")
install.packages("tsibble")
library(tsibble)
library(feasts)
library(imputeTS)
#
hourly <- sem_prices_2019_2025 %>% as_tsibble(index = datetime) #%>% mutate(logprice=log(price+10))
hourly <- hourly %>% fill_gaps() #replaces with NAs
hourly <- hourly %>% mutate(price = imputeTS::na_seasplit(price, algorithm = "interpolation", find_frequency = TRUE))
has_gaps(hourly)
#dcmp <- hourly %>% model(STL(price ~ trend(window = 365) + season(period = 90) +season(period = 365))) %>% components()
scale <- IQR(hourly$price)/10
print(scale)
hourly$logprice <- asinh(hourly$price/scale)
mean(hourly$logprice)
dcmp <- hourly %>% model(STL(logprice ~ trend(window = 2001)
                            # + season(period = "1 day",window=30*24+1)
                             + season(period = "1 week",window="periodic")
                             + season(period=8766,window="periodic")
                             ,robust=TRUE)) %>%components()



g1 <- dcmp %>% filter(yday(datetime)==12, year(datetime)==2024) %>% ggplot(aes(datetime,`season_1 week`)) + geom_line()
g2 <- dcmp %>% filter(yday(datetime)==180, year(datetime)==2024) %>% ggplot(aes(datetime,`season_1 week`)) + geom_line()
g1/g2


dcmp[1:8760,] %>% ggplot(aes(datetime,`season_1 week`)) + geom_line()
dcmp %>% ggplot(aes(datetime,season_8766)) + geom_line()
#check that price is the sum of components

dcmp <- dcmp %>% dplyr::select(datetime,logprice,remainder,trend, `season_1 week`,season_8766)
g1 <- dcmp %>% ggplot() + geom_line(aes(datetime,logprice),colour="grey40") +geom_line(aes(datetime,trend),colour="red")+#geom_line(aes(datetime,remainder),colour="yellow") +
  geom_line(aes(datetime,`season_1 week`+season_8766),colour="blue",alpha=0.3) + theme_minimal()
g2 <- dcmp %>% ggplot() +  geom_line(aes(datetime,remainder),colour="grey60",alpha=0.3) + theme_minimal()
g1/g2
#
model <- depmix(remainder ~ 1, nstates = 3, data = data.frame(dcmp))
fit_model <- fit(model)
summary(fit_model)
print(BIC(fit_model))
#
sim_data <- simulate(fit_model, nsim = 1)

# 1. Define how many future steps you want (e.g., 2000 steps)
t1 <- ymd_hms("2026-01-01 00:00:00", tz = "UTC")
t2 <- ymd_hms("2040-12-31 23:00:00", tz = "UTC")
# 2. Generate the hourly sequence using base R's seq() with lubridate's hours(1)
hourly_sequence <- seq(from = t1, to = t2, by = "1 hour")

future_length <- length(hourly_sequence)
dummy_df <- data.frame(detrended = rep(0, future_length))

# 2. Create a new model structure with the new length
extended_model <- depmix(detrended ~ 1, nstates = 3, data = dummy_df)

# 3. Copy the fitted parameters from your original model over to the new one
# (This bypasses fitting, transferring the learned rules to the longer layout)
extended_model <- setpars(extended_model, getpars(fit_model))

# 4. Simulate! This will now yield a series of 2000 steps
long_sim <- simulate(extended_model, nsim = 1)

# Extract your new long series
extended_series <- long_sim@response[[1]][[1]]@y
extended_states <- long_sim@states
# sim_data now contains:
# 1. The sequence of "Hidden States" (State 1, 2, or 3)
# 2. The "Observed" synthetic prices
synthetic_logprices <-  tibble(datetime=hourly_sequence,sim=long_sim@response[[1]][[1]]@y[,1])

###################
# seasonal factor
###################

weekly_lookup <- dcmp %>% as_tibble() %>%
  # Identify unique hour of the week (1 to 168)
  mutate(hour_of_week = (wday(datetime) - 1) * 24 + hour(datetime) + 1) %>%
  group_by(hour_of_week) %>%
  summarise(season_weekly = first(`season_1 week`), .groups = "drop")

annual_lookup <- dcmp %>%
  as_tibble() %>%
  # Identify unique hour of the year (approx 1 to 8766)
  mutate(
    # Use yday * 24 + hour to track the exact solar timeline position
    hour_of_year = (yday(datetime) - 1) * 24 + hour(datetime) + 1
  ) %>%
  group_by(hour_of_year) %>%
  summarise(season_annual = first(season_8766), .groups = "drop")
#
seasonal_logprice <- tibble(datetime=hourly_sequence) %>%
  # Calculate index keys for the future timestamps
  mutate(
    hour_of_week = (wday(datetime) - 1) * 24 + hour(datetime) + 1,
    hour_of_year = (yday(datetime) - 1) * 24 + hour(datetime) + 1
  ) %>%
  # Left join the master lookup profiles
  left_join(weekly_lookup, by = "hour_of_week") %>%
  left_join(annual_lookup, by = "hour_of_year") %>%
  # If a leap year creates an unmapped hour_of_year = 8784,
  # use tidyr::fill() or safely fallback to the closest winter profile
  tidyr::fill(season_annual, .direction = "down") %>%
  # Combine them into your total additive seasonal adjustment
  mutate(
    seasonal = season_weekly + season_annual
  ) %>% dplyr::select(datetime, seasonal)

synthetic_logprices <- synthetic_logprices %>% inner_join(seasonal_logprices)
#make linear trend to 2030 and 2040
trend_logprices <- tibble(datetime=hourly_sequence,trend=NA)
trend_logprices <- trend_logprices %>% mutate(trend=replace(trend, datetime=="2026-01-01 00:00:00",0.881))
#wholesale price 100
trend_logprices <- trend_logprices %>% mutate(trend=replace(trend, datetime=="2030-12-31 23:00:00",asinh(100/scale)))
#wholesale price 200
trend_logprices <- trend_logprices %>% mutate(trend=replace(trend, datetime=="2040-12-31 23:00:00",asinh(100/scale)))
#linearly interp
trend_logprices <- trend_logprices %>% mutate(trend=zoo::na.approx(trend))

synthetic_logprices <- synthetic_logprices %>% inner_join(trend_logprices)

syn_prices <- synthetic_logprices %>% mutate(price=scale*sinh(sim+trend+seasonal)) %>% dplyr::select(datetime,price)
syn_prices <- sem_2019_2025 %>% mutate(regime="historical") %>% bind_rows(syn_prices)

hourly <- sem_prices_2019_2025 %>% as_tsibble(index = datetime) #%>% mutate(logprice=log(price+10))
hourly <- hourly %>% fill_gaps() #replaces with NAs
hourly <- hourly %>% mutate(price = na_seasplit(price, algorithm = "interpolation", find_frequency = TRUE))



decompose_logprices <- function(price_data,scale=42){

  hourly <- price_data %>% as_tsibble(index = datetime) #%>% mutate(logprice=log(price+10))
  hourly <- hourly %>% fill_gaps() #replaces with NAs
  hourly <- hourly %>% mutate(price = imputeTS::na_seasplit(price, algorithm = "interpolation", find_frequency = TRUE))

  stopifnot(!has_gaps(hourly))
  # scale has to be set
  #scale <- sd(hourly$price)/2
  hourly$logprice <- asinh(hourly$price/scale)
  dcmp <- hourly %>% fabletools::model(STL(logprice ~ trend(window = 2001)
                               # + season(period = "1 day",window=30*24+1)
                               + season(period = "1 day",window="periodic")
                               + season(period = "1 week",window="periodic")
                               + season(period=8766,window="periodic")
                               ,robust=TRUE)) %>% fabletools::components()
  dcmp <- dcmp %>% dplyr::rename("season_week"=`season_1 week`) %>% dplyr::rename("season_year"=season_8766) %>% dplyr::rename("season_day"=`season_1 day`)
  dcmp <- dcmp %>% dplyr::select(-season_adjust,-.model)

  dcmp_clean <- dcmp %>%
    as_tibble() %>% # temporarily drop tsibble to do vector math safely
    mutate(
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
dcmp <- decompose_logprices(sem_prices_2019_2025,10)
g1 <- dcmp %>% ggplot() + geom_line(aes(datetime,logprice),colour="grey40") +geom_line(aes(datetime,trend),colour="red")+#geom_line(aes(datetime,remainder),colour="yellow") +
  geom_line(aes(datetime,season_day+season_week+season_year),colour="blue",alpha=0.3) + theme_minimal()
g2 <- dcmp %>% ggplot() +  geom_line(aes(datetime,remainder),colour="grey60",alpha=0.3) + theme_minimal()
g1/g2

dcmp %>% filter(year(datetime)==2024,yday(datetime) %in% 30:40) %>% ggplot()+geom_line(aes(datetime, season_day))+
                                                                                         geom_line(aes(datetime, season_week),colour="red")
generate_logprice_hmm <- function(dcmp,n_states=3){
  #
  model <- depmixS4::depmix(remainder ~ 1, nstates = n_states, data = data.frame(dcmp))
  fit_model <- depmixS4::fit(model)
  summary(fit_model)
  print(paste("BIC score", n_states, "states",BIC(fit_model)))
  return(fit_model)
}

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

scale0 <- median(sem_prices_2019_2025$price)/10
dcmp <- decompose_logprices(sem_prices_2019_2025,scale=scale0)
fit_hmm <- generate_logprice_hmm(dcmp,3)


proj <- simulate_prices(dcmp,fit_hmm,scale=scale0,trend_price_2030=150,trend_price_2040=250)
#
proj %>% ggplot(aes(datetime,price,colour=regime))+geom_line()

post_probs <- posterior(fit_model)
states_implied <- post_probs$state

# Plotting the result
syn_vs_obsv <- tibble(day=1:1096,synthetic=synthetic_prices,observed=dcmp$detrended, obsv_state=states_implied, sim_state=states_sequence)
state_labels_1 <- tibble(obsv_state=c(1,2,3),obsv_state_label=c("RES surplus","RES deficit","Normal"))
state_labels_2 <- tibble(sim_state=c(1,2,3),sim_state_label=c("RES surplus","RES deficit","Normal"))

syn_vs_obsv <- syn_vs_obsv %>% inner_join(state_labels_1) %>% inner_join(state_labels_2)

g1 <- syn_vs_obsv %>% ggplot() + geom_line(aes(day,observed),colour="grey60") + ggtitle("2023-2025 SEM wholesale prices") + scale_y_continuous(limits=c(-200,200))
g2 <- syn_vs_obsv %>% mutate(y=1) %>% ggplot(aes(day, y, fill = obsv_state_label)) + geom_tile(height=0.2) +
  # Use professional colors: Blue (Low), Green (Mid), Red (High)
  scale_fill_canva() +
  # Remove the Y axis and background to make it look like a clean bar
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Implied state (hidden Markov fit)",
    #subtitle = "A: Low/Wind | B: Normal | C: Peak/Scarcity",
    x = "Time (Days)",
    y = ""
  )

g3 <- syn_vs_obsv %>% ggplot() + geom_line(aes(day,synthetic),colour="grey60") + scale_y_continuous(limits=c(-200,200)) + ggtitle("Simulated prices 3-state hidden Markov")
#g3 <- syn_vs_obsv %>% ggplot() + geom_line(aes(day,wind_state), colour="grey70") + ggtitle("Underlying state")
g4 <- syn_vs_obsv %>% mutate(y=1) %>% ggplot(aes(day, y, fill = sim_state_label)) + geom_tile(height=0.5)+
  # Use professional colors: Blue (Low), Green (Mid), Red (High)
  scale_fill_canva() +
  # Remove the Y axis and background to make it look like a clean bar
  theme_minimal() +
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  ) +
  labs(
    title = "Simulated states",
    #subtitle = "A: Low/Wind | B: Normal | C: Peak/Scarcity",
    x = "Time (Days)",
    y = ""
  )

g <- (g1+g3)/(g2+g4) + plot_layout(heights = c(4, 1))
#
export::graph2ppt(g,"~/Policy/CAMG/Dynamic Pricing/markov_sem_prices.ppt")


# Extract parameters
pars <- getpars(fit_model)

# For a 3-state Gaussian model, the parameters are usually:
# States 1-3: [intercept (mean), sd]
mu <- c(pars[13], pars[15], pars[17])
sigma <- c(pars[14], pars[16], pars[18])

# Stationary probabilities (the average time spent in each state)
# You can get these from the transition matrix or simply look at
# the proportion of states in your posterior
pi_weights <- 2*as.numeric(table(posterior(fit_model)$state) / nrow(daily_2025))
# Create a sequence of prices for the X-axis
#
library(ggplot2)

# Create a sequence of prices for the X-axis

x_range <- seq(min(dcmp$detrended), max(dcmp$detrended), length.out = 1000)

# Calculate weighted densities for each state (A, B, C)
plot_data <- data.frame(
  price = rep(x_range, 3),
  Density = c(pi_weights[1] * dnorm(x_range, mean = mu[1], sd = sigma[1]),
              pi_weights[2] * dnorm(x_range, mean = mu[2], sd = sigma[2]),
              pi_weights[3] * dnorm(x_range, mean = mu[3], sd = sigma[3])),
  State = rep(c("Surplus", "Deficit", "Normal"), each = 1000)
)

# Calculate the 'Total' HMM density (the sum of the three)
total_density <- plot_data %>% group_by(price) %>% summarise(Density=sum(Density,na.rm=T))

g <- ggplot() +
  # 1. The actual price data (Background)
  geom_histogram(data = dcmp, aes(x = detrended, y = ..density..),
                 bins = 60, fill = "grey90", color = "grey50",alpha=0.2) +

  # 2. The three individual state Gaussians (Colored Areas)
   geom_area(data = plot_data, aes(x = price, y = Density, fill = State),
               alpha = 0.5, position = "identity") +

  # 3. The Total HMM Fit (Black Line)
  geom_line(data = total_density, aes(x = price, y = Density),
            color = "black", size = 1, linetype = "dashed") +

  scale_fill_canva() +
  theme_minimal() +
  labs(title = "Gaussian Mixture: SEM daily prices 2023-2025",
       subtitle = "Dashed line = Total HMM Density | Shaded areas = State-specific price distributions",
       x = "Price (EUR/MWh)", y = "Density") + scale_x_continuous(limits=c(-150,150))
g
export::graph2ppt(g,"~./Policy/CAMG/Dynamic Pricing/price_mixtures.ppt")

g <- ggplot() +
  # 1. The actual price data (Background)
  geom_histogram(data = dcmp, aes(x = detrended, y = ..density..),
                 bins = 60, fill = "grey90", color = "grey50",alpha=0.2) + theme_minimal() + scale_x_continuous(limits=c(-150,150))
export::graph2ppt(g,"~./Policy/CAMG/Dynamic Pricing/price_histogram.ppt")
###############################
# consumer algorithms (strategies): serial correlatio is critical
#################################

#1. endurance 1 day
#algorithm: observe current state. If normal consumption= D_0
#if RES deficit and tomorrow is normal or surphus, then consume (1-\epsilon_0) D_0 today and (1+epsilon_0) D_0 tomorrow
#if RES surplus and tomorrow is normal or

# Calculate the daily price diffs
price_diffs <- diff(dcmp$detrended)
delta_D <- 5
# Calculate savings assuming you always pick the right direction
# We use 'abs' because we can shift either way (+ or -)
# We take every second value because one shift consumes two days
savings_vector <- abs(price_diffs[seq(1, length(price_diffs), by=2)]) * delta_D

sum(savings_vector)/1000  #58 euros


#####################
# general case : N-day endurance
#####################

library(lpSolve)

# --- Parameters ---
D0 <- 15
D_min <- 10
D_max <- 100       # The 12 kVA hard ceiling
T_endurance <- 4
T_len <- length(prices)
prices <- dcmp$price/1000

get_dr <-function(D0=15,D_min,D_max=100,T_endurance,prices){

 # Buffering Capacity of household (kWh)
 B_max <- T_endurance * (D0 - D_min) # 40 * 15 = 600 kWh
 print(paste("Buffer", B_max))

 # Variables: y_i = Actual Daily Demand (kWh)
 # lpSolve uses y_i >= 0 by default.
 obj <- prices

 # 1. State of Charge (SoC) Constraints
 # SoC_t = Sum_{i=1 to t} (y_i - D0). Must stay within [-B_max, B_max]
 con_mat_soc <- matrix(0, nrow = T_len, ncol = T_len)
 for(i in 1:T_len) { con_mat_soc[i, 1:i] <- 1 }

 # 2. Daily Physical Grid Constraints (D_min <= y_i <= D_max)
 con_mat_daily <- diag(T_len)

 # --- Combine All Constraints ---
 # Final Balance (=), SoC Floor (>=), SoC Ceiling (<=), Daily Floor (>=), Daily Ceiling (<=)
 final_con_mat <- rbind(
  con_mat_soc[T_len, ],
  con_mat_soc,
  con_mat_soc,
  con_mat_daily,
  con_mat_daily
 )

 # RHS offset for the SoC: Sum(y_i) compared to Sum(D0)
 rhs_base_sum <- (1:T_len) * D0

 final_dir <- c("=", rep(">=", T_len), rep("<=", T_len), rep(">=", T_len), rep("<=", T_len))
 final_rhs <- c(
  T_len * D0,                    # Final balance: total demand must match baseline
  rhs_base_sum - B_max,          # Cumulative debt floor (-600 kWh)
  rhs_base_sum + B_max,          # Cumulative surplus ceiling (+600 kWh)
  rep(D_min, T_len),             # Daily floor (0 kWh)
  rep(D_max, T_len)              # Daily ceiling (280 kWh) - THIS PREVENTS THE 1000
 )

 # --- Solve ---
 sol <- lp("min", obj, final_con_mat, final_dir, final_rhs)
 final_demand <- sol$solution
 return(final_demand)
 }

final_demand <- get_dr(15,12,50,8,prices)

cat("Max Demand Observed:", max(final_demand), "kWh\n")
cat("Theoretical Max Demand (D0 + B_max):", D0 + B_max, "kWh\n")
cost_inflexible <- (365/T_len)*sum(D0 * prices)
annual_savings <- (365/T_len)*(sum(D0 * prices) - sum((final_demand) * prices))
percent_savings = savings/sum(D0 * prices)
cat("Annual inflexible cost:", round(cost_inflexible, 2), "EUR\n")
cat("Annual Strategic Savings:", round(annual_savings, 2), "EUR\n")

dcmp$demand <- final_demand
g1 <- dcmp %>% ggplot(aes(date, demand)) +geom_line() + geom_hline(yintercept = 15, linetype="dotted")
#
g2 <- dcmp %>% ggplot(aes(date, price)) +geom_line()
g1/g2

dcmp %>% ggplot(aes(price,demand)) + geom_point(alpha=0.2)
#
dcmp %>%

get_savings <- function(final_demand){

  annual_savings <- (365/T_len)*(sum(D0 * prices) - sum((final_demand) * prices))

}

df <- expand_grid(D_min=0:15,T_endurance=2:10) %>% rowwise() %>% mutate(annual_savings = get_savings(get_dr(15,D_min,D_max=100,T_endurance,prices)))
df <- df %>% mutate(savings_percent=annual_savings/cost_inflexible, buffer=T_endurance * (D0 - D_min))
#
df %>% ggplot(aes(15-D_min,T_endurance,fill=annual_savings))+geom_tile() + theme_minimal() + scale_fill_viridis_c() + scale_y_continuous(breaks = seq(2, 10, by = 1))
df %>% ggplot(aes(buffer,savings_percent,colour=factor(15-D_min)))+geom_point()

#check the demand constrain
library(zoo) # Great for rolling calculations

# Calculate a rolling sum with a window of N
# If the constraint is a "Hard Reset" every N days,
# only the endpoints of the windows in this alignment will be zero.
rolling_check <- rollapply(net_shift, width = T_endurance, by = T_endurance, FUN = sum, align = "left")

print(rolling_check)
#
#use HMM model
D0 <- 15           # Base Daily Demand (kWh)
delta_D <- 5     # Max Daily Shift (kWh)
N_days <- 2       # Endurance Window
prices <- dcmp$price / 1000  # Price in EUR/kWh
T_len <- length(prices)

# We define x = x_positive - x_negative
# This doubles the number of variables to 2 * T_len
obj <- c(prices, -prices)

# 1. Endurance Constraints (Sum of shifts over N days = 0)
# (x_pos_1 - x_neg_1) + (x_pos_2 - x_neg_2) ... = 0
num_windows <- ceiling(T_len / N_days)
con_mat_endurance <- matrix(0, nrow = num_windows, ncol = 2 * T_len)

for(i in 1:num_windows) {
  start <- (i-1) * N_days + 1
  end <- min(i * N_days, T_len)
  con_mat_endurance[i, start:end] <- 1            # Positive parts
  con_mat_endurance[i, (T_len + start):(T_len + end)] <- -1 # Negative parts
}

# 2. Individual Capacity Constraints (x_pos + x_neg <= delta_D)
# This ensures that on any day, the net shift doesn't exceed delta_D
con_mat_capacity <- matrix(0, nrow = T_len, ncol = 2 * T_len)
for(i in 1:T_len) {
  con_mat_capacity[i, i] <- 1
  con_mat_capacity[i, T_len + i] <- 1
}

# Combine Constraints
final_con_mat <- rbind(con_mat_endurance, con_mat_capacity)
final_dir <- c(rep("=", num_windows), rep("<=", T_len))
final_rhs <- c(rep(0, num_windows), rep(delta_D, T_len))

# Solve
sol <- lp("min", obj, final_con_mat, final_dir, final_rhs)

# Reconstruct the Net Shift
net_shift <- sol$solution[1:T_len] - sol$solution[(T_len + 1):(2 * T_len)]

# Calculate results
cost_inflexible <- (365/T_len)*sum(D0 * prices)
annual_savings <- (365/T_len)*(sum(D0 * prices) - sum((D0 + net_shift) * prices))
percent_savings = savings/sum(D0 * prices)
cat("N-Day Strategic Savings:", round(annual_savings, 2), "EUR\n")





###########################
# power spectrum
#############################

# Using the stats package
#use 2023-2025 data
test1 <- test %>% mutate(date=as.Date(Timestamp_UTC), year=year(date)) %>% dplyr::filter(year >=2023)
hourly_detrend <- test1$Price #- mean(test1$Price)


library(gsignal)

# fs = 1 (1 sample per hour)
# window = 2048 (approx 85 days)
pwc <- pwelch(hourly_detrend,
              fs = 1,
              window = 2048,
              overlap = 0.5)

# Plotting on a Linear Scale
# We use xlim to zoom into the "Operational DR" frequency range (0 to 0.1)
plot(pwc$freq, pwc$spec, type = "l",
     xlab = "Frequency (cycles/hour)",
     ylab = "Power S(w)",
     log="y",
     #main = "Linear Power Spectral Density (Hourly Prices)",
     main = "Log Power Spectral Density (Hourly Prices)",
     col = "darkblue",
     xlim = c(0, 0.5)) # Zooms into periods > 10 hours

# Add markers for key physical cycles
abline(v = 1/24, col = "red", lty = 2)
abline(v = 1/12, col = "pink", lty = 2)
abline(v = 1/6, col = "yellow", lty = 2)
abline(v = 1/8, col = "brown", lty = 2) # 24h Daily Cycle
abline(v = 1/48, col = "orange", lty = 2)
abline(v = 1/168, col = "darkgreen", lty = 2) # 168h Weekly Cycle
grid()

omega <- 2 * pi * pwc$freq
tau <- 48
transfer_func_sq <- (omega * tau)^2 / (1 + (omega * tau)^2)

# Apply to the Spectrum
s_filtered <- pwc$spec * transfer_func_sq

plot(pwc$freq, s_filtered, type = "l", col = "darkred",
     xlim = c(0, 0.5), # Focus on hours to ~10 days
     xlab = "Frequency (cycles/hour)",
     ylab = "Filtered Power S(w)",
     main = paste("DR-Filtered Power Spectrum (tau =", tau, "hrs)"))


# 1. Calculate the frequency step (delta omega)
dw <- pwc$freq[2] - pwc$freq[1]

# 2. Integrate the filtered spectrum (Sum of Power * df)
# This gives the total variance in (Euro/MWh)^2

cost_savings <- function(tau, mean_demand=8, epsilon_0 = 1){
  #
  transfer_func_sq <- epsilon_0*(omega * tau)^2/ (1 + (omega * tau)^2)
  #
  # Apply to the Spectrum
  s_filtered <- pwc$spec * transfer_func_sq

  annual_cost <- mean_demand*mean(hourly_detrend)
  savings <- mean_demand*sum(s_filtered) * dw/3
  relative_savings <- savings/annual_cost
  mean_price=mean(hourly_detrend)
  #
  list("annual_cost"= mean_demand*mean_price, "savings"=mean_demand/mean_price*sum(s_filtered) * dw/3, "relative savings"=savings/annual_cost) # ann

}

cost_savings(48)

savings_df <- tibble(tau=0:96) %>% rowwise() %>% mutate(savings=cost_savings(tau)[["savings"]])

savings_df %>% ggplot(aes(tau,savings))+geom_line()


#############################
# hourly model
###############################
library(tsibble)
#hourly price profile
hourly_2023_2025 <- readr::read_csv("~/Policy/CAMG/Dynamic Pricing/historical-irish-electricity-prices.csv")
names(hourly_2023_2025) <- c("datetime","price")

hourly_2023_2025 <- hourly_2023_2025 %>% dplyr::filter(lubridate::year(datetime) >= 2023) %>% as_tsibble(index = datetime) %>% tsibble::fill_gaps() %>% dplyr::mutate(Price = imputeTS::na_interpolation(price))

hourly_2023_2025 <- hourly_2023_2025 %>% dplyr::filter(lubridate::minute(datetime) == 0)
hourly_ts <- hourly_2023_2025 %>% model(STL(price ~ trend(window = 365) + season(period = 7) +season(period = 365))) %>% fabletools::components()

hourly_ts <- hourly_ts %>% mutate(detrended=price-trend)

hourly_ts %>% ggplot() + geom_line(aes(Timestamp_UTC, Price)) + geom_line(aes(Timestamp_UTC, detrended),colour="blue")

hourly_ts<- hourly_ts %>% mutate(hour = hour(datetime), wday=wday(datetime),quarter=quarter(datetime,fiscal_start = 3))
quarters <- tibble(quarter=1:4,season=c("MAM","JJA","SON","DJF"))
hourly_ts <- hourly_ts %>% inner_join(quarters)
hourly_ts <- hourly_ts %>% select(datetime,price,trend,detrended,hour,wday,season)
#write_rds(hourly_ts,"~/Policy/CAMG/Dynamic Pricing/hourly_prices_ts.RData")

#weekdays
g1 <- hourly_ts %>% dplyr::filter(wday %in% 2:6) %>% ggplot(aes(factor(hour),Price, fill=factor(quarter)))+geom_boxplot(colour="grey50") + facet_wrap(quarter~.)
g1 <- g1 + scale_y_continuous(limits=c(-10,500)) + theme_minimal() + scale_fill_canva(palette="Fun and cheerful")
#weekends
g2 <- hourly_ts %>% dplyr::filter(wday %in% c(1,7)) %>% ggplot(aes(factor(hour),Price, fill=factor(quarter)))+geom_boxplot(colour="grey50") + facet_wrap(quarter~.)
g2 <- g2 + scale_y_continuous(limits=c(-10,500)) + theme_minimal() + scale_fill_canva(palette="Fun and cheerful")
g <- g1+g2
export::graph2ppt(g,"~/Policy/CAMG/Dynamic Pricing/hourly_prices_seasonality.ppt")


# 1. Strip it to a base data frame to kill any 'rowwise' or 'grouped' metadata
hourly_2023_2025 <- as.data.frame(hourly_2023_2025)

###################
# standard load profiles
#####################

#standard load profiles
load_profile <-  readxl::read_xlsx("~/Policy/CAMG/Dynamic Pricing/Load_profiles/load-profile-indexes-2026.xlsx",sheet="LP3",range="A5:CT370")
start <- as.POSIXct("2024-01-01 00:15:00")
end   <- as.POSIXct("2024-01-02 00:00:00")

# Use "15 min" as the increment
q_hours <- seq(from = start, to = end, by = "15 min")
names(load_profile)[3:98] <- format(q_hours, "%H:%M")
load_profile <- load_profile %>% pivot_longer(cols=c(-`Day Notes`,-Date), names_to="time",values_to="kWh")
load_profile <- load_profile %>% mutate(datetime = ymd_hm(str_glue("{Date} {time}"))) %>% dplyr::select(datetime,kWh, `Day Notes`)
#
#
load_profile <- load_profile %>% group_by(datetime=floor_date(datetime, "1 hour"),`Day Notes`) %>% summarise(kWh=sum(kWh))
# extend to year 2023-2025
load_profile1 <- load_profile %>% mutate(datetime = datetime %m-% years(1))
load_profile2 <- load_profile %>% mutate(datetime = datetime %m-% years(2))
load_profile3 <- load_profile %>% mutate(datetime = datetime %m-% years(3))
load_profile <- bind_rows(load_profile1,load_profile2,load_profile3) %>% arrange(datetime)
#write_csv(load_profile, "~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP3.csv")


#################################################
##########################################
## hourly model flexibility optimisation quadratic model
##########################################
##########################################

hourly_ts <- read_rds("~/Policy/CAMG/Dynamic Pricing/hourly_prices_ts.RData")
#combine load profile and prices
lp1 <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1.csv") %>% rename("lp1"=kWh)
lp2 <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP2.csv")  %>% rename("lp2"=kWh)
lp3 <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP3.csv")  %>% rename("lp3"=kWh)
lp4 <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP4.csv")  %>% rename("lp4"=kWh)


lp <- inner_join(lp1,lp2) %>% inner_join(lp3) %>% inner_join(lp4)
hourly_ts <- hourly_ts %>% inner_join(lp1)

hourly_ts %>% ggplot(aes(lp1,price))+geom_point(alpha=0.01)

mean_daily_demand <- 20 #kWh per day
mean(hourly_ts$price)/1000
total_cost <- sum(hourly_ts$price*hourly_ts$kWh*365*mean_daily_demand/1000)


#assume 20kWh usage per day

D0 <- 20 #kWh per day
phi <- 0.5

demand <- hourly_ts %>% mutate(price=price/1000,load=lp1*D0*365,baseload=phi*load,year=year(datetime))

#ToU
tariffs <- tibble(hour=0:23,tariff=c(rep("N",8),rep("D",9),rep("P",2),rep("D",4),"N"))

demand <- demand %>% inner_join(tariffs)

g <- demand %>% as_tibble() %>% group_by(date=as.Date(datetime),tariff) %>% summarise(price=mean(price)) %>% ggplot(aes(date,price,colour=tariff)) + geom_line(alpha=0.9)+scale_color_canva(palette="Simple but bold")+theme_minimal()
#export::graph2ppt(g,"~/Policy/CAMG/Dynamic Pricing/daily_tariff_prices.ppt")
demand %>% as_tibble() %>% group_by(tariff) %>% summarise(price=mean(price))

tou <- demand %>% mutate(price=case_when(tariff=="D"~0.35,tariff=="N"~0.2,tariff=="P"~0.5))

##########################
# functional minimisation
###########################

#typical setup Parameters
phi <- 0.5           # 50% Inflexible
tau <- 48        # Time constant (hours)
alpha <- 0.01       # Weighting factor
P_max <- 10.0         # Household import limit (kW)

get_flex <- function(demand, phi = 0.5, tau = 24, gamma = 0.5, eta = 1.0, P_max = 10,precision=1e-6) {
  # T = Total horizon in hours
  T_total <- nrow(demand)
  load <- demand$load
  print(paste("mean load =", mean(load)))
  price <- demand$price
  #frobenius scaling


  # 1. Bandwidth for the Exponential Kernel
  W <- ceiling(5 * tau)
  lags <- 0:W
  kernel_values <- exp(-lags / tau)

  frob_sq <- sum(kernel_values^2) + sum(kernel_values[-1]^2)
  frob_norm <- sqrt(frob_sq)
  # Energy-consistent Gamma
  gamma_scaled <- gamma / frob_norm


  # 2. Build Quadratic Matrix P
  # Discomfort Kernel (Banded Sparse)
  P_kern <- bandSparse(T_total, k = lags,
                       diag = lapply(kernel_values, function(v) rep(v, T_total)),
                       symmetric = TRUE)

  # Kinetic/Ramping Penalty (Finite Difference Matrix)
  # Represents eta * sum((x_t+1 - x_t)^2)
  D <- sparseMatrix(i = rep(1:(T_total-1), each = 2),
                    j = as.vector(rbind(1:(T_total-1), 2:T_total)),
                    x = rep(c(-1, 1), T_total-1))
  P_kin <- t(D) %*% D

  # Combined P matrix
  P <- (gamma_scaled * P_kern) + (eta * P_kin)
  #symmetrise
  P <- 0.5 * (P + t(P))
  # 3. Linear Vector q (Prices)
  q <- price

  # 4. Constraints (A*x)
  # We stack: 1. Sum(x) = 0  (Equality)
  #           2. x_t         (Identity for box constraints)

  A_sum <- Matrix(1, nrow = 1, ncol = T_total, sparse = TRUE)
  A_box <- Diagonal(T_total)
  A <- rbind(A_sum, A_box)

  # Lower and Upper Bounds
  # Equality constraint: l=0, u=0
  # Capacity constraint: -L0 <= x <= P_max - L0
  l <- c(0, -load)
  u <- c(0, P_max - load)

  # 5. Solve using OSQP
  settings <- osqpSettings(eps_abs = precision, eps_rel = precision, verbose = TRUE)
  model <- osqp(P = P, q = q, A = A, l = l, u = u, pars = settings)
  res <- model@Solve()

  # 6. Post-processing
  x_opt <- res$x
  demand$x <- x_opt
  demand$load_opt <- load + x_opt

  # Split the resulting load into components
  demand$baseload <- phi * load
  demand$flex_load <- (1 - phi) * load
  demand$flex_opt <- (1 - phi) * load + x_opt

  return(demand)
}

test <- get_flex(demand,0.5,1e-1,24,10)



100*(sum(test$load - test$load_opt))



df <- tibble()
for(tau in 48)
#for(tau in c(1,3,12,24,36))
for(eta in c(0.01,0.05,0.1,0.25,0.5))
for(alpha in seq(0.1,2,by=0.1)){
 df1 <- get_flex(demand,0.5,tau,gamma=alpha,eta,P_max,precision = 1e-6) %>% select(datetime,year,hour,wday,price,load,baseload,load_opt,x)
 df1 <- df1 %>% as_tibble()
 df1$alpha <- alpha
 df1$eta <- eta
 df1$tau <- tau
 df <- df %>% bind_rows(df1)
 print(paste("tau=",tau,"alpha=",alpha,"eta=",eta))
 #write_csv(df1,"~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_SEM_frobenius.csv",append=T)
}

df <- df %>% arrange(alpha)
df <- df %>% mutate(yday=yday(datetime))
#
#add tau=0
#
get_load_tau0 <- function(demand, phi, alpha, P_max, precision = 1e-6) {

  cast_sparse <- function(m) { as(m, "dgCMatrix") }


  T_len <- length(demand$price)
  L_base <- (1 - phi) * demand$load
  total_energy_target <- sum(L_base)

  # --- 1. THE HESSIAN (P) ---
  # In the limit tau -> 0, K'K becomes the Identity matrix.
  # The penalty is simply sum( alpha * (L - L_base)^2 )
  # Diagonal of P is 2 * alpha
  P_osqp <- Diagonal(T_len, 2 * alpha)

  # --- 2. THE GRADIENT (q) ---
  # q = price - 2 * alpha * L_base
  q_osqp <- as.numeric(demand$price - (2 * alpha * L_base))

  # --- 3. CONSTRAINTS ---
  # Equality: sum(L) = Target
  # Inequality: 0 <= L <= P_max
  A_global <- matrix(1, nrow = 1, ncol = T_len)
  A_box    <- Diagonal(T_len)
  A_cons   <- rbind(cast_sparse(A_global), A_box)

  l_cons   <- c(total_energy_target, rep(0, T_len))
  u_cons   <- c(total_energy_target, rep(P_max, T_len))

  # --- 4. SOLVE ---
  settings <- osqpSettings(eps_abs = precision, eps_rel = precision, verbose = FALSE)
  model <- osqp(P = P_osqp, q = q_osqp, A = A_cons, l = l_cons, u = u_cons, pars = settings)
  res   <- model@Solve()

  # Return results
  demand$flex_load_opt <- res$x
  demand$load_opt <- (phi * demand$load) + res$x

  return(demand %>% as_tibble())
}

df0 <- tibble()
for(alpha in c( 1e-04,5e-04,1e-03,5e-03,1e-02,5e-02,1e-01,5e-01,1e+00,5e+00,1e+01,5e+01,1e+02,5e+02,1e+03)){
 df1 <- get_load_tau0(demand,0.5,alpha,10) %>% as_tibble()
 df1$alpha <- alpha
 df0 <- df0 %>% bind_rows(df1)
}
df0$tau <- 0
df0 <- df0 %>% arrange(alpha)
df0 <- df0 %>% mutate(yday=yday(datetime))

df <- df %>% bind_rows(df0)

#write_csv(df,"~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_SEM_frobenius_phi=0.5_tau=48.csv")
df0 <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_SEM_frobenius_phi=0.5.csv")
df <- df0 %>% bind_rows(df)
#write_csv(df,"~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_SEM_frobenius_phi=0.5.csv")

df <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_SEM_frobenius_phi=0.5.csv")

g1 <- df %>% dplyr::filter(year==2024,yday %in% 120:145,eta==0.01,alpha==1) %>% ggplot() + geom_line(aes(datetime,load_opt,colour=factor(alpha)),linewidth=1.2) #+ geom_point(aes(datetime,baseload+flex_opt),colour="red")
g1 <- g1 + theme_minimal() + scale_color_canva(palette ="Sunny and calm") +geom_line(aes(datetime,load), linewidth = 1.1,alpha=0.75,colour="red",linetype="dashed")
g2 <- df %>% dplyr::filter(year==2024,yday %in% 120:125,alpha==1) %>% ggplot() + geom_line(aes(datetime,price),colour="grey50") + theme_minimal() #+ ggtitle("euros/kWh")

g <- g1/g2 + plot_layout(heights = c(2, 0.6))
#export::graph2ppt(g,"~/Policy/CAMG/Dynamic Pricing/load_shifting_example_LP1.ppt")
#demand_response[2000:2050,] %>% ggplot()+geom_point(aes(price,flex_opt))

df %>% dplyr::filter(year==2025, yday %in% 321:321, hour==12) %>% ggplot(aes(alpha,load_opt,colour=factor(hour)))+geom_line() + geom_point()

df <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_flexible_load_phi=0.5.csv")

#check total consumption as a function of alpha

df %>% group_by(tau,alpha,eta) %>% summarise(total_load=sum(load), total_load_opt=sum(load_opt)) %>% mutate(cost_error=0.11*(total_load_opt-total_load)) %>% arrange(cost_error)
#cost by alpha
g1 <- df %>% group_by(tau,alpha,eta) %>% summarise(total_cost=sum(load*price), total_cost_opt=sum(price*load_opt)) %>% ggplot(aes(alpha,total_cost_opt,colour=factor(tau))) + geom_point() +geom_line()
g1 <- g1 + facet_wrap(.~eta)
g1 <- g1 +geom_hline(yintercept = sum(demand$price*demand$load),linetype="dotted",colour="grey50") + theme_minimal() + scale_x_continuous(trans="log10") + scale_y_continuous(limits=c(1000,2750))
#
g1

#
df <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP2_flexible_load_phi=0.5.csv")
#cost by alpha
g2 <- df %>% group_by(tau,alpha) %>% summarise(total_cost=sum(load*price), total_cost_opt=sum(price*load_opt)) %>% ggplot(aes(alpha/tau^2,total_cost_opt,colour=factor(tau))) + geom_point()  + geom_line()
g2 <- g2 +geom_hline(yintercept = sum(demand$price*demand$load),linetype="dotted",colour="grey50") + theme_minimal() + scale_x_continuous(trans="log10") + scale_y_continuous(limits=c(2000,2750))
#
g2
g1+g2
#export::graph2ppt(g1+g2,"~/Policy/CAMG/Dynamic Pricing/dp_costs_LP1_vs_LP2.ppt")

#inter-day load-shifting


daily_demand <- df %>% mutate(yday=yday(datetime)) %>% as_tibble()
daily_demand <- daily_demand %>% as_tibble() %>% select(alpha,tau,year,yday,price,load,load_opt) %>% group_by(tau,alpha,year, yday) %>% mutate(load=sum(load),load_opt=sum(load_opt),price=mean(price))
daily_demand <- distinct(daily_demand) %>% mutate(load_shift=load_opt-load)

interday <- daily_demand %>% group_by(tau,alpha) %>% summarise(mean_shift=mean(load_shift),sd_shift=sd(load_shift))


g1 <- daily_demand[200:214,] %>% ggplot() + geom_line(aes(yday,load)) + geom_line(aes(yday,load_opt),colour="red")
g2 <- daily_demand[200:214,] %>% ggplot() + geom_line(aes(yday,price)) + geom_point(aes(yday,price))
g1/g2

cor(diff(daily_demand_response$price),diff(daily_demand_response$load))
plot(diff(daily_demand_response$price),diff(daily_demand_response$load_opt))

#peak-shaving
# reduction in 17:00-19:00



cost0 <- sum(demand$price*demand$load)
#
cost <- sum(demand$price*(demand$baseload+demand$flex_opt))

#################################################
# ToU
# Day-night-peak flexibility analysis
####################################################

tou <- demand %>% mutate(price=case_when(tariff=="D"~0.35,tariff=="N"~0.184,tariff=="P"~0.373))
tou <- tou[1:8760,]


tf <- tibble()
for(tau in c(3,12,24,36,48,60))
  #for(tau in c(1))
  #for(alpha in c(1000)){
  for(alpha in c( 1e-04,5e-04,1e-03,5e-03,1e-02,5e-02,1e-01,5e-01,1e+00,5e+00,1e+01,5e+01,1e+02,5e+02,1e+03)){
    tf1 <- get_flex_constr(demand[1:8760,],0.5,alpha,tau,P_max,precision = 1e-6) %>% select(datetime,year,hour,wday,price,load,baseload,load_opt,flex_load_opt)
    tf1 <- tf1 %>% as_tibble()
    tf1$alpha <- alpha
    tf1$tau <- tau
    tf <- tf %>% bind_rows(tf1)
    print(paste("tau=",tau,"alpha=",alpha))
  }

tf <- tf %>% arrange(alpha)
tf <- tf %>% mutate(yday=yday(datetime))
#
#add tau=0
#
get_load_tau0 <- function(demand, phi, alpha, P_max, precision = 1e-6) {

  cast_sparse <- function(m) { as(m, "dgCMatrix") }


  T_len <- length(demand$price)
  L_base <- (1 - phi) * demand$load
  total_energy_target <- sum(L_base)

  # --- 1. THE HESSIAN (P) ---
  # In the limit tau -> 0, K'K becomes the Identity matrix.
  # The penalty is simply sum( alpha * (L - L_base)^2 )
  # Diagonal of P is 2 * alpha
  P_osqp <- Diagonal(T_len, 2 * alpha)

  # --- 2. THE GRADIENT (q) ---
  # q = price - 2 * alpha * L_base
  q_osqp <- as.numeric(demand$price - (2 * alpha * L_base))

  # --- 3. CONSTRAINTS ---
  # Equality: sum(L) = Target
  # Inequality: 0 <= L <= P_max
  A_global <- matrix(1, nrow = 1, ncol = T_len)
  A_box    <- Diagonal(T_len)
  A_cons   <- rbind(cast_sparse(A_global), A_box)

  l_cons   <- c(total_energy_target, rep(0, T_len))
  u_cons   <- c(total_energy_target, rep(P_max, T_len))

  # --- 4. SOLVE ---
  settings <- osqpSettings(eps_abs = precision, eps_rel = precision, verbose = FALSE)
  model <- osqp(P = P_osqp, q = q_osqp, A = A_cons, l = l_cons, u = u_cons, pars = settings)
  res   <- model@Solve()

  # Return results
  demand$flex_load_opt <- res$x
  demand$load_opt <- (phi * demand$load) + res$x

  return(demand %>% as_tibble())
}

tf0 <- tibble()
for(alpha in c( 1e-04,5e-04,1e-03,5e-03,1e-02,5e-02,1e-01,5e-01,1e+00,5e+00,1e+01,5e+01,1e+02,5e+02,1e+03)){
  tf1 <- get_load_tau0(demand[1:8760,],0.5,alpha,10) %>% as_tibble()
  tf1$alpha <- alpha
  tf0 <- tf0 %>% bind_rows(tf1)
}
tf0$tau <- 0
tf0 <- tf0 %>% arrange(alpha)
tf0 <- tf0 %>% mutate(yday=yday(datetime))

tf <- tf %>% bind_rows(tf0)
#write_csv(tf,"~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_day_night_peak_phi=0.5.csv")
#cost curves
tf %>% group_by(tau,alpha) %>% summarise(total_load=sum(load), total_load_opt=sum(load_opt)) %>% mutate(cost_error=0.11*(total_load_opt-total_load)) %>% arrange(total_load_opt)
#cost by alpha
g1 <- tf %>% group_by(tau,alpha) %>% summarise(total_cost=sum(load*price), total_cost_opt=sum(price*load_opt)) %>% ggplot(aes(alpha,total_cost_opt,colour=factor(tau))) + geom_point() +geom_line()
g1 <- g1 +geom_hline(yintercept = sum(tou$price*tou$load)/3,linetype="dotted",colour="grey50") + theme_minimal() + scale_x_continuous(trans="log10") #+ scale_y_continuous(limits=c(1000,2750))
#
g1




#mean daily profile
daily_profiles <- tou %>% as_tibble() %>% group_by(hour) %>% summarise(price = mean(price),load = mean(load),load_opt=mean(load_opt))
daily_profiles %>% ggplot(aes(hour,price))+geom_line()
daily_profiles %>% ggplot()+geom_line(aes(hour,load))+geom_line(aes(hour,load_opt),colour="red")



sum(tou$price*tou$load)
sum(tou$price*tou$load_opt)

df <- tibble()
#for(tau in c(12,24,36,48,60))
for(tau in c(96))
for(alpha in c(0.1,0.2, 0.5,1,2,3,4,5,10,20,50,75,100,200,1000)){
  df1 <- get_flex(tou[1:8760,],0.5,alpha,tau,P_max) %>% select(datetime,year,hour,wday,price,load,baseload,load_opt,flex_load_opt)
  df1 <- df1 %>% as_tibble()
  df1$alpha <- alpha
  df1$tau <- tau
  df <- df %>% bind_rows(df1)
}

df <- df %>% arrange(alpha)
df <- df %>% mutate(yday=yday(datetime))

#write_csv(df,"~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_day_night_peak_phi=0.5.csv")

#df <- read_csv("~/Policy/CAMG/Dynamic Pricing/Load_profiles/LP1_day_night_peak_phi=0.5.csv")
#alpha dependence
g1 <- df %>% group_by(tau,alpha) %>% summarise(total_cost=sum(load*price), total_cost_opt=sum(price*load_opt)) %>% ggplot(aes(alpha,total_cost_opt,colour=factor(tau))) + geom_point() +geom_line()
g1 <- g1 +geom_hline(yintercept = sum(tou$price*tou$load)/3,linetype="dotted",colour="grey50") + theme_minimal() + scale_x_continuous(trans="log10") #+ scale_y_continuous(limits=c(2000,2750))
#
g1

#tau dependence
g1 <- df %>% group_by(tau,alpha) %>% summarise(total_cost=sum(load*price), total_cost_opt=sum(price*load_opt)) %>% ggplot(aes(tau,total_cost_opt,colour=factor(alpha))) + geom_point() +geom_line()
g1 <- g1 +geom_hline(yintercept = sum(tou$price*tou$load)/3,linetype="dotted",colour="grey50") + theme_minimal() + scale_x_continuous(trans="log10") #+ scale_y_continuous(limits=c(2000,2750))
#
g1

g1 <- df %>% dplyr::filter(year==2023, yday %in% 128:128, alpha %in% c(5,20,50,100),tau==24) %>% ggplot() + geom_line(aes(datetime,load_opt,colour=factor(alpha)),linewidth=1.2) #+ geom_point(aes(datetime,baseload+flex_opt),colour="red")
g1 <- g1 + theme_minimal() + scale_color_canva(palette ="Sunny and calm") +geom_line(aes(datetime,load), linewidth = 1.1,alpha=0.75,colour="red",linetype="dashed")
g2 <- df %>% dplyr::filter(year==2023,yday %in% 128:128,alpha==alpha) %>% ggplot() + geom_line(aes(datetime,price),colour="grey50") + theme_minimal() #+ ggtitle("euros/kWh")

g <- g1/g2 + plot_layout(heights = c(2, 0.6))
g

test <- df %>% group_by(yday, alpha, tau) %>% summarise(load=sum(load),load_opt =sum(load_opt)) %>% mutate(delta_load = load_opt-load)

test %>% ggplot(aes(yday,delta_load,colour=factor(alpha)))+geom_line() + facet_wrap(.~tau)


#
scale_factor <- function(tau) tanh(1/tau)^2


##############################
# Helmholtz
##############################

helm <- demand %>% mutate(price=case_when(tariff=="D"~0.35,tariff=="N"~0.184,tariff=="P"~0.373))
helm$price <- 1
helm$load <- 0
helm <- helm[1:1000,]
#point sources
helm[400,"price"] <- 1.1
helm[450,"price"] <- 0.9

test <- get_flex_constr(helm,0,10000,6,P_max=10,precision = 1e-6,drop_load_constraints = TRUE) %>% select(datetime,year,hour,wday,price,load,baseload,load_opt,flex_load_opt)

g1 <- test[380:480,] %>% ggplot(aes(datetime,price)) + geom_line()+geom_point()
g2 <- test[380:480,] %>% ggplot(aes(datetime,load_opt)) + geom_line()+geom_point()
g2/g1

######################
# continuous time
#####################


###################
# kinetic terms
#####################


# 1. Setup Parameters
T <- 26280           # T in hours
tau <- 60            # Timescale
W <- 5 * tau         # Bandwidth (300)
gamma <- 0.5         # Discomfort scaling
eta <- 1.0           # Kinetic scaling

library(Matrix)
library(osqp)

get_flex <- function(demand, phi = 0.2, tau = 24, gamma = 0.5, eta = 1.0, P_max = 10) {
  # T = Total horizon in hours
  T_total <- nrow(demand)
  load <- demand$load
  price <- demand$price

  # 1. Bandwidth for the Exponential Kernel
  W <- ceiling(5 * tau)
  lags <- 0:W
  kernel_values <- exp(-lags / tau)

  # 2. Build Quadratic Matrix P
  # Discomfort Kernel (Banded Sparse)
  P_kern <- bandSparse(T_total, k = lags,
                       diag = lapply(kernel_values, function(v) rep(v, T_total)),
                       symmetric = TRUE)

  # Kinetic/Ramping Penalty (Finite Difference Matrix)
  # Represents eta * sum((x_t+1 - x_t)^2)
  D <- sparseMatrix(i = rep(1:(T_total-1), each = 2),
                    j = as.vector(rbind(1:(T_total-1), 2:T_total)),
                    x = rep(c(-1, 1), T_total-1))
  P_kin <- t(D) %*% D

  # Combined P matrix
  P <- (gamma * P_kern) + (eta * P_kin)

  # 3. Linear Vector q (Prices)
  q <- price

  # 4. Constraints (A*x)
  # We stack: 1. Sum(x) = 0  (Equality)
  #           2. x_t         (Identity for box constraints)

  A_sum <- Matrix(1, nrow = 1, ncol = T_total, sparse = TRUE)
  A_box <- Diagonal(T_total)
  A <- rbind(A_sum, A_box)

  # Lower and Upper Bounds
  # Equality constraint: l=0, u=0
  # Capacity constraint: -L0 <= x <= P_max - L0
  l <- c(0, -load)
  u <- c(0, P_max - load)

  # 5. Solve using OSQP
  settings <- osqpSettings(eps_abs = 1e-5, eps_rel = 1e-5, verbose = TRUE)
  model <- osqp(P = P, q = q, A = A, l = l, u = u, pars = settings)
  res <- model@Solve()

  # 6. Post-processing
  x_opt <- res$x
  demand$x <- x_opt
  demand$load_opt <- demand$load + x_opt

  # Split the resulting load into components
  demand$baseload <- phi * load
  demand$flex_load <- (1 - phi) * load
  demand$flex_opt <- (1 - phi) * load + x_opt

  return(demand)
}


test <- get_flex(demand)


#############################
# Electric Ireland price plans
#############################



########################
#
#########################
library(depmicrosimr)
library(tidyverse)
library(ggthemes)
library(patchwork)
df <- tibble()
for(gamma %in% c(0.1,0.5,1,2,5)){
 df <- get_flex(make_demand_response_data(),tau=48,gamma=1,eta=0.1)
}
df <- df %>% dplyr::mutate(year=year(datetime),yday=yday(datetime))

g1 <- df %>% dplyr::filter(year==2024,yday %in% 220:225) %>% ggplot() + geom_line(aes(datetime,load_opt),linewidth=1.2) #+ geom_point(aes(datetime,baseload+flex_opt),colour="red")
g1 <- g1 + theme_minimal() + scale_color_canva(palette ="Sunny and calm") +geom_line(aes(datetime,load), linewidth = 1.1,alpha=0.75,colour="red",linetype="dashed")
g2 <- df %>% dplyr::filter(year==2024,yday %in% 220:225) %>% ggplot() + geom_line(aes(datetime,price),colour="grey50") + theme_minimal() #+ ggtitle("euros/kWh")

g <- g1/g2 + plot_layout(heights = c(2, 0.6))
g


#######################
# supplier perspective
#######################

tou_tariffs <- function(flat_rate) {
  # Absolute empirical coefficients observed in the Irish consumer market


  r_day   <- day_rate_anchor
  r_night <- day_rate_anchor * ALPHA_NIGHT
  r_peak  <- day_rate_anchor * ALPHA_PEAK
  r_flat  <- day_rate_anchor * BETA_FLAT

  return(tibble::tibble(
    tariff_type = c("Flat_24hr", "ToU_Day", "ToU_Night", "ToU_Peak"),
    rate_cents  = round(c(r_flat, r_day, r_night, r_peak), 4)
  ))
}

# Example: Feed it an arbitrary base day rate of 40c to watch the matrix scale
generate_empirical_tariffs(day_rate_anchor = 40.00)


tariffs <- read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/tariffs.csv")

#
