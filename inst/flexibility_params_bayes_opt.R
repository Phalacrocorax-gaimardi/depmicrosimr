#####################################################
# find reasonable flexibility parameter estimates
####################################################

library(depmicrosimr)
library(tidyverse)

get_lp2_profile <- function(kWh,phi=0.5,gamma=0.25,eta=0.25,tau=60,prices_scen){
  
  yeartime <- 2026
  profile <- "LP1"
  profile <- tolower(profile)
  load <- load_profiles %>% dplyr::select(datetime,dplyr::any_of(profile)) %>% dplyr::rename("load":=all_of(profile))
  #normalise to kWh annual
  load <- load %>% dplyr::mutate(load = load*kWh)
  mean_load <- mean(load$load)
  gamma <-gamma/mean_load
  eta <- eta/mean_load
  #mean load is kWh/8760
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
  df %>% inner_join(load_profiles %>% select(datetime,"lp2") %>% mutate(lp2=kWh*lp2)) %>% return()
  
}
# visual
# make a table of flexibility score as measured by the % of load that is shifted
#
test <- tibble()
for(phi in seq(0,1,by=0.1))
 for(gamma in c(0.1,0.25,0.5,1,2,5,25,50)){
 #
 test <- test %>%  bind_rows(get_lp2_profile(8760,phi,gamma,1,96,prices_scen) %>% mutate(phi=phi,gamma=gamma))
}

dyn <- test %>% filter(tariff_plan=="dynamic")
g1 <- dyn %>% filter(week(datetime)==30,wday(datetime)==2,tariff_plan=="dynamic") %>% ggplot() + geom_line(aes(datetime,load_opt,colour=factor(gamma))) +geom_line(aes(datetime,load),linetype="dotted",linewidth=1.2)
g2 <- dyn  %>% filter(week(datetime)==30,wday(datetime)==2,tariff_plan=="dynamic") %>%  ggplot(aes(datetime,price))+geom_line()
#
g1/g2 + plot_layout(heights = c(3, 1))

###################
# flexibility scores (MAD)
###################

flex_scores_7 <- dyn %>% group_by(phi,gamma) %>% summarise(flex_score = 100*mean(abs(load-load_opt))/mean(load))
#
flex_scores_7$eta <- 1
flex_scores_7$tau <- 96
#
flex_scores <- flex_scores %>% bind_rows(flex_scores_7)
#
#interday load shifting
flex_day <- dyn %>% group_by(phi,gamma, yday=yday(datetime)) %>% summarise(load=sum(load),load_opt=sum(load_opt))
flex_day <- flex_day %>% group_by(phi,gamma) %>% summarise(flex_score = 100*mean(abs(load-load_opt))/mean(load))
#interweek load shifting
flex_week <- dyn %>% group_by(phi,gamma, week=week(datetime)) %>% summarise(load=sum(load),load_opt=sum(load_opt))
flex_week <- flex_week %>% group_by(phi,gamma) %>% summarise(flex_score_week = 100*mean(abs(load-load_opt))/mean(load))





100*mean(abs(dyn$load-dyn$load_opt))/mean(dyn$load) #15%

library(ParBayesianOptimization)

# 1. Dummy definition of your slow function (Replace this with your real one)
min_fun <- function(phi,gamma,eta){
  tau <- 48
  kWh <- 8760
  df <- get_lp2_profile(kWh,phi,gamma,eta,tau,prices_scen)
  df <- df %>% filter(tariff_plan=="tou")
  sum((df$lp2 - df$load_opt)^2)
  
}
# 2. Wrap your function so the package can read it
# CRITICAL: It must return a list with Score = Value (Maximization)
scoring_function <- function(phi,gamma, eta) {
  actual_loss <- min_fun(phi, gamma, eta)
  
  return(list(
    Score = -actual_loss, # Negating because the package maximizes Score
    Value = actual_loss   # Optional: keeps track of the real min value
  ))
}
bounds <- list(
  phi = c(0, 1),
  gamma = c(0.01, 50),
  eta = c(0.01, 50)
  #tau = c(3, 120)
)

# 4. Run the Bayesian Optimization
opt_results <- bayesOpt(
  FUN = scoring_function,
  bounds = bounds,
  initPoints = 20,  # Number of random initial points to build the starting GP model
  iters.n = 50,     # Number of smart Bayesian steps to take after initialization
  acq = "ei",       # Acquisition function: Expected Improvement
  verbose = 1
)

# 5. Extract the best parameters found
best_pars <- getBestPars(opt_results)
print("Best parameters found:")
print(best_pars)
min_fun(best_pars$phi,best_pars$gamma,best_pars$eta)
## Hessian Check
# install.packages("numDeriv")
library(numDeriv)
best <- getBestPars(opt_results)

# Define a quick wrapper for numDeriv
wrapper_fun <- function(x) {
  min_fun(x[1], x[2], x[3])
}

# Calculate the Hessian matrix at your best parameter point
hessian_matrix <- numDeriv::hessian(wrapper_fun, c(best$phi, best$gamma, best$eta))

# Check the eigenvalues
eigen_values <- eigen(hessian_matrix)$values
print(eigen_values)
#try to imrpove
# Use base R's local optimizer to escape the saddle point
best_pars <- getBestPars(opt_results)
best_vector <- c(best_pars$phi, best_pars$gamma, best_pars$eta, best_pars$tau)

escape_run <- optim(
  par = best_vector, 
  fn = function(x) min_fun(x[1], x[2], x[3], x[4]), 
  method = "BFGS",
  control = list(trace = 1) # This lets you watch the score drop in real-time
)

print("New, much better parameters:")
print(escape_run$par)
print(paste("Previous Best Score:", min(opt_results$scoreSummary$Value)))
print(paste("New Lower Minimum:", escape_run$value))



#visual check



#
test <- get_lp2_profile(3000,best_pars$phi,best_pars$gamma,best_pars$eta,best_pars$tau,prices_scen) %>% filter(tariff_plan=="tou")
#
#test <- get_lp2_profile(3000,0.5,2,1,24,prices_scen) %>% filter(tariff_plan=="tou")

test0 <- test %>% group_by(hour=hour(datetime)) %>% summarise(load=mean(load),load_opt=mean(load_opt),lp2=mean(lp2))
#
test0 %>% ggplot()+geom_line(aes(hour,load)) + geom_line(aes(hour,load_opt),colour="red") + geom_line(aes(hour,lp2),colour="blue")


test0 <- test %>% group_by(hour=hour(datetime)) %>% summarise(load=mean(load),load_opt=mean(load_opt),baseload=mean(baseload), flex_opt=mean(flex_opt))
test0 %>% ggplot()+geom_line(aes(hour,load)) + geom_line(aes(hour,load_opt),colour="red") + geom_line(aes(hour,baseload),colour="blue")+geom_line(aes(hour,flex_opt),colour="green")





#add LP2
test <- test %>% inner_join(load_profiles %>% select(datetime,"lp2") %>% mutate(lp2=kWh*lp2))
#
test0 <- test %>% group_by(tariff_plan, hour=hour(datetime)) %>% summarise(load=mean(load), load_opt=mean(load_opt))

g1 <- test0 %>% filter(tariff_plan!= "dynamic") %>% ggplot(aes(hour,load_opt,colour=tariff_plan))+geom_line()

load0 <- load_profiles %>% group_by(hour=hour(datetime)) %>% summarise(lp1=mean(lp1),lp2=mean(lp2))
g2 <- load0 %>% pivot_longer(-hour,names_to="profile",values_to="load") %>% ggplot(aes(hour,load,colour=profile)) + geom_line()
library(patchwork)
g1+g2
