library(depmicrosimr)
library(tidyverse)
library(patchwork)
library(ggthemes)

################
# prices
#################

sem <- dynamic_prices(sD,2040)
sem %>% ggplot(aes(datetime,price,colour=regime))+geom_line()+theme_minimal()

prices_scen <- set_prices(sD)

prices_scen %>% filter(year(datetime)==2030,yday(datetime)==150) %>% ggplot(aes(datetime,price,colour=tariff_plan))+geom_line() + geom_point()
#daily dynmaic price variations
pp <- prices_scen %>% filter(tariff_plan != "flat") %>% pivot_wider(names_from=tariff_plan,values_from=price)
pp <- pp %>% group_by(date=as.Date(datetime)) %>% summarise(tou=mean(tou),dynamic=mean(dynamic),mae=sqrt(sum((dynamic-tou)^2)))
#weekly dynamic price variations
pp <- prices_scen %>% filter(tariff_plan != "flat") %>% pivot_wider(names_from=tariff_plan,values_from=price)
pp <- pp %>% group_by(year=year(datetime),week=week(datetime)) %>% summarise(tou_mean=mean(tou),dynamic_mean=mean(dynamic),mae=sqrt(sum((dynamic-tou)^2)))


pp %>% ggplot()+geom_point(aes(year+week/52,dynamic_mean),colour="red")+geom_point(aes(year+week/52,tou_mean))

pp <- prices_scen %>% filter(tariff_plan != "flat") %>% pivot_wider(names_from=tariff_plan,values_from=price)
pp %>% filter(year(datetime)==2030) %>% group_by(as.Date(datetime)) %>% summarise(mae=sqrt(sum((dynamic-tou)^2))) %>% slice_max(mae)
week("2030-12-06")

####################
# initial conditions
####################

social <- make_artificial_society(dep_society_1,homophily,nu=4.5)
agents_in <- initialise_agents(sD,2019,prices_scen,social)

scen <- sD
scen <- scen %>% mutate(value=replace(value,parameter=="theta.",0.))
abm <- runABM(scen,1,2029,use_parallel = T)

abm_neutral <- read_rds("~/Policy/CAMG/Dynamic Pricing/ABM_outputs/neutral.RData")
#write_rds(abm_theta_0.2,"~/Policy/CAMG/Dynamic Pricing/ABM_outputs/abm_theta_0.0.RData")

uptake2 <- abm_neutral[[1]] %>% group_by(date) %>% count(tariff_plan)
uptake2$theta_max <- 0.

uptake <- uptake2 %>% bind_rows(uptake1)

g <- uptake2 %>% ggplot(aes(date,n/1217,fill=tariff_plan))+geom_area() +theme_minimal() + facet_wrap(.~theta_max)
g <- g + scale_fill_canva(palette = "Fun and cheerful") + geom_vline(xintercept = ymd("2026-01-01"),linetype="dotted")
export::graph2ppt(g,"~/Policy/CAMG/Dynamic Pricing/ABM_outputs/uptake.ppt")

prices_scen <- set_prices(sD)

g1 <- prices %>% filter(tariff_plan=="dynamic") %>% ggplot(aes(datetime,price, colour=(year(datetime)>2025)))+geom_line()
g1 <- g1 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,1))
g1
g3 <- prices %>% filter(tariff_plan=="dynamic", year(datetime)==2035,week(datetime)==48) %>% ggplot(aes(datetime,price)) +geom_line(colour="grey60",linewidth=1.2)
g3 <- g3 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,1))
#export::graph2ppt(g1,"~/Policy/CAMG/reports/Empower2/report_AMJ_2026/heteroskedastic_prices.png")
#


###############################
# flexible profiles
############################

tariff_plan0 <- "tou"
eta <- 0.25

pp <- prices_scen %>% filter(tariff_plan != "flat") %>% pivot_wider(names_from=tariff_plan,values_from=price)
week_no <- pp %>% filter(year(datetime)==2030) %>% group_by(date=as.Date(datetime)) %>% summarise(mae=sqrt(sum((dynamic-tou)^2))) %>% slice_max(mae) %>% pull(date) %>% week()



demand <- prices_scen
demand <- demand %>% dplyr::filter(lubridate::year(datetime)==2030)
demand <- demand %>% dplyr::inner_join(load_profiles_generalised %>% dplyr::select(datetime,lp1))
demand <- demand %>% dplyr::mutate(load=8760*lp1) %>% dplyr::select(-lp1)
demand <- demand %>% dplyr::filter(tariff_plan==tariff_plan0) %>% dplyr::select(datetime,price,load)



g3 <- prices_scen %>% filter(tariff_plan==tariff_plan0, year(datetime)==2020,week(datetime)==week_no) %>% ggplot(aes(datetime,price)) +geom_line(colour="grey60",linewidth=1.2)
g3 <- g3 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,0.75))

test <- get_flex(demand,phi=0.25,gamma=10,eta=eta,tau=12,kernel="exp")
g0 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g0 <- g0 + geom_line(aes(datetime,load_opt),colour="#a7d2cb",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g0 <- g0 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,3.5))

test <- get_flex(demand,phi=0.25,gamma=10,eta=eta,tau=24,kernel="exp")
g1 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g1 <- g1 + geom_line(aes(datetime,load_opt),colour="#f2d388",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g1 <- g1 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,3.5))

test <- get_flex(demand,phi=0.25,gamma=10,eta=eta,tau=36,kernel="exp")
g2 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g2 <- g2 + geom_line(aes(datetime,load_opt),colour="#c98474",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g2 <- g2 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,2.5))

test <- get_flex(demand,phi=0.25,gamma=10,eta=eta,tau=48,kernel="exp")
g4 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g4 <- g4 + geom_line(aes(datetime,load_opt),colour= "#874c62",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g4 <- g4 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(-0.1,3.5))

test <- get_flex(demand,phi=0.25,gamma=10,eta=eta,tau=96,kernel="exp")
g5 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g5 <- g5 + geom_line(aes(datetime,load_opt),colour= "grey80",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g5 <- g5 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(-0.1,3.5))


g3/g0/g1/g2/g4/g5
#export::graph2ppt(g3/g1/g2,"~/Policy/CAMG/reports/Empower2/report_AMJ_2026/load_shifting.png")


export::graph2ppt(g1 + g2,"~/Policy/CAMG/reports/Empower2/report_AMJ_2026/heteroskedastic_prices.png")


sem <- get_sem_prices(sD)

g <- sem %>% filter(year(datetime) %in% 2022:2030) %>% ggplot(aes(datetime,price,colour=(year(datetime)<2026)))+geom_line()
g <- g + theme_minimal() + scale_color_canva(palette = "Fun and cheerful") + scale_y_continuous(limits=c(-0.1,0.75)) + theme(legend.position = "none")
g1 <- sem %>% filter(year(datetime) %in% 2026, month(datetime)==1) %>% ggplot(aes(datetime,price,colour=(year(datetime)<2026)))+geom_line()
g1 <- g1 + theme_minimal() + scale_color_canva(palette = "Fun and cheerful") + scale_y_continuous(limits=c(-0.1,0.75)) + theme(legend.position = "none")
g + g1 + plot_layout(widths = c(4, 1))

#export::graph2ppt(g1 + g2,"~/Policy/CAMG/Dynamic Pricing/sem_simulation.png")


prices %>% group_by(year = year(datetime),tariff) %>% summarise(price=mean(dynamic_price)) %>% ggplot(aes(year,price,colour=tariff))+geom_line()


prices <- sem_prices_2019_2025 %>% mutate(hour=hour(datetime)) %>% inner_join(tou_tariffs %>% rename("hour"=start)) %>% rename("sem"=price)
prices %>% group_by(yday=date(datetime),tariff) %>% summarise(sem=mean(sem)/1000) %>% ggplot(aes(yday,sem,colour=tariff))+geom_line(alpha=0.3)
prices <- prices %>% select(-end)

#annual averages
sem_prices <- prices %>% group_by(year=year(datetime),tariff) %>% summarise(sem=mean(sem))
tou_prices <- prices %>% group_by(year=year(datetime),tariff) %>% summarise(tou=mean(dynamic_price))
flat_prices <- prices %>% group_by(year=year(datetime)) %>% summarise(flat=mean(dynamic_price))


tou_prices %>% ggplot(aes(year,tou,colour=tariff))+geom_line()


prices_scen %>% filter(tariff_plan=="dynamic") %>% ggplot(aes(datetime,price,colour=tariff_plan))+geom_line()

##########################
# costs
##############################


#load dependence
df <- tibble()
for(kWh in seq(1000,20000,by=1000)){
  df0 <- get_annual_cost(2030,kWh,"tou",0.5,0.25,1,48,"LP1",prices_scen)
  df0$kWh <- kWh
  df <- df %>% bind_rows(df0)
}

df %>% ggplot()+geom_line(aes(kWh,annual_bill_inflexible)) + geom_line(aes(kWh,annual_bill_flexible),colour="red")

#phi-tau dependence
#recall:
prices_scen <- set_prices(sD)
df <- tibble()
for(phi in seq(0,1,by=0.1))
for(tau in c(6,12,24,36,48,60,72)){
  df0 <- tariff_plan_bills(8760,phi,2,0.4,tau,"LP1",2030,2020,prices_scen)
  df0$phi <- phi
  df0$gamma <- 2
  df0$eta <- 0.4
  df0$tau <- tau
  df$kWh <- 8760
  df <- df %>% bind_rows(df0)
}


g <- df %>% ggplot(aes(gamma,annual_bill,colour=tariff_plan))+geom_line() + facet_wrap(.~tau)# + scale_x_continuous(trans="sqrt")
g + theme_minimal() + scale_colour_canva() + geom_point()

df1 <- df %>%  filter(tariff_plan != "flat") %>% pivot_wider(names_from="tariff_plan",values_from="annual_bill") %>% mutate(dyn_savings=-(dynamic-tou))

g <- df1 %>% filter(gamma < 10) %>% ggplot(aes(gamma,dyn_savings,colour=factor(tau)))+geom_line() #+ facet_wrap(.~tau) + scale_x_continuous(trans="sqrt")
g + theme_minimal() + scale_colour_tableau() + geom_point()

#phi-tau dependence

phi <- 0.1
df <- tibble()
for(tariff_plan in c("flat","tou","dynamic"))
  for(eta in c(0.1,0.5,1,2))
 for(phi in seq(0,0.8,by=2))
  for(tau in seq(3,72,by=3)){
    df0 <- get_full_annual_cost(2030,8760,tariff_plan,0.2,10,eta,tau,"exp","LP1",prices_scen)
    #df0$tau <- tau
    #df0$tariff <- tariff
    df <- df %>% bind_rows(df0)
  }



g <- df  %>% filter(tariff_plan %in% c("dynamic","tou")) %>% ggplot(aes(tau,annual_bill_flexible,colour=factor(eta)))+geom_line() #+ facet_wrap(.~tau) + scale_x_continuous(trans="sqrt")
g <- g + theme_minimal() + scale_colour_tableau() + geom_point() + facet_wrap(.~tariff_plan)
g +geom_hline(yintercept = filter(df,tariff_plan=="flat")$annual_bill_inflexible %>% mean(),linetype="dotted")

df <- df %>% mutate(full_cost=annual_bill_flexible+penalty+kinetic)
df1 <- df %>% pivot_longer(c(annual_bill_inflexible,annual_bill_flexible,fin_gain,penalty,kinetic,real_gain,full_cost))


df1 <- df1 %>% filter(!str_detect(name,"gain"),tariff_plan != "flat",name != "annual_bill_inflexible")
g <- df1  %>% ggplot(aes(tau,value,colour=name,linetype=factor(eta)))+geom_line() #+ facet_wrap(.~tau) + scale_x_continuous(trans="sqrt")
g <- g + theme_minimal() + scale_colour_tableau()  + facet_wrap(.~tariff_plan)
g +geom_hline(yintercept = filter(df,tariff_plan=="flat")$annual_bill_inflexible %>% mean(),linetype="dotted")


df %>% ggplot()+geom_line(aes(tau,annual_bill_flexible,colour=tariff)) #+ geom_line(aes(tau,annual_bill_flexible),colour="red") + scale_y_continuous(limits=c(2600,3600))


#phi dependence

df <- tibble()
for(tariff in c("flat","tou","dynamic"))
  for(phi in seq(0,0.9,by=0.1)){
    df0 <- get_annual_cost(2030,8760,tariff,phi,10,1,24,"LP1",prices_scen)
    df0$phi <- phi
    df0$tariff <- tariff
    df <- df %>% bind_rows(df0)
  }


df %>% ggplot()+geom_line(aes(phi,gain,colour=tariff)) #+ geom_line(aes(tau,annual_bill_flexible),colour="red") + scale_y_continuous(limits=c(2600,3600))

prices_scen <- set_prices(sD)
tariff_plan_bills(2025,8760,0.25,2,0.2,48,natural_profile="LP1",prices_scen)
#


retail_tou_model <- tibble(tariff=c("night","day","peak"),A= c(0.1981,0.0852,0.2255),B=c(1.09,1.09,1.09))
prices_tou <- prices_tou %>% mutate(tou_implied=case_when(tariff=="night"~0.0852+1.09*price,
                                                          tariff=="day"~0.1981+1.09*price,
                                                          tariff=="peak"~0.2255+1.09*price))
prices_tou %>% ggplot()+geom_line(aes(year,tou_implied,colour=tariff)) + geom_line(aes(year,price,colour=tariff),linetype="dotted")


kWh <- 8760/10
load <- load_profiles%>% dplyr::select(datetime,"lp1")
load <- load %>% dplyr::mutate(datetime = update(datetime, year=2030))
load <- load %>% dplyr::mutate(load=kWh*lp1) %>% dplyr::select(-lp1)
prices <- get_tariff_prices(sD) %>% dplyr::filter(tariff_plan=="tou")
prices <- prices  %>% dplyr::select(datetime,price)
demand <- load %>% dplyr::inner_join(prices)
test <- get_flex(demand,phi=0.5,gamma=0.5*8760/kWh,eta=8760/kWh,tau=48)

test %>% filter(week(datetime)==20) %>% ggplot()+geom_line(aes(datetime,load))+geom_line(aes(datetime,load_opt),colour="red")

prices_scen <- set_prices(sD)
get_annual_cost(2030,8760,tariff="tou",prices_scen=prices_scen)
#

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

get_flex_scores(sD,2026,8760,"tou",0,1,1,24,"exp",profile="LP1",prices_scen)
get_flex_scores(sD,2026,8760,"tou",0.5,1,0.1,24,"exp",profile="LP1",prices_scen)


get_flex_scores(sD,2026,8760,"tou",0,1,1,48,"cauchy",profile="LP1",prices_scen)

get_flex_scores(sD,2026,8760,"dynamic",0,1,1,96,"matern",profile="LP1",prices_scen)

#test gamma-tau scaling


flex_scores_new <- tibble()
#flex_scores_new <-read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/flex_scores.csv")
for(eta in 0.6)
 for(tariff_plan in c("tou","dynamic"))
  for(phi in seq(0,0.8,by=0.2))
   for(gamma in c(1,10,20,50,100))
      for(tau in c(6,12,18,24,30,36,48,60,72))
      {
          print(paste("phi=",phi,"tau=",tau))
          flex_scores_new <- flex_scores_new %>% bind_rows(get_flex_scores(sD,2026,8760,tariff_plan,phi,gamma,eta,tau,"exp","LP1",prices_scen))
        }

#flex_scores <-read_csv("C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/flex_scores.csv")
flex_scores <- flex_scores %>% filter(eta != 0.6)
flex_scores <- flex_scores %>% bind_rows(flex_scores_new)
#
#write_csv(flex_scores,"C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/flex_scores.csv")

#make flex scores table

flex_scores_new <- tibble()
for(tariff_plan in c("tou","dynamic"))
 for(phi in seq(0,0.8,by=0.2))
  for(gamma in c(0,0.1,0.25,0.5,1,2,5,10,20))
    for(tau in c(24,48,72,96))
      #for(eta in c(0.1,0.2,0.5,1,2)){
      for(eta in 0.2){
      print(paste("gamma=",gamma))
      flex_scores_new <- flex_scores_new %>% bind_rows(get_flex_scores(sD,2026,8760,tariff_plan,phi,gamma,eta,tau,"matern","LP1",prices_scen))
      }

#write_csv(flex_scores_new,"C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/flex_scores.csv")

flex_scores_new %>% filter(phi==0.4,tariff_plan=="tou") %>% ggplot(aes(eta,flex_3hr,colour=factor(tau)))+geom_line()# + facet_wrap(.~tau)
flex_scores_new %>% filter(phi==0,tariff_plan=="tou") %>% ggplot(aes(eta,flex_1hr,colour=factor(tau)))+geom_line()# + facet_wrap(.~tau)

#tau dependence

df <- tibble()
for(tariff in c("flat","tou","dynamic"))
  for(tau in seq(12,2*96,by=12)){
    df0 <- get_flex_score(sD,2030,8760,tariff,0.25,1,0.1,tau,"LP1",prices_scen)
    df <- df %>% bind_rows(df0)
  }

df <- df %>% pivot_longer(c(flex_hour,flex_day,flex_week),names_to="timescale",values_to="flexibility")
df %>% ggplot()+geom_line(aes(tau,flexibility,colour=tariff_plan)) + geom_point(aes(tau,flexibility,colour=tariff_plan)) + facet_wrap(.~timescale)


##############################
# typical household flexibility
##############################

score_cube <- flex_score_cube()

result <- purrr::map(1:n, ~match_flex_params(25.4,score_cube)) |> dplyr::list_rbind()




##############################
# impact of cap
##############################

prices_scen <- set_prices(sD)

prices_scen_no_cap <- set_prices(sD,cru_cap = FALSE)
library(patchwork)
g1 <- prices_scen %>% filter(tariff_plan=="dynamic") %>% ggplot(aes(datetime,price))+geom_line() + scale_y_continuous(limits=c(0,2))
g2 <- prices_scen_no_cap %>% filter(tariff_plan=="dynamic") %>% ggplot(aes(datetime,price))+geom_line() + scale_y_continuous(limits=c(0,2))
g1+g2


###################################
# aggregate load profiles
####################################

abm <- read_rds("~/Policy/CAMG/Dynamic Pricing/ABM_outputs/abm_theta_0.0.RData")[[1]]

get_profile(2030,3000,"dynamic",0.25,1,0.2,48,"LP1")

get_aggregate_profile <- function(year){

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

get_aggregate_profile(2040)

###########
# kernels
#############
tau <- 96
shape <- 2.5
scale <- tau/gamma(1+1/shape)
matern <- function(hour,tau){
  #Matern 5/2 Kernel
  #just one tuning parameter
  p <- sqrt(5) * abs(hour) / tau
  # Smooth flat top, exponential tail, 100% PSD
  (1 + p + (p^2) / 3) * exp(-p)
}
mixture <- function(hour,tau_short,tau_long,a_s){

  a_s*exp(-(hour/tau_short)^2) + (1-a_s)*exp(-(hour/tau_long)^2)

}

tau <- 48
kern <- tibble(hour=seq(-5*tau,5*tau)) %>% mutate(exp=exp(-abs(hour)/tau),gauss=exp(-0.5*(hour/tau)^2))
kern <- kern %>% mutate(matern=matern(hour,tau),cauchy=1/(1+(hour/tau)^2))
kern <- kern %>% pivot_longer(-hour)

kern %>% ggplot()+geom_line(aes(hour,value,colour=name))


integrate(function(t) 1/(1+(t/(0.6366*48))^2),lower = 0, upper=Inf)


#########################
# flexibility forces
##########################

N <- 500
tau <- 24
sep <- 100
l <- rep(1,N)
lp <- l
lp[200-sep/2] <- 1.25
lp[200+sep/2] <- 0.75
df <- tibble(t=1:N,l=l,lp=lp)
g_kernel <- function(k,t){
  matern(k-t,tau)
}
cost <- function(t){

  sum((lp-1)*sapply(1:N, function(k) g_kernel(k,t)))^2

}

total_cost <- function(sep,tau){

  N <- 500
  l <- rep(1,N)
  lp <- l
  lp[200-sep/2] <- 1.25
  lp[200+sep/2] <- 0.75
  df <- tibble(t=1:N,l=l,lp=lp)
  g_kernel <- function(k,t){
    matern(k-t,tau)
  }
  cost <- function(t){

    sum((lp-1)*sapply(1:N, function(k) g_kernel(k,t)))^2

  }

  total_cost <- sum(sapply(1:N,cost))
  return(total_cost)
}

df <- expand_grid(sep=seq(1,60),tau=seq(3,60,by=3)) %>% rowwise() %>% mutate(c=total_cost(sep,tau))

df %>% filter(tau %in% c(12)) %>% ggplot(aes(sep,c,colour=factor(tau)))+geom_line()


df <- df %>% mutate(k2=matern(t-100,24))
df <- df %>% rowwise() %>% mutate(cost=cost(t))
g1 <- df %>% ggplot()+geom_line(aes(t,lp)) +geom_line(aes(t,lp),colour="red")
g2 <- df %>% ggplot()+ geom_line(aes(t,k2),colour="grey50")
g1/g2

df$cost %>% sum()
df %>% ggplot(aes(t,cost))+geom_line()

tariff_plan <- "flat"

get_full_annual_cost <- function (yeartime = 2030, kWh = 8760, tariff_plan, phi = 0.5,
          gamma = 10, eta = 1, tau = 24, kernel = "exp", natural_profile = "LP1",
          prices_scen)
{
  stopifnot(tariff_plan %in% c("flat", "tou", "tou_old", "dynamic"))
  profile <- tolower(natural_profile)
  load <- depmicrosimr::load_profiles_generalised %>% dplyr::select(datetime,
                                                                    any_of(profile))
  prices_scen_1 <- prices_scen %>% dplyr::inner_join(load,
                                                     by = c("datetime", profile)) %>% dplyr::filter(tariff_plan ==
                                                                                                      .env$tariff_plan)
  start_time <- lubridate::date_decimal(yeartime)
  end_time <- lubridate::date_decimal(yeartime + 1)
  df <- prices_scen_1 %>% dplyr::filter(datetime >= start_time,
                                        datetime <= end_time)
  df$load <- df[[profile]] * kWh
  df <- df %>% dplyr::select(datetime, load, price) %>% dplyr::arrange(datetime)
  W <- ceiling(5 * tau)
  lags <- 0:W
  plags <- sqrt(5) * lags/(0.8385 * tau)
  kernel_values <- if (kernel == "matern") {
    (1 + plags + (plags^2)/3) * exp(-plags)
  }
  else if (kernel == "cauchy") {
    1/(1 + (lags/(0.6366 * tau))^2)
  }
  else if (kernel == "exp") {
    exp(-(lags/tau))
  }
  else {
    exp(-(lags/(1.128 * tau))^2)
  }
  frob_sq <- sum(kernel_values^2) + sum(kernel_values[-1]^2)
  p_ref <- median(df$price)
  L_ref <- mean(df$load) * (1 - phi)
  parameter_scaling <- p_ref/L_ref
  gamma_scaled <- gamma * parameter_scaling/frob_sq
  eta_scaled <- eta * parameter_scaling
  compute_behavioural_cost <- function(x, tau) {
    N <- length(x)
    r <- exp(-1/tau)
    a <- numeric(N)
    a[1] <- x[1]
    for (t in 2:N) {
      a[t] <- x[t] + r * a[t - 1]
    }
    b <- numeric(N)
    b[N] <- 0
    if (N > 1) {
      for (t in (N - 1):1) {
        b[t] <- r * (b[t + 1] + x[t + 1])
      }
    }
    y <- a + b
    return(sum(y^2))
  }
  if (tariff_plan != "flat") {
    df <- get_flex(df, phi, gamma, eta, tau, kernel)
    bill_inflex <- sum(df$price * df$load)
    bill_flex <- sum(df$price * df$load_opt)
    ramping_cost <- eta_scaled * sum(diff(df$x)^2)
    behavioural_cost <- gamma_scaled * compute_behavioural_cost(df$x,
                                                                tau)
  }
  else {
    bill_inflex <- sum(df$price * df$load)
    bill_flex <- bill_inflex
    ramping_cost <- 0
    behavioural_cost <- 0
  }
  return(data.frame(tariff_plan = tariff_plan, kWh = kWh, phi = phi,
                    gamma = gamma, eta = eta, tau = tau, annual_bill_inflexible = bill_inflex,
                    annual_bill_flexible = bill_flex, fin_gain = round(bill_flex -
                                                                         bill_inflex), penalty = behavioural_cost, kinetic = ramping_cost,
                    real_gain = round(bill_flex + behavioural_cost + ramping_cost -
                                        bill_inflex)))
}

# ==============================
# ===============================
# flexible profile check
# Does ToU profile looks like LP2?
# find optimal values of gamma and eta
# =================================
# ===================================

prices_scen <- set_prices(sD)
social_network <- make_artificial_society(dep_society_1,homophily,nu=4.5)

profiler <- function(eta,gamma,N=10){
  #
  agents_init <- initialise_agents(sD,2019,prices_scen,social_network,eta=eta,gamma=gamma)
  agents_init <- agents_init %>% filter(natural_profile=="lp1")
  n_agent <- dim(agents_init)[1]
  print(paste("n_agent=",n_agent))
  res <- get_aggregate_test_profile(2026,agents_init[sample(1:n_agent,N),],"tou",prices_scen)
  res <- res %>% inner_join(load_profiles,by="datetime") %>% mutate(optimised_load_profile=optimised_load/sum(optimised_load))
  res <- res %>% mutate(err=abs(optimised_load_profile-lp2),rel_err = abs(optimised_load_profile-lp2)/lp2)
  sum(res$err)
}

#profiler(0.4,10,100) 0.225
#profiler(0.4,20,100) 0.1676211
#profiler(0.4,50,100)  0.1647912
#profiler(0.4,100,100) 0.2152141
profiler(0.6,2,10)

flex_scores <- flex_scores %>% arrange(phi,gamma,eta,tau)
df_err <- tibble()
for(eta in flex_scores$eta %>% unique())
  for(gamma in flex_scores %>% filter(gamma <= 5) %>% pull(gamma) %>% unique()){
    print(paste(gamma,eta))
    df <- tibble(eta=eta,gamma=gamma) %>% mutate(err=profiler(eta,gamma,10))
    print(df)
    write_csv(df, "~/Policy/CAMG/Dynamic Pricing/df_err.csv",append=T)
    df_err <- df_err %>% bind_rows(df)
  }


demand <- prices_scen
demand <- demand %>% dplyr::filter(lubridate::year(datetime)==2030)
demand <- demand %>% dplyr::inner_join(load_profiles_generalised %>% dplyr::select(datetime,lp1))
demand <- demand %>% dplyr::mutate(load=8760*lp1) %>% dplyr::select(-lp1)
demand <- demand %>% dplyr::filter(tariff_plan=="tou") %>% dplyr::select(datetime,price,load)

test <- get_flex(demand,1,30,0.5,24)
test %>% filter(week(datetime)==10) %>% ggplot() + geom_line(aes(datetime,load),linetype="dotted") + geom_line(aes(datetime,load_opt))

test0 <- test %>% group_by(hour=hour(datetime)) %>% summarise(load=mean(load),load_opt=mean(load_opt), fload=mean(fload))
test0  %>% ggplot() + geom_line(aes(hour,load),linetype="dotted") + geom_line(aes(hour,load_opt))


test <- get_profile(2025,4000,"tou",0.8,10,0.1,48,"LP1",prices_scen)

test %>% filter(week(datetime)==10) %>% ggplot() + geom_line(aes(datetime,natural_load),linetype="dotted") + geom_line(aes(datetime,optimised_load))

get_aggregate_test_profile <- function(year,agents_init,tariff_plan,prices_scen){

  agents_init <- agents_init %>% mutate(date=ymd(paste(year,"01","01",sep="-")))

  func <- function(kWh, phi, gamma, eta, tau, natural_profile) get_profile(year,kWh, tariff_plan, phi, gamma, eta, tau, natural_profile,prices_scen)

  agents_init <- agents_init %>% filter(tariff_plan != "tou_old")
  agents_init <- agents_init %>% select(kWh,phi, gamma, eta, tau, natural_profile)
  #aggregate by tariff_plan


  res <- pmap(agents_init, func) |> list_rbind() |> group_by(datetime) |> summarise(natural_load=sum(natural_load),optimised_load = sum(optimised_load, na.rm = TRUE), .groups = "drop")
  res$tariff_plan <- "tou"


  return(res)

}

agents_init <- initialise_agents(sD,2019,prices_scen,social_network,eta=0.4,gamma=2)

agents_in <- agents_init %>% filter(natural_profile=="lp1")
n_agents <- dim(agents_in)[1]
res <- get_aggregate_test_profile(2026,agents_in[sample(1:n_agents,50),],"tou",prices_scen)

g1 <- res %>% filter(week(datetime)==24) %>% ggplot() + geom_line(aes(datetime,natural_load),linetype="dotted") + geom_line(aes(datetime,optimised_load),linetype="solid")
g1 <- g1 + theme_minimal()

g2 <- load_profiles %>% filter(week(datetime)==24) %>% ggplot() + geom_line(aes(datetime,lp1),linetype="dotted") + geom_line(aes(datetime,lp2),linetype="solid")
g2 <- g2 + theme_minimal()
#
#dev.new()
g1/g2 + plot_annotation(title = "eta=0.5")

#match aggregate profile to LP2

res <- res %>% inner_join(load_profiles)
res <- res %>% mutate(optimised_load_profile=optimised_load/sum(optimised_load)) %>% mutate(err=abs(optimised_load_profile-lp2))
sum(res$err)

obsv_shift <- load_profiles %>% group_by(hour=hour(datetime)) %>% summarise(obsv_diff=mean(lp1-lp2))
model_shift <-  res  %>% group_by(hour=hour(datetime)) %>% summarise(model_diff=mean(natural_load-optimised_load))

shift <- obsv_shift %>% inner_join(model_shift)

coef <- lm(model_diff~obsv_diff,shift) %>% coefficients()
shift <- shift %>% mutate(obsv_diff= coef[2]*obsv_diff)

g <- shift %>% pivot_longer(-hour) %>% filter(name != "ratio") %>% ggplot(aes(hour,-value,colour=name))+geom_line(linewidth=2) + theme_minimal()
 g + scale_colour_canva(palette="Playful greens and blues")


shift %>% ggplot(aes(hour,difference))+geom_area()


######################
# mean
#######################

