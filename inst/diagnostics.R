library(depmicrosimr)
library(tidyverse)
library(patchwork)
library(ggthemes)

scen <- sD
scen <- scen %>% mutate(value=replace(value,parameter=="theta.",0))

abm_theta_0.1 <- test
write_rds(abm_theta_0.1,"~/Policy/CAMG/Dynamic Pricing/ABM_outputs/abm_theta_0.1.RData")

abm_theta_0.0 <- runABM(scen,1,2040)

uptake <- abm_theta_0.0[[1]] %>% group_by(date) %>% count(tariff_plan)

g <- uptake %>% ggplot(aes(date,n/1217,fill=tariff_plan))+geom_area() +theme_minimal()
g <- g + scale_fill_canva(palette = "Fun and cheerful") + geom_vline(xintercept = ymd("2026-01-01"),linetype="dotted")
export::graph2ppt(g,"~/Policy/CAMG/Dynamic Pricing/ABM_outputs/hello_world.ppt")

prices_scen <- set_prices(sD)

g1 <- prices %>% filter(tariff_plan=="dynamic") %>% ggplot(aes(datetime,price, colour=(year(datetime)>2025)))+geom_line()
g1 <- g1 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,1))
g1
g3 <- prices %>% filter(tariff_plan=="dynamic", year(datetime)==2035,week(datetime)==48) %>% ggplot(aes(datetime,price)) +geom_line(colour="grey60",linewidth=1.2)
g3 <- g3 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,1))
#export::graph2ppt(g1,"~/Policy/CAMG/reports/Empower2/report_AMJ_2026/heteroskedastic_prices.png")
#
demand <- prices
demand <- demand %>% dplyr::filter(lubridate::year(datetime)==2035)
demand <- demand %>% dplyr::inner_join(load_profiles_generalised %>% dplyr::select(datetime,lp1))
demand <- demand %>% dplyr::mutate(load=8760*lp1) %>% dplyr::select(-lp1)
demand <- demand %>% dplyr::filter(tariff_plan=="tou") %>% dplyr::select(datetime,price,load)

week_no <- 42
g3 <- prices %>% filter(tariff_plan=="dynamic", year(datetime)==2035,week(datetime)==week_no) %>% ggplot(aes(datetime,price)) +geom_line(colour="grey60",linewidth=1.2)
g3 <- g3 + theme_minimal() + scale_colour_canva() + scale_y_continuous(limits=c(0,0.75))
test <- get_flex(demand,phi=0.25,gamma=10,eta=1,tau=96)
g1 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g1 <- g1 + geom_line(aes(datetime,load_opt),colour="red",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g1 <- g1 + theme_minimal() + scale_colour_canva() #+ scale_y_continuous(limits=c(0,1))
test <- get_flex(demand,phi=0.25,gamma=3,eta=1,tau=96)
g2 <- test %>% filter(week(datetime)==week_no) %>% ggplot()
g2 <- g2 + geom_line(aes(datetime,load_opt),colour="blue",linewidth=1.2) +geom_line(aes(datetime,load),linetype="dotted")
g2 <- g2 + theme_minimal() + scale_colour_canva() #+ scale_y_continuous(limits=c(0,1))
g3/g1/g2
export::graph2ppt(g3/g1/g2,"~/Policy/CAMG/reports/Empower2/report_AMJ_2026/load_shifting.png")


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

#load dependence
df <- tibble()
for(kWh in seq(1000,20000,by=1000)){
  df0 <- get_annual_cost(2030,kWh,"tou",0.5,0.25,1,48,"LP1",prices_scen)
  df0$kWh <- kWh
  df <- df %>% bind_rows(df0)
}

df %>% ggplot()+geom_line(aes(kWh,annual_bill_inflexible)) + geom_line(aes(kWh,annual_bill_flexible),colour="red")

#gamma dependence

df <- tibble()
for(tau in seq(24,96,by=12))
for(gamma in c(0.1,0.25,0.5,0.75,1,2,5,10)){
  df0 <- tariff_plan_bills(2030,8760,0.25,gamma,0.2,tau,"LP1",prices_scen)
  df0$phi <- 0.25
  df0$gamma <- gamma
  df0$eta <- 0.2
  df0$tau <- tau
  df <- df %>% bind_rows(df0)
}


df %>% filter(tau==96) %>% ggplot()+geom_line(aes(gamma,annual_bill,colour=tariff_plan)) + facet_wrap(.~tau) + scale_x_continuous(trans="sqrt")

#tau dependence

df <- tibble()
for(tariff in c("flat","tou","dynamic"))
  for(tau in seq(12,96,by=12)){
    df0 <- get_annual_cost_fast(2030,8760,tariff,0.5,2,1,tau,"LP1",prices_scen)
    df0$tau <- tau
    df0$tariff <- tariff
    df <- df %>% bind_rows(df0)
  }


df %>% ggplot()+geom_line(aes(tau,gain,colour=tariff)) #+ geom_line(aes(tau,annual_bill_flexible),colour="red") + scale_y_continuous(limits=c(2600,3600))


#phi dependence

df <- tibble()
for(tariff in c("flat","tou","dynamic"))
  for(phi in seq(0,1,by=0.1)){
    df0 <- get_annual_cost(2030,8760,tariff,phi,0.25,1,24,"LP1",prices_scen)
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

prices_scen <- get_tariff_prices(sD)
get_annual_cost(2030,w,tariff="tou",prices_scen=prices_scen)
#

get_flex_scores <- function(scen,year,kWh,tariff_plan,phi,gamma,eta,tau,profile="LP1",prices_scen){
  #
  demand <- prices_scen %>% dplyr::filter(lubridate::year(datetime)==year)
  demand <- demand %>% dplyr::inner_join(load_profiles_generalised %>% dplyr::select(datetime,tolower(profile)))
  demand <- demand %>% dplyr::mutate(load=kWh*lp1) %>% dplyr::select(-lp1)
  demand <- demand %>% dplyr::filter(tariff_plan==.env$tariff_plan) %>% dplyr::select(datetime,price,load)
  flex <- get_flex(demand,phi,gamma,eta,tau,precision = 1e-3)
  flex_hourly <- 100*sum(abs((flex$load_opt-flex$load)))/kWh
  flex1 <- flex %>% dplyr::group_by(lubridate::yday(datetime)) %>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_interday <- 100*sum(abs(flex1$load_opt-flex1$load))/kWh
  flex1 <- flex %>% dplyr::group_by(lubridate::week(datetime)) %>% dplyr::summarise(load=sum(load),load_opt=sum(load_opt))
  flex_interweek <- 100*sum(abs(flex1$load_opt-flex1$load))/kWh
  tibble::tibble(tariff_plan=tariff_plan,profile=profile,phi=phi,gamma=gamma,eta=eta,tau=tau,flex_hour=flex_hourly,flex_day=flex_interday,flex_week=flex_interweek)
}

get_flex_scores(sD,2026,8760,"tou",0,0,0.5,48,profile="LP1",prices_scen)

get_flex_scores(sD,2026,8760,"tou",0,0.1,0.5,48,profile="LP1",prices_scen)

#make flex scores table
flex_scores_new <- tibble()
for(tariff_plan in c("tou","dynamic"))
 for(phi in seq(0,1,by=0.2))
  for(gamma in c(0,0.1,0.25,0.5,1,2,5,20))
    for(tau in 120)
      for(eta in c(0.1,0.2,0.5,1,2)){
      print(paste("gamma=",gamma))
      flex_scores_new <- flex_scores_new %>% bind_rows(get_flex_scores(sD,2026,8760,tariff_plan,phi,gamma,eta,tau,"LP1",prices_scen))
      }

#write_csv(flex_scores_new,"C:/Users/Joe/pkgs/depmicrosimr/inst/ext_data/flex_scores.csv")

flex_scores_new %>% filter(phi==0,tariff_plan=="tou") %>% ggplot(aes(gamma,flex_hour,colour=factor(eta)))+geom_line() + facet_wrap(.~tau)

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
