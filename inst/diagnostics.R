library(depmicrosimr)
library(tidyverse)
library(patchwork)

prices_scen <- get_tariff_prices(sD)

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
for(tariff in c("flat","tou","dynamic"))
for(gamma in seq(0.1,10,by=0.25)){
  df0 <- get_annual_cost(2030,8760,tariff,0,gamma,1,96,"LP1",prices_scen)
  df0$gamma <- gamma
  df0$tariff <- tariff
  df <- df %>% bind_rows(df0)
}


df %>% ggplot()+geom_line(aes(gamma,gain,colour=tariff)) #+ geom_line(aes(tau,annual_bill_flexible),colour="red") + scale_y_continuous(limits=c(2600,3600))

#tau dependence

df <- tibble()
for(tariff in c("flat","tou","dynamic"))
  for(tau in seq(12,96,by=12)){
    df0 <- get_annual_cost(2030,8760,tariff,0.5,2,1,tau,"LP1",prices_scen)
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


#why is tou outperforming dynamic?
prices <- sem_prices_2019_2025 %>% mutate(hour=hour(datetime)) %>% inner_join(tou_tariffs %>% rename("hour"=start))

network_charges <- tibble(tariff=c("day","night","peak"),network=c(0.1981,0.0852,0.2255))
#
prices <- prices %>% inner_join(network_charges) %>% mutate(dynamic=network+price/1000*1.09)
prices %>% group_by(tariff) %>% summarise(tou=mean(dynamic))

prices %>% filter(year(datetime)==2025)

prices <- prices %>% mutate(tou=case_when(tariff=="night"~night_tariff_fun(sD,decimal_date(datetime)),
                                          tariff=="day"~day_tariff_fun(sD,decimal_date(datetime)),
                                          tariff=="peak"~peak_tariff_fun(sD,decimal_date(datetime))))
prices_tou <- prices %>% group_by(year=year(datetime),tariff) %>% summarise(price=mean(price)/1000,tou=mean(tou))
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



