##############################################################
# ABM consists of an initialiser, an update and a run module
###########################################################

#' initialise_agents
#'
#' sets the initial state variables at the beginning of each run (Jan 2019 before smart-meter rollout)\cr
#' \cr
#' 14% of households are on the old (dual meter) day/night tariff.\cr
#' \cr
#' 3-flexibility parameters are initialised based on household flexibility scores. These are an inflexible load fraction (\eqn{\phi}),
#' a cost parameter (\eqn{\gamma}) and the load mean reversion timescale (\eqn{\tau}).
#'
#' @param sD scenario design dataframe
#' @param start_year default 2019
#' @param prices_scen tariff prices dataframe
#'
#' @returns a dataframe with columns serial ID, annual kWh, initial tariff plan, smart meter install time, and behavioural parameters
#' @export
#' @examples
#' prices_scen <- set_prices(sD)
#' initialise_agents(sD,2019,prices_scen)
initialise_agents <- function(sD, start_year=2019,prices_scen){

  #agents_in has a minimal set of survey data
  demand <- survey_bills_to_kwh(dep_survey) %>% dplyr::select(serial,kWh)
  #
  agents_in <- dep_survey %>% dplyr::select(serial,q14,q15,q41,Q41_oth,qc1,qg,qi)
  #remove prepay customers
  prepays <- agents_in %>% dplyr::filter(stringr::str_detect(Q41_oth,"Pay|pay")) %>% dplyr::pull(serial)
  agents_in <- agents_in %>% dplyr::filter(!(serial %in% prepays))
  #assume "dont knows" and "others" and day/night/peak were flat rate customers
  agents_in <- agents_in %>% dplyr::mutate(q41 = replace(q41,q41 %in% c(3,4,5),1))
  #assume that 2/3 of day/night customers were on the old day/night rate in 2019
  agents_in <- agents_in %>% dplyr::mutate(q41 = replace(q41,sample(which(q41 == 2), floor(sum(q41 == 2) / 3)),1))
  tous <- tibble::tibble(q41=c(1,2),tariff_plan=c("flat","tou"))
  agents_in <- agents_in %>% dplyr::inner_join(demand) %>% dplyr::inner_join(tous)
  county_codes <- dep_qanda %>% dplyr::filter(question_code=="qc1") %>% dplyr::rename("qc1"=response_code)
  county_codes <- county_codes %>% dplyr::rename("county"=response) %>% dplyr::select(qc1,county)
  area_codes <- dep_qanda %>% dplyr::filter(question_code=="qg") %>% dplyr::rename("qg"=response_code)
  area_codes <- area_codes %>% dplyr::rename("area"=response) %>% dplyr::select(qg,area)
  area_codes <- area_codes %>% dplyr::mutate(area=dplyr::if_else(qg %in% c(1,3),"Rural","Urban"))
  agents_in <- agents_in %>% dplyr::inner_join(area_codes) %>% dplyr::inner_join(county_codes)
  agents_in <- agents_in %>% dplyr::inner_join(smart_meter_rollout)
  #
  agents_in <- agents_in %>% dplyr::select(serial,kWh,tariff_plan,yeartime,area)
  #combine with structural params
  agents_in <- agents_in %>% dplyr::inner_join(struct_params)
  #rollout year
  #add flex params
  agents_in$eta <- 0.2
  #generate "flexibility scores" (hourly MAD load-shifting values) range from min_flex to max_flex%
  min_flex <- sD %>% dplyr::filter(parameter=="min_flex") %>% dplyr::pull(value)
  max_flex <- sD %>% dplyr::filter(parameter=="max_flex") %>% dplyr::pull(value)
  #scale heterogeneous flexibilities to lie between min_flex and max_flex
  agents_in <- agents_in %>% dplyr::mutate(flex_score= min_flex+max_flex*(flexibility - min(flexibility))/(max(flexibility)-min(flexibility)))
  #standardized_z <- agents_in$flexibility/sd(agents_in$flexibility)
  #agents_in$flex_score <- (standardized_z * (15 / 2.576)) + 15
  #agents_in$flex_score <- pmax(1.01*min(score_matrix),agents_in$flex_score)
  #sample flexible parameter values based on flex_score
  score_cube <- flex_score_cube(eta=0.2)
  agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(match_flex_params(flex_score,score_cube)) %>% dplyr::ungroup()
  #rescale eta and gamma parameters according to mean hourly demand
  # reduced effect of quadratic
  #agents_in <- agents_in %>% dplyr::mutate(eta=eta*(8760/kWh), gamma=gamma*(8760/kWh))
  #rescale proactive: 0 to theta_max: theta_max very risk intolerant
  theta_max <- sD %>% dplyr::filter(parameter=="theta.") %>% dplyr::pull(value)
  agents_in <- agents_in %>% dplyr::mutate(theta = theta_max*(1-(proactive - min(proactive))/(max(proactive)-min(proactive))))
  #assign natural profile codes : currently only an urban/rural profile
  agents_in <- agents_in %>% dplyr::mutate(natural_profile=dplyr::case_when(area=="Urban"~"lp1",
                                                                    area=="Rural"~"lp3"))
  #actual profile
  agents_in <- agents_in %>% dplyr::mutate(profile=dplyr::case_when(area=="Urban"&tariff_plan=="flat"~"lp1",
                                                                    area=="Urban"&tariff_plan=="tou"~"lp2",
                                                                    area=="Rural"&tariff_plan=="flat"~"lp3",
                                                                    area=="Rural"&tariff_plan=="tou"~"lp4"))

  agents_in <- agents_in %>% dplyr::select(-flexibility,-proactive,-inertia)
  #current annual bills
  #agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(annual_bill=get_annual_cost(2019,kWh,tariff,phi,gamma,eta,tau,"lp1",prices_scen)$annual_bill_flexible)
  get_bill <- function(kWh, tariff_plan, profile) {get_annual_cost_simple(start_year,kWh,tariff_plan,profile,prices_scen)}

  agents_in <- agents_in  %>% dplyr::mutate(annual_bill = purrr::pmap_dbl(list(kWh, tariff_plan, profile), get_bill))
  #tidy up
  agents_in$serial <- as.character(agents_in$serial)
  agents_in %>% return()
}


#' update_agents
#'
#' micro-simulation time-step updater
#'
#' The workhorse ABM function.Within a scenario, does a single month update of the agent characteristics. A random sample of agents evaluates their economic and social
#' utilities. Agents evaluate the optimal electricity tariff plan. This includes a dynamic tariff.
#'
#'
#' @param scen  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param prices_scen tariff plan price assumptions
#' @param social_network artificial social network
#' @param ignore_social option to ignore social effects. Default is FALSE.
#' @param quiet TRUE to suppress messages
#'
#' @return updated agent dataframe
#' @export
#' @examples
#'
#' prices_scen <- set_prices(sD)
#' agents_in <- initialise_agents(sD,2019,prices_scen)
#' social_network <- make_artificial_society(dep_society_1,homophily,nu=4.5)
#' #agents_1 <- update_agents(sD,2026+1/6,agents_in,prices_scen,social_network,quiet=FALSE)

update_agents <- function(scen,yeartime,agents_in, prices_scen, social_network,ignore_social=F,quiet=TRUE){
  #
  #params at yeartime
  params <- scenario_params(scen,yeartime)
  #
  #empirical_u <- hp_empirical_utils %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #social utility - knowing others who have installed an heat pump
  #theta <- dplyr::filter(empirical_u,question_code=="theta")$du_average

  a_s <- agents_in
  n_dynamic <- dim(a_s %>% dplyr::filter(tariff_plan=="dynamic"))[1]
  print(paste("initial number of dynamics", n_dynamic))
  #temporary social norm
  du_social <- params$nu.*n_dynamic/dim(a_s)[1]
  #update current annual electricity cost? too slow
  #a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(annual_bill = ))
  a_s <- dplyr::ungroup(a_s)
  #random subset of potential switchers
  b_s <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*params$p.))
  #nore ellipsis to handle uncalled columns
  tariff_plan_bills_env <- function(kWh,phi,gamma,eta,tau,natural_profile,...) tariff_plan_bills(yeartime,kWh,phi,gamma,eta,tau,natural_profile,prices_scen)

  b_s <- b_s %>% dplyr::select(-annual_bill,-tariff_plan)

  b_s_1 <- b_s %>% dplyr::mutate(bills_data = purrr::pmap(dplyr::pick(dplyr::everything()), tariff_plan_bills_env)) %>% tidyr::unnest(bills_data)
  #agent evaluates savings relative to closest non-risky tariff plan
  b_s_1 <- b_s_1 %>% dplyr::group_by(serial) %>% dplyr::slice_min(order_by = annual_bill, n = 2,with_ties = FALSE)
  #evaluate the total utilities
   #
  aux <- b_s_1 %>% dplyr::group_by(serial) %>% dplyr::summarise(
      cheapest_plan = dplyr::first(tariff_plan),
      cheapest_bill = dplyr::first(annual_bill),
      next_plan = dplyr::nth(tariff_plan, 2),
      next_bill = dplyr::nth(annual_bill, 2),
      # Calculates savings compared to the 2nd cheapest plan (the runner-up)
      savings       = (dplyr::first(annual_bill-dplyr::nth(annual_bill, 2))/dplyr::nth(annual_bill, 2)),
      .groups       = "drop")
  b_s_2 <- b_s %>% dplyr::inner_join(aux,by="serial")
  b_s_2 <- b_s_2 %>% dplyr::mutate(du_tot = dplyr::if_else(cheapest_plan=="dynamic", du_social-savings-theta,-savings))
  #only adopt if
  b_s_2 <- b_s_2 %>% dplyr::mutate(tariff_plan = dplyr::if_else(du_tot>0,cheapest_plan,next_plan),
                                   annual_bill=dplyr::if_else(du_tot>0,cheapest_bill,next_bill))#barrier
  b_s <- b_s_2 %>% dplyr::select(-cheapest_plan,-cheapest_bill,-next_plan,-next_bill,-du_tot,-savings)

  b_s$profile <- "computed"
  #update agents
  a_s <- dplyr::filter(a_s, !(serial %in% b_s$serial))
  a_s <- dplyr::bind_rows(a_s,b_s) %>% dplyr::arrange(serial)
  a_s <- a_s %>% dplyr::mutate(dep_adopter=(tariff_plan=="dynamic"))
  #a_s <- a_s %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))
  #recompute social variable
  ma <- igraph::as_adjacency_matrix(social_network)
  g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="serial")
  #social network conformity effect
  adopter_nodes <- igraph::V(g)$dep_adopter==TRUE
  a_s$q_dyn <- as.numeric(ma %*% adopter_nodes) #social reinforcement 0 no adoption 1 adoption
  if(ignore_social) a_s$q_dyn <- 0 #no adopters assumed present in local network
  #a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(q52 = min(q52+1,4)) #update q52 encoding 1,2,3,4
  #agents_out <- a_s
  #a_s <- a_s %>% dplyr::select(-du_tot)
  if(!quiet) {
    print(paste("time", round(yeartime,1), "number of system breakdowns",dim(b_s1)[1]))
    }
  a_s <- a_s %>% dplyr::select(-dep_adopter)
  return(dplyr::ungroup(a_s))
}



