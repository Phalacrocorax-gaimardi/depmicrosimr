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
#' prices_scen <- get_price_load_scen(sD)
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
  tous <- tibble::tibble(q41=c(1,2),tariff=c("flat","tou"))
  agents_in <- agents_in %>% dplyr::inner_join(demand) %>% dplyr::inner_join(tous)
  county_codes <- dep_qanda %>% dplyr::filter(question_code=="qc1") %>% dplyr::rename("qc1"=response_code)
  county_codes <- county_codes %>% dplyr::rename("county"=response) %>% dplyr::select(qc1,county)
  area_codes <- dep_qanda %>% dplyr::filter(question_code=="qg") %>% dplyr::rename("qg"=response_code)
  area_codes <- area_codes %>% dplyr::rename("area"=response) %>% dplyr::select(qg,area)
  area_codes <- area_codes %>% dplyr::mutate(area=dplyr::if_else(qg %in% c(1,3),"Rural","Urban"))
  agents_in <- agents_in %>% dplyr::inner_join(area_codes) %>% dplyr::inner_join(county_codes)
  agents_in <- agents_in %>% dplyr::inner_join(smart_meter_rollout)
  #
  agents_in <- agents_in %>% dplyr::select(serial,kWh,tariff,yeartime,area)
  #combine with structural params
  agents_in <- agents_in %>% dplyr::inner_join(struct_params)
  #rollout year
  #add flex params
  agents_in$eta <- 1
  #generate "flexibility scores" (hourly MAD load-shifting values) range from min_flex to max_flex%
  min_flex <- sD %>% dplyr::filter(parameter=="min_flex") %>% dplyr::pull(value)
  max_flex <- sD %>% dplyr::filter(parameter=="max_flex") %>% dplyr::pull(value)
  #scale heterogeneous flexibilities to lie between min_flex and max_flex
  agents_in <- agents_in %>% dplyr::mutate(flex_score= min_flex+max_flex*(flexibility - min(flexibility))/(max(flexibility)-min(flexibility)))
  #standardized_z <- agents_in$flexibility/sd(agents_in$flexibility)
  #agents_in$flex_score <- (standardized_z * (15 / 2.576)) + 15
  #agents_in$flex_score <- pmax(1.01*min(score_matrix),agents_in$flex_score)
  #sample flexible parameter values based on flex_score
  score_cube <- flex_score_cube()
  agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(match_flex_params(flex_score,score_cube)) %>% dplyr::ungroup()
  #rescale eta and gamma parameters according to mean hourly demand
  # reduced effect of quadratic
  #agents_in <- agents_in %>% dplyr::mutate(eta=eta*(8760/kWh), gamma=gamma*(8760/kWh))
  #rescale proactive: 0 to theta_max: theta_max very risk intolerant
  theta_max <- sD %>% dplyr::filter(parameter=="theta.") %>% dplyr::pull(value)
  agents_in <- agents_in %>% dplyr::mutate(theta = theta_max*(1-(proactive - min(proactive))/(max(proactive)-min(proactive))))
  #assign load profile codes : currently only a single profile
  agents_in <- agents_in %>% dplyr::mutate(profile=dplyr::case_when(area=="Urban"&tariff=="flat"~"lp1",
                                                                    area=="Urban"&tariff=="tou"~"lp2",
                                                                    area=="Rural"&tariff=="flat"~"lp3",
                                                                    area=="Rural"&tariff=="tou"~"lp4"))
  agents_in <- agents_in %>% dplyr::select(-flexibility,-proactive,-inertia,-flex_score)
  #current annual bills
  #agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(annual_bill=get_annual_cost(2019,kWh,tariff,phi,gamma,eta,tau,"lp1",prices_scen)$annual_bill_flexible)
  get_bill <- function(kWh, tariff, profile) {get_annual_cost_base(start_year,kWh,tariff,profile,prices_scen)}

  agents_in <- agents_in <- agents_in  %>% dplyr::mutate(annual_bill = purrr::pmap_dbl(list(kWh, tariff, profile), get_bill))
  #tidy up
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
#' @param sD  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param price_scen tariff plan price assumptions
#' @param social_network artificial social network
#' @param ignore_social option to ignore social effects. Default is FALSE.
#' @param quiet TRUE to suppress messages
#'
#' @return updated agent dataframe
#' @export
#' @examples
#'
#' prices_scen <- get_price_load_scen(sD)
#' agents_in <- initialise_agents(sD,2019,prices_scen)
#' social_network <- make_artificial_society(dep_society,homophily,nu=4.5)
#' #agents_1 <- update_agents(sD,2026+1/6,agents_in,social_network,quiet=FALSE)

update_agents <- function(sD,yeartime,agents_in, price_scen, social_network,ignore_social=F,quiet=TRUE){
  #
  #beta. <- 0.2532785
  #params at yeartime
  params <- scenario_params(sD,yeartime)
  #
  #empirical_u <- hp_empirical_utils %>% dplyr::filter(calibration_run==cal_run) %>% dplyr::select(-calibration_run)
  #social utility - knowing others who have installed an heat pump
  #du_social <- dplyr::filter(empirical_u,question_code=="q52")$du_average
  #theta <- dplyr::filter(empirical_u,question_code=="theta")$du_average

  a_s <- agents_in
  n_dynamic <- dim(a_s %>% dplyr::filter(tariff=="dynamic"))[1]
  #calculate current annual electricity cost
  #a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(bill = ,params,"None",include_rebound = FALSE))
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(eac_actual = annualised_heating_system_cost(hli,tech,heating_install_time,"new",floor_area,house_type,construction_year,params,"None",include_rebound = TRUE))
  #update definitions of old and new for all agents
  #a_s <- a_s %>% dplyr::mutate(S1_old=S1_new,S2_old = S2_new,B_old=B_new)
  #a_s <- a_s %>% dplyr::mutate(capex_old=capex_new,opex_old=opex_new)
  #this subsample of agents decide to look at rooftop pv
  #assing any heating tech breakdowns during timestep
  a_s <- a_s %>% dplyr::mutate(failure = weibull_failure(heating_install_time,params$yeartime,params$yeartime+1/6,tech))
  a_s <- dplyr::ungroup(a_s)
  #filter on failure
  b_s1 <- a_s %>% dplyr::filter(failure)
  #print(paste("number of heating system failures",dim(b_s1)[1]))
  b_s1 <- b_s1 %>% dplyr::select(-heat_pump_grant,-grant_type)
  #filter on system upgraders
  b_s2 <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*params$p.))
  b_s2 <- b_s2 %>% dplyr::select(-heat_pump_grant)
  #print(paste("number of potential upgraders",dim(b_s2)[1]))
  #households that consider full upgrade following failure
  b_s3 <- b_s1 %>% dplyr::filter(serial %in% b_s2$serial)
  #exclude failure where upgrade is being implemented => just two categories failure and upgr
  b_s1 <- b_s1 %>% dplyr::filter(!(serial %in% b_s3$serial))

  if(nrow(b_s1)==0 & nrow(b_s2)==0) return(a_s)

  hp_savings_env <- function(hli,tech, house_type,storeys, construction_year, region, floor_area,eta) {
    heat_pump_savings(hli,tech, params$yeartime, house_type, storeys,construction_year, region, floor_area, eta,params)
  }
  hp_upgrade_savings_env <- function(hli,tech, heating_install_time,house_type, storeys,construction_year, region, floor_area,eta,fuel_allowance) {
    heat_pump_upgrade_savings(hli,tech, heating_install_time, house_type,storeys, construction_year, region, floor_area, eta, params,fuel_allowance,include_grants = TRUE)
  }

  ########################
  # Heating system failures
  ########################

  #logic: if has heat pump replace
  b_s0 <- b_s1 %>% dplyr::select(hli,tech,house_type, storeys,construction_year, region, floor_area,eta)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  # should REBOUND be inlcuded at this step?
  df <- purrr::pmap(b_s0,hp_savings_env)
  df <- do.call(rbind,df)
  b_s1 <- b_s1 %>% dplyr::bind_cols(df)
  ##############################
  #heat_pump failures. choose between retaining the heat pump or switching to gas
  ##############################
  b_s1_hp <- b_s1 %>% dplyr::filter(tech=="heat_pump")
  #print(paste("number of heat pump failures", dim(b_s1_hp)[1]))
  # CORRECT
  if(nrow(b_s1_hp) > 0) {
    #b_s1_hp_stick <- b_s1_hp %>% dplyr::filter(savings <= 0 & hli <= params$hli_heat_pump_threshold)
    b_s1_hp_stick <- b_s1_hp %>% dplyr::filter(savings <= 0)
    #b_s1_hp_switch <- b_s1_hp %>% dplyr::filter(savings > 0 | hli > params$hli_heat_pump_threshold )  #
    b_s1_hp_switch <- b_s1_hp %>% dplyr::filter(savings > 0)
    #print("BUG??")
    b_s1_hp_stick <- b_s1_hp_stick %>% dplyr::mutate(eac=eac_stick,tech_cost=tech_cost_stick)
    b_s1_hp_switch <- b_s1_hp_switch %>% dplyr::mutate(eac=eac_switch,tech_cost=tech_cost_switch)
    #b_s1_hp_switch <- b_s1_hp_switch %>% dplyr::select(-eac_stick,-eac_switch,-savings,-hp_grant_type)
    b_s1_hp_switch$tech <- "gas"
    #print(paste("number of heat pump retainers", dim(b_s1_hp_stick)[1]))
    b_s1_hp <- b_s1_hp_stick %>% dplyr::bind_rows(b_s1_hp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  b_s1_hp <- b_s1_hp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

  ################################
  #non heat pump failures
  ############################
  #reject heat pump adoption when financial utility does not overcome barriers
  b_s1_nhp <- b_s1 %>% dplyr::filter(tech != "heat_pump" )
  if(nrow(b_s1_nhp) > 0) {
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_fin = -params$nu.*w_q13*savings)
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_social = w_q52*du_social[q52])
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_theta = w_theta*theta)
    #sum and include hypothetical bias correction (default is zero)
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="oil" & yeartime >= params$oil_boiler_ban,1,du_tot))
    b_s1_nhp <- b_s1_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="gas" & yeartime >= params$gas_boiler_ban,1,du_tot))
    #COULD ASSUME A HLI THRESHOLD FOR FAILURE ADOPTERS i.e. clearly heat pump ready
    #b_s1_nhp_switch <- b_s1_nhp %>% dplyr::filter(du_tot > 0 & hli <= params$hli_heat_pump_threshold)
    b_s1_nhp_switch <- b_s1_nhp %>% dplyr::filter(du_tot > 0)
    b_s1_nhp_switch$tech <- "heat_pump"
    #b_s1_nhp_stick <- b_s1_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0 | hli > params$hli_heat_pump_threshold)
    b_s1_nhp_stick <- b_s1_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
    #print(paste("number of heat pump adopters",dim(b_s1_nhp_switch)[1]))
    #stickers
    b_s1_nhp_stick <- b_s1_nhp_stick %>% dplyr::mutate(eac=eac_stick,tech_cost=tech_cost_stick)
    b_s1_nhp_stick <- b_s1_nhp_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot)
    b_s1_nhp_stick$heat_pump_grant <- 0
    #b_s1_stick$adopt <- FALSE
    #switchers
    b_s1_nhp_switch <- b_s1_nhp_switch %>% dplyr::mutate(eac=eac_switch, tech_cost=tech_cost_switch)
    b_s1_nhp_switch <- b_s1_nhp_switch %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot)
    b_s1_nhp <- b_s1_nhp_stick %>% dplyr::bind_rows(b_s1_nhp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  b_s1_nhp <- b_s1_nhp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

  #if(dim(b_s1_hp)[1] != 0 ) stopifnot(dim(b_s1_hp_stick)[1] + dim(b_s1_hp_switch)[1] == dim(b_s1_hp)[1])
  b_s1 <- b_s1_hp %>% dplyr::bind_rows(b_s1_nhp)
  #calculate including rebound eac_actual
  b_s1 <- b_s1 %>% dplyr::select(!dplyr::matches("switch|stick"))
  #recompute BER - there will be an improvement
  if(nrow(b_s1)>0) b_s1 <- b_s1 %>% dplyr::mutate(ber=ber_from_hli(hli,tech,heating_install_time,params))

  ################################
  # Energy Efficiency upgrades
  #################################

  ######################
  # each agent chooses between (1) optimum fabric upgrade + sticking with current heating tech
  # (2) fabric upgrade and switching to heat pump
  # (3) reject upgrade do nothing
  ######################
  #print("b_s2")
  b_s2$upgrade <- TRUE
  b_s0 <- b_s2 %>% dplyr::select(hli,tech,heating_install_time,house_type,storeys, construction_year, region, floor_area,eta,fuel_allowance)
  #df <- purrr::pmap(b_s0,optimise_heat_env)
  df <- purrr::pmap(b_s0,hp_upgrade_savings_env)
  df <- do.call(rbind,df)
  b_s2 <- b_s2 %>% dplyr::bind_cols(df)
  #############################################################################################
  # households that reject efficiency upgrade because of insufficient reward financial relative to disruption
  ##############################################################################################
  print(paste(dim(b_s2 %>% dplyr::filter(eac_stick >= eac_old & eac_switch >= eac_old))[1],"upgrades rejected"))
  b_s2 <- b_s2 %>% dplyr::filter(eac_stick < eac_old | eac_switch < eac_old) %>% dplyr::select(-eac_old)
  #b_s2 %>% dplyr::select(tech,hli,hli_stick,hli_switch,eac_stick,eac_switch,savings)
  ################################################################
  # Efficiency upgrade where there is an existing heat pump
  #################################################################
  b_s2_hp <- b_s2 %>% dplyr::filter(tech=="heat_pump")
  #print(paste("b_s2_bp"))
  #print(b_s2_hp)
  if(nrow(b_s2_hp) == 0) b_s2_hp <- b_s2_hp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))
  if(nrow(b_s2_hp) > 0){
    b_s2_hp_switch <- b_s2_hp %>% dplyr::filter(savings > 0)
    b_s2_hp_stick <- b_s2_hp %>% dplyr::filter(savings <= 0)
    #
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::select(-any_of("savings"))
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::mutate(hli = hli_stick,eac=eac_stick,upgrade_cost=upgrade_cost_stick,
                                                     tech_cost=tech_cost_stick, grant_type="None")
    b_s2_hp_stick <- b_s2_hp_stick %>% dplyr::select(!dplyr::matches("switch|stick"))

    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::select(-any_of("savings"))
    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::mutate(hli = hli_switch,eac=eac_switch,upgrade_cost=upgrade_cost_switch,
                                                       tech_cost=tech_cost_switch, grant_type="None")
    b_s2_hp_switch <- b_s2_hp_switch %>% dplyr::select(!dplyr::matches("switch|stick"))

    b_s2_hp <- b_s2_hp_stick %>% dplyr::bind_rows(b_s2_hp_switch)
  }
  #####################################################################
  # Efficiency upgrade where existing heating tech is not a heat pump
  #####################################################################
  #Are financial savings are strong enough?
  b_s2_nhp <- b_s2 %>% dplyr::filter(tech!="heat_pump")
  #what happens if b_s2_nhp is empty?
  if(nrow(b_s2_nhp)==0)  b_s2_nhp <- b_s2_nhp %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))

  if(nrow(b_s2_nhp) > 0) {
    b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_fin = -params$nu.*w_q13*savings) #financial
    b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_social = w_q52*du_social[q52]) #social influence
    b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_theta = w_theta*theta) #barrier
    #sum and include a possible hypothetical bias correction (default is zero but you might need it)
    b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_tot = du_fin+du_social+du_theta + params$lambda.)
    b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="oil" & yeartime >= params$oil_boiler_ban,1,du_tot))
    b_s2_nhp <- b_s2_nhp %>% dplyr::mutate(du_tot = dplyr::if_else(tech=="gas" & yeartime >= params$gas_boiler_ban,1,du_tot))

    #adopters
    b_s2_nhp_switch <- b_s2_nhp %>% dplyr::filter(du_tot > 0)
    #non-adopters
    b_s2_nhp_stick <- b_s2_nhp %>% dplyr::filter(is.na(du_tot) | du_tot <= 0)
    #clean up non-adopters e.g. remove redundant "switch" data
    b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-any_of("savings"))
    b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::mutate(hli = hli_stick,eac = eac_stick,upgrade_cost=upgrade_cost_stick,
                                                       tech_cost=tech_cost_stick, grant_type=grant_type_stick,
                                                       upgrade_grant=upgrade_grant_stick,heat_pump_grant=heat_pump_grant_stick)
    b_s2_nhp_stick <- b_s2_nhp_stick %>% dplyr::select(!dplyr::matches("switch|stick"))
    #
    #clean up adopters e.g. remove redundant "stick" data
    b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::select(-du_fin,-du_social,-du_theta,-du_tot,-any_of("savings"))
    b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::mutate(hli = hli_switch,eac = eac_switch,upgrade_cost=upgrade_cost_switch,
                                                         tech_cost=tech_cost_switch, grant_type=grant_type_switch,
                                                         upgrade_grant=upgrade_grant_switch,heat_pump_grant=heat_pump_grant_switch)
    b_s2_nhp_switch <- b_s2_nhp_switch %>% dplyr::select(!dplyr::matches("switch|stick"))
    b_s2_nhp_switch$tech <- "heat_pump"
    #combine adopters and non-adopters
    b_s2_nhp <- b_s2_nhp_stick %>% dplyr::bind_rows(b_s2_nhp_switch) %>% dplyr::mutate(heating_install_time = params$yeartime)
  }
  #combine all upgrades whether existing heat pump or not and update to the new kW installed capacities
  b_s2 <-  b_s2_hp %>% dplyr::bind_rows(b_s2_nhp) %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))

  b_s2 <- b_s2 %>% dplyr::select(-any_of("savings")) %>% dplyr::select(!dplyr::matches("switch|stick"))
  if(nrow(b_s2) > 0 ) b_s2 <- b_s2 %>% dplyr::mutate(ber=ber_from_hli(hli,tech,heating_install_time,params))
  #stopifnot(dim(b_s2_nhp)[1] + dim(b_s2_hp)[1] == dim(b_s2)[1])
  #if(dim(b_s2_nhp)[1] != 0) stopifnot(dim(b_s2_nhp_stick)[1]+dim(b_s2_nhp_switch)[1] == dim(b_s2_nhp)[1])
  #if(dim(b_s2_hp)[1] != 0) stopifnot(dim(b_s2_hp_stick)[1]+dim(b_s2_hp_switch)[1] == dim(b_s2_hp)[1])

  #combine == dim(b_s2)[1])
  ##################################
  #combine failures and upgraders
  #################################
  b_s <- b_s1 %>% dplyr::bind_rows(b_s2) %>% dplyr::ungroup()
  #compute new bers
  #stopifnot(nrow(b_s %>% dplyr::filter(lengths(tech) != 1)) != 0)
  #print(b_s)
  #b_s <- b_s  %>% dplyr::mutate(ber=ber_from_hli(hli,tech,install_time = params$yeartime,params))

  #update agents
  a_s <- dplyr::filter(a_s, !(serial %in% b_s$serial))
  a_s <- dplyr::bind_rows(a_s,b_s) %>% dplyr::arrange(serial)
  a_s <- a_s %>% dplyr::mutate(adopt=(tech=="heat_pump"))
  #a_s <- a_s %>% dplyr::mutate(kW=heating_system_size(ber*floor_area))
  #recompute social variable
  ma <- igraph::as_adjacency_matrix(social_network)
  g <- social_network %>% tidygraph::activate(nodes) %>% dplyr::left_join(a_s,by="serial")
  #social network conformity effect
  adopter_nodes <- igraph::V(g)$adopt==TRUE
  a_s$q52 <- as.numeric(ma %*% adopter_nodes) #social reinforcement 0 no adoption 1 adoption
  if(ignore_social) a_s$q52 <- 1 #no adopters assumed present in local network
  a_s <- a_s %>% dplyr::rowwise() %>% dplyr::mutate(q52 = min(q52+1,4)) #update q52 encoding 1,2,3,4
  #agents_out <- a_s
  #a_s <- a_s %>% dplyr::select(-du_tot)
  if(!quiet) {
    print(paste("time", round(yeartime,1), "number of system breakdowns",dim(b_s1)[1]))
    print(paste("time", round(yeartime,1), "number of efficiency upgrades",dim(b_s2)[1]))
    print(paste("time", round(yeartime,1), "number of heat pump adopters following system breakdown",dim(b_s1_nhp %>% dplyr::filter(tech=="heat_pump"))[1]))
    print(paste("number of heat pump adopters as part of efficiency upgrade",dim(b_s2_nhp %>% dplyr::filter(tech=="heat_pump"))[1]))
  }
  a_s <- a_s %>% dplyr::select(-adopt)
  return(dplyr::ungroup(a_s))
}



