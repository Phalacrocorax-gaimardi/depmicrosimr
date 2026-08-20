##############################################################
# ABM consists of an initialiser, an update and a run module
###########################################################

#' initialise_agents
#'
#' initialise_agents() sets the initial state variables at the beginning of each run (Jan 2019 before smart-meter rollout)\cr
#' \cr
#' The stated household "flexibility" inferred from survey are normally distributed around zero. These assumed to represent logarithms of hourly flexibility in response to
#' ToU pricing. This flexibility score index lies between 0 & 1.
#' \deqn{\frac{1}{2} \frac{ \sum_t | l(t)-l^0(t) |}{\sum_t l^0(t)} }\cr
#' \cr
#' 4-flexibility parameters are initialised based on household flexibility scores. These are an inflexible load fraction (\eqn{\phi}),
#' a cost parameter (\eqn{\gamma}), kinetic parameter (\eqn{\eta}) and the load mean reversion timescale (\eqn{\tau}).The current version fixes
#' adjusts \eqn{\gamma} and \eqn{\eta} so that the *aggregate* flexibility matches the difference between 2026 LP1 and LP2 profiles. The heterogenity
#' in flexibility is described by \eqn{\phi-\tau} parameter pair. In general the aggregrate flexibilty is lower than the weighted sum of household flexibilities
#' \deqn{f_{aggregate} < sum_i^N w_i f_i} weighted by the individual household annual demand.
#'
#' \cr
#' \cr
#' Start year (default 2019) is assumed to be before the beginning smart meter rollout. The old day/night dual-metering system ("tou_old") is used by about 12% of households.
#' \cr
#' \cr
#' The nework input is used to determine the social degree of each agent.
#'
#'
#'
#' @param scen scenario design dataframe e.g. sD
#' @param start_year default 2019
#' @param prices_scen tariff prices dataframe
#' @param social_network social network
#' @param eta eta flex parameter value
#' @param phi \eqn{\phi} parameter choice
#'
#'
#' @returns a dataframe with columns serial ID, annual kWh, initial tariff plan, smart meter install time, and behavioural parameters
#' @export
#' @examples
#' prices_scen <- set_prices(sD)
#' social_network <- make_artificial_society(dep_society_1,homophily,nu=4.5)
#' initialise_agents(sD,2019,prices_scen,social_network,0.6,0.5)
initialise_agents <- function(scen, start_year=2019,prices_scen,social_network,eta=0.6,phi=0.5){

  #agents_in has a minimal set of survey data
  stopifnot(eta %in% flex_scores$eta & phi %in% flex_scores$phi)

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
  tous <- tibble::tibble(q41=c(1,2),tariff_plan=c("flat","tou_old"))
  agents_in <- agents_in %>% dplyr::inner_join(demand,by="serial") %>% dplyr::inner_join(tous,by="q41")
  county_codes <- dep_qanda %>% dplyr::filter(question_code=="qc1") %>% dplyr::rename("qc1"=response_code)
  county_codes <- county_codes %>% dplyr::rename("county"=response) %>% dplyr::select(qc1,county)
  area_codes <- dep_qanda %>% dplyr::filter(question_code=="qg") %>% dplyr::rename("qg"=response_code)
  area_codes <- area_codes %>% dplyr::rename("area"=response) %>% dplyr::select(qg,area)
  area_codes <- area_codes %>% dplyr::mutate(area=dplyr::if_else(qg %in% c(1,3),"Rural","Urban"))
  agents_in <- agents_in %>% dplyr::inner_join(area_codes,by="qg") %>% dplyr::inner_join(county_codes,by="qc1")
  agents_in <- agents_in %>% dplyr::inner_join(smart_meter_rollout,by=c("area","county"))
  #impute missing network degrees


  agents_in <- agents_in %>% dplyr::select(serial,kWh,tariff_plan,rollout,area)
  #combine with structural params
  agents_in <- agents_in %>% dplyr::inner_join(struct_params)
  #rollout year
  #add flex params
  agents_in$eta <- eta
  agents_in$phi <- phi
  #generate "flexibility scores" (hourly MAD load-shifting index) range from min_flex to max_flex%

  flex_scale <- scen %>% dplyr::filter(parameter=="flex_scale.") %>% dplyr::pull(value)
  flex_sigma <- scen %>% dplyr::filter(parameter=="flex_sigma.") %>% dplyr::pull(value)
  #interpret survey flexibilities as log(1h flexibility index) (lognormal distributed)
  #agents_in <- agents_in %>% dplyr::mutate(flex_score= min_flex+max_flex*(flexibility - min(flexibility))/(max(flexibility)-min(flexibility)))

  agents_in <- agents_in %>% dplyr::mutate(flex_score= pmin(70,flex_scale*exp(flex_sigma*flexibility))) #

  #check weighted mean flexibility
  weighted_sum <- agents_in %>% dplyr::mutate(w= kWh/sum(kWh), wflex=w*flex_score) %>% dplyr::pull(wflex) %>% sum()#*sum(agents_in$kWh)
  print(paste("weighted mean sum of household flexibilities", round(weighted_sum,1),"% vs lp1-lp2 flexibility 14.4%"))
  #standardized_z <- agents_in$flexibility/sd(agents_in$flexibility)
  #agents_in$flex_score <- (standardized_z * (15 / 2.576)) + 15
  #agents_in$flex_score <- pmax(1.01*min(score_matrix),agents_in$flex_score)
  #sample flexible parameter values based on flex_score
  score_cube <- flex_score_cube(eta,phi)
  agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(match_flex_params(flex_score,score_cube)) %>% dplyr::ungroup()
  #rescale eta and gamma parameters according to mean hourly demand
  # reduced effect of quadratic
  #agents_in <- agents_in %>% dplyr::mutate(eta=eta*(8760/kWh), gamma=gamma*(8760/kWh))
  #rescale proactive: 0 to theta_max: theta_max very risk intolerant
  theta_max <- scen %>% dplyr::filter(parameter=="theta.") %>% dplyr::pull(value)
  agents_in <- agents_in %>% dplyr::mutate(theta = theta_max*(1-(proactive - min(proactive))/(max(proactive)-min(proactive))))
  #assign natural profile codes : currently only an urban/rural profile
  agents_in <- agents_in %>% dplyr::mutate(natural_profile=dplyr::case_when(area=="Urban"~"lp1",
                                                                    area=="Rural"~"lp3"))
  #actual profile
  agents_in <- agents_in %>% dplyr::mutate(profile=dplyr::case_when(area=="Urban"&tariff_plan=="flat"~"lp1",
                                                                    area=="Urban"&tariff_plan=="tou_old"~"lp2",
                                                                    area=="Rural"&tariff_plan=="flat"~"lp3",
                                                                    area=="Rural"&tariff_plan=="tou_old"~"lp4"))

  agents_in <- agents_in %>% dplyr::select(-flexibility,-proactive,-inertia)
  #current annual bills
  #agents_in <- agents_in %>% dplyr::rowwise() %>% dplyr::mutate(annual_bill=get_annual_cost(2019,kWh,tariff,phi,gamma,eta,tau,"lp1",prices_scen)$annual_bill_flexible)
  get_bill <- function(kWh, tariff_plan, profile) {get_annual_cost_simple(start_year,kWh,tariff_plan,profile,prices_scen)}

  agents_in <- agents_in  %>% dplyr::mutate(annual_bill = purrr::pmap_dbl(list(kWh, tariff_plan, profile), get_bill))
  #tidy up
  agents_in$serial <- as.character(agents_in$serial)
  agents_in$q_dyn <- 0
  #add the network degree
  agents_in <- agents_in %>% dplyr::inner_join(tibble::as_tibble(social_network) %>% dplyr::select(serial,degree),by="serial")
  print(agents_in %>% dplyr::count(tariff_plan) %>% dplyr::mutate(frequency = n / sum(n)) %>% dplyr::select(-n))
  agents_in %>% return()
}


#' update_agents
#'
#' micro-simulation time-step updater\cr
#'\cr
#' The workhorse ABM function.Within a scenario, does a single step (bi-monthly) update of the agent characteristics. A random sample of agents evaluates their economic and social
#' utilities. Agents evaluate the optimal electricity tariff plan. This includes a dynamic tariff.
#' \cr
#'
#'
#' @param scen  scenario dataframe
#' @param yeartime decimal time
#' @param agents_in input agent dataframe
#' @param prices_scen tariff plan price assumptions
#' @param social_network artificial social network
#' @param ignore_social option to ignore social effects. Default is FALSE.
#' @param behavioural_model "prospect" (default) or "classic"
#' @param ignore_theta defaults to TRUE
#' @param quiet TRUE to suppress messages
#'
#' @return updated agent dataframe
#' @export
#' @examples
#'
#' prices_scen <- set_prices(sD)
#' social_network <- make_artificial_society(dep_society_1,homophily,nu=4.5)
#' agents_in <- initialise_agents(sD,2019,prices_scen,social_network)
#'
#' #agents_1 <- update_agents(sD,2026+1/6,agents_in,prices_scen,social_network,behavioural_model="prospect",quiet=FALSE)
#' #agents_2 <- update_agents(sD,2026+2/6,agents_1,prices_scen,social_network,quiet=FALSE)

update_agents <- function(scen,yeartime,agents_in, prices_scen, social_network,ignore_social=F,behavioural_model="prospect",ignore_theta=TRUE,quiet=TRUE){
  #
  #params at yeartime
  params <- scenario_params(scen,yeartime)
  #
  a_s <- agents_in
  n_dynamic <- dim(a_s %>% dplyr::filter(tariff_plan=="dynamic"))[1]
  print(paste("initial number of dynamic tariff plans", n_dynamic))
  #social influence (homogeneous)
  #du_social <- params$nu.*n_dynamic/dim(a_s)[1]
  #assume that any old day/night custeomers are converted to day/night/peak rate when smart meters are installed.
  a_s <- a_s %>% dplyr::mutate(tariff_plan=replace(tariff_plan, (tariff_plan=="tou_old") & (yeartime >= rollout),"tou"))
  a_s <- dplyr::ungroup(a_s)
  #only consider switcher when smart meter are installed
  #random subset of potential switchers
  b_s <- dplyr::slice_sample(a_s,n=roundr(dim(a_s)[1]*params$p.))
  #note ellipsis to handle uncalled columns
  b_s$current_plan <- b_s$tariff_plan
  #
  tariff_plan_bills_env <- function(kWh,phi,gamma,eta,tau,natural_profile,rollout,...) {

    tariff_plan_bills(kWh,phi,gamma,eta,tau,natural_profile,yeartime,rollout,prices_scen)
  }

  if (behavioural_model== "classic") {

  b_s <- b_s %>% dplyr::select(-annual_bill,-tariff_plan)

  b_s_1 <- b_s %>% dplyr::mutate(bills_data = purrr::pmap(dplyr::pick(dplyr::everything()), tariff_plan_bills_env)) %>% tidyr::unnest(bills_data)
  #agent evaluates savings relative to closest non-risky tariff plan
  #agents take avability of tariff plans into account
  #i.e no tou option if yeartime < rollout
  #b_s_1 <- b_s_1 %>% dplyr::filter(!(tariff_plan=="tou" & yeartime < rollout))

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
  #compute social influence
  #assume it saturates beyond
  b_s_2 <- b_s_2 %>% dplyr::mutate(du_social = dplyr::if_else(degree==0,0,params$nu.*pmin(1,q_dyn/degree)))
  b_s_2 <- b_s_2 %>% dplyr::mutate(du_tot = dplyr::if_else(cheapest_plan=="dynamic", du_social-savings-theta,-savings))
  #only adopt if
  b_s_2 <- b_s_2 %>% dplyr::mutate(tariff_plan = dplyr::if_else(du_tot>0 | is.na(du_tot),cheapest_plan,next_plan),
                                   annual_bill=dplyr::if_else(du_tot>0 | is.na(du_tot),cheapest_bill,next_bill))#barrier
  b_s_3 <- b_s_2 %>% dplyr::select(-cheapest_plan,-cheapest_bill,-next_plan,-next_bill,-du_tot,-du_social,-savings)

  } else {

    # decision_rule == "prospect"
    # only flat/tou agents are evaluated this pass -- dynamic agents are left untouched
    # IS THIS REALISTIC?
    # (full retrospective re-evaluation for tou/dynamic households is the deferred piece)
    to_evaluate <- b_s %>% dplyr::filter(current_plan %in% c("flat","tou"))
    unchanged   <- b_s %>% dplyr::filter(!(current_plan %in% c("flat","tou")))

    evaluate_one <- function(kWh,phi,gamma,eta,tau,natural_profile,rollout,current_plan,...) {
      result <- evaluate_tariffs(scen,kWh,phi,gamma,eta,tau,natural_profile,yeartime,rollout,prices_scen)
      # currently on tou: only an upgrade to dynamic is in scope this pass (reversion to
      # flat is the deferred retrospective piece, not decided here)
      new_plan <- if (current_plan=="tou" && result$decision!="dynamic") "tou" else result$decision
      new_bill <- switch(new_plan,
                         flat    = result$costs$flat,
                         tou     = result$costs$tou_flex,
                         dynamic = result$costs$det_flex)
      tibble::tibble(tariff_plan=new_plan, annual_bill=new_bill,
                     CE_tou=result$ce$CE_tou, CE_det=result$ce$CE_det)
    }

    if (nrow(to_evaluate) > 0) {
      to_evaluate <- to_evaluate %>%
        # CHANGED (bug fix): drop any CE_tou/CE_det carried over from a previous timestep's
        # evaluation before this -- tidyr::unnest() errors if the list-column being unnested
        # (eval_data, which also has CE_tou/CE_det) shares names with existing columns.
        # any_of() (not all_of()) is deliberate: these columns won't exist yet on the very
        # first timestep any agent is ever evaluated, and any_of() doesn't error on that.
        dplyr::select(-annual_bill,-tariff_plan,-dplyr::any_of(c("CE_tou","CE_det"))) %>%
        dplyr::mutate(eval_data = purrr::pmap(dplyr::pick(dplyr::everything()), evaluate_one)) %>%
        tidyr::unnest(eval_data) %>%
        dplyr::select(-current_plan)
    } else {
      to_evaluate$CE_tou <- NA_real_
      to_evaluate$CE_det <- NA_real_
      to_evaluate <- to_evaluate %>% dplyr::select(-current_plan)
    }

    unchanged$CE_tou <- NA_real_
    unchanged$CE_det <- NA_real_
    unchanged <- unchanged %>% dplyr::select(-current_plan)

    b_s_3 <- dplyr::bind_rows(to_evaluate, unchanged)
  }


  b_s_3$profile <- "computed"
  #update agents with switchers
  a_s <- dplyr::filter(a_s, !(serial %in% b_s_3$serial))
  a_s <- dplyr::bind_rows(a_s,b_s_3) %>% dplyr::arrange(serial)
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
    print(paste("time", round(yeartime,1), "number of switchers",dim(b_s)[1]))
    }
  a_s <- a_s %>% dplyr::select(-dep_adopter)
  return(dplyr::ungroup(a_s))
}


#' runABM
#'
#' Runs the dynamic electricity pricing adoption simulation on artificial society of ~1217 agents.
#' Each run is performed on an independently generated social network with randomisation from initialise_agents() \cr
#' \cr
#' Bi-monthly timesteps. \cr
#' \cr
#' Good luck.
#'
#' @param scen scenario set-up dataframe, typically read with readr::read_xlxs(...,sheet=scenario)
#' @param Nrun integer, number runs
#' @param simulation_end the final year of simulation of early termination is required
#' @param resample_society if TRUE resample hp_society_oo with replacement to capture additional variability
#' @param n_unused_cores number of cores left unused in parallel/foreach. Recommended values 2 or 1.
#' @param use_parallel if TRUE uses multiple cores. Use FALSE for diagnostic runs on a single core.
#' @param ignore_social if TRUE ignore social network effects. Default is FALSE
#' @param behavioural_model choose "classic" or "prospect" (default)
#' @param quiet if TRUE messaging is reduced
#'
#' @return a three component list - simulation output, scenario setup, meta-parameters
#' @export
#' @importFrom magrittr %>%
#' @importFrom lubridate %m+%
#'
runABM <- function(scen, Nrun=1,simulation_end=2030,resample_society=F,behavioural_model="prospect",n_unused_cores=2, use_parallel=T,ignore_social=F, quiet=TRUE){
  #
  year_zero <- 2019
  #calibration params:: MOVED TO SYSTDATA WHEN CALIBRATION COMPLETE
  p. <- scen %>% dplyr::filter(parameter=="p.") %>% dplyr::pull(value)/10 #inertia
  nu. <- scen %>% dplyr::filter(parameter=="nu.") %>% dplyr::pull(value) #social
  theta. <-  scen %>% dplyr::filter(parameter=="theta.") %>% dplyr::pull(value)
  #
  print(paste("inertia (nu.)=",round(nu.,2),"p.=",round(p.,4),"theta.=",round(theta.,3)))
  #seai_elec <- pvbessmicrosimr::seai_elec
  #bi-monthly runs
  Nt <- round((simulation_end-year_zero+1)*6)
  #single worker (abm run idex j)
  run_single <- function(j,scen,year_zero,Nt,resample_society,ignore_social,behavioural_model,quiet){

    print(paste("Generating price simulation for run",j,"...."))
    prices_scen <- set_prices(scen)
    #
    print(paste("Generating social network for run",j,"...."))
    if(!resample_society) social <- make_artificial_society(dep_society_1,homophily,4.5)

    if(resample_society){
      agent_resample <- sample(1:dim(dep_society_1)[1],replace=T)
      society_new <- dep_society_1[agent_resample,]
      society_new$ID <- 1:dim(dep_society_1)[1]
      social <- make_artificial_society(society_new,homophily,4.5)
    }
    print(paste("initialising agents.."))
    agents_in <- initialise_agents(scen,year_zero,prices_scen,social)
    #no transactions
    #agents_in$transaction <- FALSE
    agent_ts <- vector("list",Nt)
    agent_ts[[1]] <- agents_in #agent parameters with regularized weights

    for(t in seq(2,Nt)){
      #
      #yeartime <- year_zero+(t-1)
      yeartime <- year_zero+(t-1)/6
      agent_ts[[t]] <- update_agents(scen,yeartime,agent_ts[[t-1]],prices_scen, social_network=social,ignore_social,behavioural_model,quiet) #static socal network, everything else static
      #agent_ts[[t]] <- tibble::tibble(t=t)
    }

    for(t in 1:Nt) agent_ts[[t]]$t <- t
    #agent_ts <- tibble::as_tibble(data.table::rbindlist(agent_ts,fill=T))
    agent_ts <- purrr::list_rbind(agent_ts)
    agent_ts$simulation <- j
    return(agent_ts)
  }

  if(use_parallel & .Platform$OS.type == "windows"){

      number_of_cores <- parallel::detectCores() - n_unused_cores
      cl <- parallel::makeCluster(number_of_cores)
      on.exit(parallel::stopCluster(cl), add = TRUE)
      #exports
      parallel::clusterEvalQ(cl, library(depmicrosimr))
      abm <- parallel::parLapply(cl, 1:Nrun, run_single,scen = scen,
                                 year_zero = year_zero,
                                 Nt = Nt,
                                 resample_society = resample_society,
                                 behavioural_model=behavioural_model,
                                 ignore_social = ignore_social,
                                 quiet = quiet)
      parallel::stopCluster(cl)
    }
  #should run seamlessly on linux or Mac
    if(use_parallel & .Platform$OS.type != "windows"){
      number_of_cores <- parallel::detectCores() - n_unused_cores
      abm <- parallel::mclapply(1:Nrun, run_single,scen = scen,
                                year_zero = year_zero,
                                Nt = Nt,
                                resample_society = resample_society,
                                ignore_social = ignore_social,
                                quiet = quiet,
                                behavioural_model=behavioural_model,
                                mc.cores=number_of_cores)
    }


    #don't use parallel
    if(!use_parallel) abm <- lapply(1:Nrun,run_single,scen = scen,
                                    year_zero = year_zero,
                                    Nt = Nt,
                                    resample_society = resample_society,
                                    ignore_social = ignore_social,
                                    behavioural_model=behavioural_model,
                                    quiet = quiet)


    closeAllConnections()
    #meta <- tibble::tibble(parameter=c("Nrun","end_year","beta.","lambda.","p."),value=c(Nrun,simulation_end,beta,lambda,p))
    meta <- tibble::tibble(parameter=c("Nrun","end_year","p.","nu.","theta.","model"),value=c(Nrun,simulation_end,p.,nu.,theta.,behavioural_model))
    #replace "t" with dates
    abm <- abm %>% purrr::list_rbind()
    abm <- abm %>% dplyr::mutate(date=lubridate::ymd(paste(year_zero,"-01-01",sep="")) %m+% months((t-1)*2)) %>% dplyr::arrange(simulation,date) %>% dplyr::select(-t)
    return(list("abm"=abm,"scenario"=scen,"system"=meta))
  }


