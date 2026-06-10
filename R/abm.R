##############################################################
# ABM consists of an initialiser, an update and a run module
###########################################################

#' initialise_agents
#'
#' sets the initial state variables at the beginning of each run (Jan 2019 before smart-meter rollout)\cr
#' \cr
#' 14% of households are on the old (dual meter) day/night tariff.
#'
#'
#'
#' @param sD scenario design dataframe
#' @param start_year default 2019
#'
#' @returns a dataframe with columns serial ID, annual kWh, initial tariff plan, smart meter install time, and behavioural parameters
#' @export
#' @examples
#' initialise_agents(sD,2019)
initialise_agents <- function(sD, start_year=2019){

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
  tous <- tibble::tibble(q41=c(1,2),tariff=c("flat","day/night/peak"))
  agents_in <- agents_in %>% dplyr::inner_join(demand) %>% dplyr::inner_join(tous)
  county_codes <- dep_qanda %>% dplyr::filter(question_code=="qc1") %>% dplyr::rename("qc1"=response_code)
  county_codes <- county_codes %>% dplyr::rename("county"=response) %>% dplyr::select(qc1,county)
  area_codes <- dep_qanda %>% dplyr::filter(question_code=="qg") %>% dplyr::rename("qg"=response_code)
  area_codes <- area_codes %>% dplyr::rename("area"=response) %>% dplyr::select(qg,area)
  area_codes <- area_codes %>% dplyr::mutate(area=dplyr::if_else(qg %in% c(1,3),"Rural","Urban"))
  agents_in <- agents_in %>% dplyr::inner_join(area_codes) %>% dplyr::inner_join(county_codes)
  agents_in <- agents_in %>% dplyr::inner_join(smart_meter_rollout)
  #
  agents_in <- agents_in %>% dplyr::select(serial,kWh,tariff,yeartime)
  #combine with structural params
  agents_in <- agents_in %>% dplyr::inner_join(struct_params)
  #rollout year
  agents_in %>% return()
}

