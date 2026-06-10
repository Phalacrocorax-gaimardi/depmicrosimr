#' @title smart_meter_rollout
#' @description estimated median time of smart meter rollout by area type and county. Used by initialise_agents().
#' @format A data frame with 52 rows and 3 variables:
#' \describe{
#'   \item{\code{county}}{character county}
#'   \item{\code{area}}{character urban or rural}
#'   \item{\code{yeartime}}{double decimal time}
#'}
#' @details compiled using gemini AI
"smart_meter_rollout"
