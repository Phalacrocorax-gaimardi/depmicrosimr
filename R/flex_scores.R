#' @title flex_scores
#' @description Table of standard flexibility scores, defined as MAE loadshift as a percentage of mean load. The relationship between flexibility scores and parameters are modelled using 2026 ToU and Dynamic prices.
#' Parameters correspond to a 8760kWh annual load and LP1 reference profile.\cr
#' \cr
#' Hourly, daily and weekly scores are provided.
#' @format A data frame with 2970 rows and 9 variables:
#' \describe{
#'   \item{\code{tariff_plan}}{character dynamic or ToU}
#'   \item{\code{profile}}{character LP1}
#'   \item{\code{phi}}{double inflexible fraction}
#'   \item{\code{gamma}}{double cost penalty parameter}
#'   \item{\code{eta}}{double kinetic penalty parameter}
#'   \item{\code{tau}}{double load reversion timescale}
#'   \item{\code{flex_hour}}{double hourly score}
#'   \item{\code{flex_day}}{double daily score}
#'   \item{\code{flex_week}}{double daily score}
#'}
#' @details The output of get_flex_scores(), see diagnostic.R.
"flex_scores"
