#' @title diurnal_inflex
#' @description the relative variation of baseload share during the day. The overal share of inflexibility load is \eqn{phi}. The flexible share of the load during the day is \eqn{(1-\phi f_h)}load.  normalise so that
#' \eqn{\sum_h f_h = 24}
#' @format A data frame with 24 rows and 2 variables:
#' \describe{
#'   \item{\code{hour}}{double 0-23}
#'   \item{\code{f}}{double inflexibility weight}
#'}
#' @details based on 5% quantiles of hourly profiles of 600 households with no solar PV.
"diurnal_inflex"
