#' @title degrees
#' @description the frequency distribution of influence network degree
#' @format A data frame with 20 rows and 3 variables:
#' \describe{
#'   \item{\code{degree}}{integer number of associates}
#'   \item{\code{n}}{integer survey count, excluding NAs}
#'   \item{\code{f}}{double survey frequency, excluding NAs and zeroes}
#'}
#' @details includes non-zero degree only
"degrees"
