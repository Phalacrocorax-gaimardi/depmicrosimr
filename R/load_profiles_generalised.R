#' @title load_profiles_generalised
#' @description 2026 load profiles replicated for the year 2019-2040\cr
#' \cr
#' The generalisation includes leap years. Hourly profiles for 29th Feb taken to be the mean of \eqn{\pm 7 \times 24} hour values.\cr
#' \cr
#' \code{load_profiles_generalised} also gives the hourly ToU price bands adjusted for Daylight Savings Time (DST) between 2019 and 20240.
#'
#' @format A data frame with 192720 rows and 6 variables:
#' \describe{
#'   \item{\code{datetime}}{double UTC date time.}
#'   \item{\code{lp1}}{double urban aggregate load profile}
#'   \item{\code{lp2}}{double urban ToU aggregate load profile}
#'   \item{\code{lp3}}{double rural aggregate load profile}
#'   \item{\code{lp4}}{double rural ToU aggregate load profile}
#'   \item{\code{tou_band}}{character ToU band}
#'}
#' @details see \code{generalise_load_profiles()} in scratch.R
"load_profiles_generalised"
