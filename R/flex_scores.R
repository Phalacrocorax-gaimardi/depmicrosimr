#' @title flex_scores
#' @description table of flexibility scores (% MAD loadshifting) with tau=48 and eta = 0.5 (scaled to 8760kWh annual load).
#' These are calculated with 2026 dynamic price assumptions.
#' @format A data frame with 126 rows and 3 variables:
#' \describe{
#'   \item{\code{phi}}{double baseload}
#'   \item{\code{gamma}}{double cost penalty parameter}
#'   \item{\code{flex_score}}{double % score in range 0-31%}
#'}
#' @details see scratch.R
"flex_scores"
