#' @title struct_params
#' @description heterogeneous parameters describing agents preferences for dynamic electricity pricing based on
#' proactive attitude, perceived flexibility and inertia latent characteristics inferred from Structural Equation Modelling.\cr
#' \cr
#' The flexibility values are related to the heterogeneous \eqn{\gamma} parameter in quadratic cost penalty model. The proactive
#' values are reflected in the heterogeneous threshold \eqn{theta} in the ABM.
#'
#' @format A data frame with 1225 rows and 4 variables:
#' \describe{
#'   \item{\code{serial}}{double HH serial ID}
#'   \item{\code{inertia}}{double inertia parameter}
#'   \item{\code{proactive}}{double COLUMN_DESCRIPTION}
#'   \item{\code{flexibility}}{double COLUMN_DESCRIPTION}
#'}
#' @details structural equation modelling from Kumar et al (2026)
"struct_params"
