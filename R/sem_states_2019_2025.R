#' @title sem_states_2019_2025
#' @description states implied by hmm_fit over the historical period
#' @format A data frame with 61368 rows and 2 variables:
#' \describe{
#'   \item{\code{datetime}}{double hourly datetimes}
#'   \item{\code{hmm_state}}{integer state labels 1-3 (1 corresponds to lowest prices)}
#'}
#' @details prodouced by HiddenMarkov::Viterbi(hmm_fit)
"sem_states_2019_2025"
