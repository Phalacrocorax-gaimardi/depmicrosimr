#' hmm_fit
#'
#' A pre-fitted 3-state gaussian hidden markov model trained on SEM wholesale log price residuals
#' over the six year period 2019-2026. This model is used to generate simulations of future electricity price fluctuations.
#'
#' @format A \code{depmix.fitted} object with 3 hidden states.
#' \describe{
#'   \item{transition}{Transition probabilities between states}
#'   \item{response}{Response models for each state}
#' }
#' @source
#' @details the output of the function generate_logprice_hmm(sem_logprices_2019_2023,n_states=3,winsor=0.01)
"hmm_fit"
