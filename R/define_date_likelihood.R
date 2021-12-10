#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @param case_infection
#' @param sd
#' @return
#' @author Nick Golding
#' @export
# define the interval-censored date likelihood for one date vector, skipping
# missing observations
define_date_likelihood <- function(observed, predicted, sd) {
  keep <- !is.na(observed)
  interval_likelihood(observed[keep], predicted[keep], sd = sd)
}
