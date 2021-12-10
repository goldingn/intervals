#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param nameme1
#' @param nameme2
#' @param sd
#' @return
#' @author Nick Golding
#' @export
# interval-censored normal likelihood with the ones trick (assume integer dates
# are the floor value of continuous)
interval_likelihood <- function(observed_date_lower, expected_date, observed_date_upper = observed_date_lower + 1, sd) {
  
  dummy_obs <- ones(length(observed_date_upper))
  
  upper <- (observed_date_upper - expected_date) / sd
  lower <- (observed_date_lower - expected_date) / sd
  prob <- iprobit(upper) - iprobit(lower)
  
  distribution(dummy_obs) <- bernoulli(prob)
  
  invisible(prob)
  
}
