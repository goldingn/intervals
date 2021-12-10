#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meanlog
#' @param sdlog
#' @param upper
#' @param dim
#' @return
#' @author Nick Golding
#' @export
# define a right-censored lognormal random variable, with variable parameters
# and upper bound
right_censored_lognormal <- function(meanlog, sdlog, upper, dim) {
  u <- uniform(0, 1, dim = n)
  unif_to_tlnorm(
    u,
    meanlog = meanlog,
    sdlog = sdlog,
    upper = upper
  )
}

