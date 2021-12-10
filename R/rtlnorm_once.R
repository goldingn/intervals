#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param meanlog
#' @param sdlog
#' @param upper
#' @return
#' @author Nick Golding
#' @export
# simulatone draw from a truncated lognormal
rtlnorm_once <- function(meanlog, sdlog, upper) {
  truncdist::rtrunc(
    n = 1,
    spec = "lnorm",
    a = 0,
    b = upper,
    meanlog = meanlog,
    sdlog = sdlog
  )
}