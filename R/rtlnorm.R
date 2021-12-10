#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n
#' @param gi_meanlog
#' @param gi_sdlog
#' @param upper
#' @return
#' @author Nick Golding
#' @export
# simulate multiple truncated lognormal with diffeent truncations. This is a
# dreadful solution, but we need vectorised truncation bounds
rtlnorm <- function(n, meanlog, sdlog, upper) {
  
  args <- data.frame(
    idx = seq_len(n),
    meanlog = meanlog,
    sdlog = sdlog,
    upper = upper
  )
  res <- rep(NA, n)
  
  for (i in args$idx) {
    res[i] <- rtlnorm_once(
      meanlog = args$meanlog[i],
      sdlog = args$sdlog[i],
      upper = args$upper[i])
  }
  
  res  
  
}