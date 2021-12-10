#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @param meanlog
#' @param sdlog
#' @param upper
#' @return
#' @author Nick Golding
#' @export
# tensorflow fucntion for mapping from a uniform variat toa  truncated lognormal
tf_unif_to_tlnorm <- function(x, meanlog, sdlog, upper) {
  dist <- greta:::tfp$distributions$Normal(meanlog, sdlog)
  exp(dist$quantile(x * dist$cdf(log(upper))))
}