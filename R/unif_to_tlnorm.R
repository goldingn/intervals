#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param u
#' @param meanlog
#' @param sdlog
#' @param upper
#' @return
#' @author Nick Golding
#' @export
# greta op to model truncated lognormals with varying upper bounds
unif_to_tlnorm <- function(x, meanlog, sdlog, upper) {
  op("uniform_to_tlnorm",
     x,
     meanlog = meanlog,
     sdlog = sdlog,
     upper = upper,
     tf_operation = "tf_unif_to_tlnorm"
  )
}

op <- greta::.internals$nodes$constructors$op
