#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param n
#' @param nameme1
#' @param nameme2
#' @return
#' @author Nick Golding
#' @export
# simulate 'true' data
sim_truth <- function(n = 100, gi_meanlog = 1.4, gi_sdlog = 0.5) {
  tibble(
    # simulate case infection date, and delays
    case_infection_date = runif(n, 0, 10),
    case_incubation_period = rlnorm(n, 1, 0.5),
    case_time_to_isolation = rlnorm(n, 1.2, 0.5),
    contact_incubation_period = rlnorm(n, 1, 0.5)
  ) %>%
    # simulate truncated generation interval delay, based on isolation delay
    mutate(
      generation_interval = rtlnorm(n, gi_meanlog, gi_sdlog, upper = case_time_to_isolation)
    )  %>%
    # compute remaining dates
    mutate(
      case_symptom_onset_date = case_infection_date + case_incubation_period,
      case_isolation_date = case_infection_date + case_time_to_isolation,
      contact_infection_date = case_infection_date + generation_interval,
      contact_symptom_onset_date = contact_infection_date + contact_incubation_period,
    )
}

