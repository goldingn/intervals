#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param truth
#' @return
#' @author Nick Golding
#' @export
# given 'true' data, simulate observations
sim_observations <- function(truth, probs = c(0.4, 0.2, 0.4)) {
  
  truth %>%
    mutate(
      case_which = sample(
        c("infection", "onset", "both"),
        size = n(),
        replace = TRUE,
        prob = probs
      ),
      contact_which = sample(
        c("infection", "onset", "both"),
        size = n(),
        replace = TRUE,
        prob = probs
      ),
      case_symptom_onset_date = if_else(
        case_which %in% c("onset", "both"),
        case_symptom_onset_date,
        NA_real_
      ),
      case_infection_date = if_else(
        case_which %in% c("infection", "both"),
        case_infection_date,
        NA_real_
      ),
      contact_symptom_onset_date = if_else(
        contact_which %in% c("onset", "both"),
        contact_symptom_onset_date,
        NA_real_
      ),
      contact_infection_date = if_else(
        contact_which %in% c("infection", "both"),
        contact_infection_date,
        NA_real_
      )
    ) %>%
    select(
      -case_incubation_period,
      -contact_incubation_period,
      -case_time_to_isolation,
      -generation_interval,
      -case_which,
      -contact_which
    ) %>%
    mutate(
      across(
        everything(),
        floor
      )
    )
}