#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param observations
#' @param incubation_meanlog
#' @param incubation_sdlog
#' @return
#' @author Nick Golding
#' @export
# do imputation on the data to provide initial values
impute_initials <- function(observations, incubation_meanlog, incubation_sdlog) {
  
  observations %>%
    # jitter everything within the integer day
    mutate(
      across(
        everything(),
        ~.x + runif(n())
      )
    ) %>%
    # then make sure they are in the right order
    mutate(
      # case infection before isolation
      case_infection_date = case_when(
        !is.na(case_infection_date) ~ pmin(case_infection_date, case_isolation_date - 0.2),
        TRUE ~ case_infection_date
      ),
      # case onset after case infection
      case_symptom_onset_date = case_when(
        !is.na(case_symptom_onset_date) & !is.na(case_infection_date) ~ pmax(case_symptom_onset_date, case_infection_date + 0.1),
        TRUE ~ case_symptom_onset_date
      ),
      # contact infection after case infection
      contact_infection_date = case_when(
        !is.na(contact_infection_date) & !is.na(case_infection_date) ~ pmax(contact_infection_date, case_infection_date + 0.1),
        TRUE ~ contact_infection_date
      ),
      # contact infection before case isolation
      contact_infection_date = case_when(
        !is.na(contact_infection_date) ~ pmin(contact_infection_date, case_isolation_date - 0.1),
        TRUE ~ contact_infection_date
      ),
      # contact onset after contact infection
      contact_symptom_onset_date = case_when(
        !is.na(contact_symptom_onset_date) & !is.na(contact_infection_date) ~ pmax(contact_symptom_onset_date, contact_infection_date + 0.1),
        TRUE ~ contact_symptom_onset_date
      )
    ) %>%
    mutate(
      case_incubation_period = rlnorm(n(), incubation_meanlog, incubation_sdlog),
      contact_incubation_period = rlnorm(n(), incubation_meanlog, incubation_sdlog)
    ) %>%
    mutate(
      case_infection_date = case_when(
        is.na(case_infection_date) ~ pmin(case_symptom_onset_date - case_incubation_period,
                                          case_isolation_date - 0.1,
                                          contact_infection_date - 0.1,
                                          contact_symptom_onset_date - 0.2,
                                          na.rm = TRUE),
        TRUE ~ case_infection_date
      ),
      case_symptom_onset_date = case_when(
        is.na(case_symptom_onset_date) ~ case_infection_date + case_incubation_period,
        TRUE ~ case_symptom_onset_date
      ),
      contact_symptom_onset_date = case_when(
        is.na(contact_symptom_onset_date) ~ contact_infection_date + contact_incubation_period,
        TRUE ~ contact_symptom_onset_date
      )
    ) %>%
    mutate(
      contact_infection_date = case_when(
        is.na(contact_infection_date) ~ 
          runif(
            n,
            case_infection_date,
            pmin(contact_symptom_onset_date, case_isolation_date)
          ),
        TRUE ~ contact_infection_date
      )
    ) %>%
    mutate(
      case_incubation = case_symptom_onset_date - case_infection_date,
      contact_incubation = contact_symptom_onset_date - contact_infection_date,
      case_infection_to_isolation = case_isolation_date - case_infection_date
    ) %>%
    select(
      case_infection = case_infection_date,
      case_infection_to_isolation,
      case_incubation,
      contact_incubation
    ) %>%
    as.list() %>%
    c(
      list(
        # initually broad observation error, to help it burn in
        obs_sd_infection = runif(1, 1, 1.5),
        obs_sd_symptom_onset = runif(1, 1, 1.5),
        obs_sd_isolation = runif(1, 1, 1.5),
        # fix the gi parameters at the prior mean since it is sensitive to
        # these
        gi_meanlog = 1.38,
        gi_sdlog = 0.567
      )
    ) %>%
    do.call(initials, .)
}
