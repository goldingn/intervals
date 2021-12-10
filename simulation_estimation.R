# Simulation estimation study of a Bayesian model to infer generation interval
# distributions etc.

# load packages and functions
source("packages.R")
. <- list.files("R", full.names = TRUE) %>%
  lapply(source)

# simulatge 'true' data
true_gi <- list(meanlog = 1.2, sdlog = 0.4)
hist(rlnorm(1e5, true_gi$meanlog, true_gi$sdlog), breaks = 100)
n <- 1000
truth <- sim_truth(n, true_gi$meanlog, true_gi$sdlog)

# don't observe all dates for all individuals
observations <- sim_observations(truth)

# incubation period distribution parameters (assume these are known, estimated
# in non-household infections)
incubation_meanlog <- 1.6
incubation_sdlog <- 0.3

# model the unknown dates

# non-truncated generation interval distribution parameters (infectiousness) use
# nishiura posteriors as priors (needs informative priors to start it in the
# right ballpark)
gi_meanlog <- normal(1.38, 0.113, truncation = c(0, Inf))
gi_sdlog <- normal(0.567, 0.0858, truncation = c(0, Inf))

# observation error to reflect potential for miscoding of dates (sd of
# interval-censored normal)
obs_sd_symptom_onset <- normal(0, 0.1, truncation = c(0, Inf))
obs_sd_infection <- normal(0, 0.1, truncation = c(0, Inf))
obs_sd_isolation <- normal(0, 0.1, truncation = c(0, Inf))

# latent case infection date (all other dates defined from this via delays)
case_infection  <- variable(dim = n)

# time to isolation
case_infection_to_isolation  <- lognormal(1, 0.5, dim = n)
case_isolation <- case_infection + case_infection_to_isolation 

# define generation interval - contrained to not extend beyond the date of
# isolation
generation_interval <- right_censored_lognormal(
  meanlog = gi_meanlog,
  sdlog = gi_sdlog, 
  upper = case_infection_to_isolation,
  dim = n
)

# compute contact infection date
contact_infection <- case_infection + generation_interval

# compute incubation periods for cases and contacts
case_incubation <- lognormal(incubation_meanlog, incubation_sdlog, dim = n)
contact_incubation <- lognormal(incubation_meanlog, incubation_sdlog, dim = n)

# define symptoms onset dates for these
case_symptom_onset <- case_infection + case_incubation
contact_symptom_onset <- contact_infection + contact_incubation

# define likelihoods for all observed dates
prob_case_infection <- define_date_likelihood(
  observations$case_infection_date,
  case_infection,
  sd = obs_sd_infection
)
prob_case_isolation <- define_date_likelihood(
  observations$case_isolation_date,
  case_isolation,
  sd = obs_sd_isolation
)
prob_case_symptom_onset <- define_date_likelihood(
  observations$case_symptom_onset_date,
  case_symptom_onset,
  sd = obs_sd_symptom_onset
)
prob_contact_infection <- define_date_likelihood(
  observations$contact_infection_date,
  contact_infection,
  sd = obs_sd_infection
)
prob_contact_symptom_onset <- define_date_likelihood(
  observations$contact_symptom_onset_date,
  contact_symptom_onset,
  sd = obs_sd_symptom_onset
)

m <- model(gi_meanlog, gi_sdlog, obs_sd_infection, obs_sd_isolation, obs_sd_symptom_onset)

n_chains <- 4
inits <- replicate(
  n_chains,
  impute_initials(
    observations,
    incubation_meanlog = incubation_meanlog,
    incubation_sdlog = incubation_sdlog
  ),
  simplify = FALSE
)

draws <- mcmc(m, chains = n_chains, initial_values = inits) 

coda::gelman.diag(draws, autoburnin = FALSE, multivariate = FALSE)
plot(draws)
summary(draws)
true_gi

indices <- 1:8
pred_draws <- calculate(contact_infection[indices], values = draws)
plot(pred_draws)

cbind(
  truth$contact_infection_date[indices],
  observations$contact_infection_date[indices],
  summary(pred_draws)$statistics[, "Mean"]
)


si <- contact_symptom_onset - case_symptom_onset
si_sims <- calculate(si, values = draws, nsim = 1000)
mean(si_sims[[1]])
sd(si_sims[[1]])

hist(si_sims[[1]])

tost <- contact_infection - case_symptom_onset
tost_sims <- calculate(tost, values = draws, nsim = 1000)
mean(tost_sims[[1]])
sd(tost_sims[[1]])
hist(tost_sims[[1]])


# naive estimates
naive_data <- observations %>%
  mutate(
    across(
      c(case_infection_date,
        contact_infection_date,
        case_isolation_date),
      ~.x + runif(n())
    ),
    case_infection_date = pmin(case_infection_date, case_isolation_date - 2e-3),
    contact_infection_date = pmin(contact_infection_date, case_isolation_date - 1e-3),
    contact_infection_date = pmax(contact_infection_date, case_infection_date + 1e-3),
    gi = contact_infection_date - case_infection_date,
    time_to_isolation = case_isolation_date - case_infection_date
  ) %>%
  filter(
    is.finite(gi) & is.finite(time_to_isolation)
  ) %>%
  dplyr::select(
    gi,
    time_to_isolation
  )

par(mfrow = c(1, 1))
hist(naive_data$gi, breaks = 100)

# naive estimate - non-truncated
naive_data %>%
  summarise(
    meanlog = mean(log(gi)),
    sdlog = sd(log(gi))
  )

# naive estimate - truncated, posterior density
gi_meanlog_naive <- normal(1.38, 0.113, truncation = c(0, Inf))
gi_sdlog_naive <- normal(0.567, 0.0858, truncation = c(0, Inf))
true_gi


distribution(naive_data$gi) <- lognormal(gi_meanlog_naive, gi_sdlog_naive, truncation = c(0, naive_data$time_to_isolation))

m2 <- model(gi_meanlog_naive, gi_sdlog_naive)
draws2 <- mcmc(m2)
coda::gelman.diag(draws2, autoburnin = FALSE, transform = FALSE)
plot(draws2)
summary(draws2)
dim(naive_data)

