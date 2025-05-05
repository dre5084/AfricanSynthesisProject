devtools::install_github("nicholasjclark/mvgam")

library(mvgam)
library(tidyverse)
library(RUtilpol)

data_roc_rescaled <-
  RUtilpol::get_latest_file(
    file_name = "data_roc_rescaled",
    dir = here::here("Data/Processed/")
  ) %>%
  dplyr::mutate(
    series = as.factor(dataset_id)
  )

data_roc_rescaled_full <-
  data_roc_rescaled %>%
  dplyr::select(series, time, ROC_mean) %>%
  dplyr::full_join(
    tidyr::expand_grid(
      series = unique(data_roc_rescaled$series),
      time = seq(1, 45, by = 1)
    ),
    by = dplyr::join_by(series, time)
  ) %>%
  dplyr::arrange(series, time) %>%
  dplyr::mutate(
    time = as.integer(time)
  )

data_priors <-
  get_mvgam_priors(
    formula = ROC_mean ~ series,
    trend_formula = ~
      0 +
        gp(time, k = 5, gr = FALSE) +
        gp(time, by = trend, k = 10, gr = FALSE),
    trend_model = mvgam::AR(cor = TRUE),
    data = data_roc_rescaled_full,
    family = Gamma()
  )

hg_model <-
  mvgam::mvgam(
    data = data_roc_rescaled_full,
    formula = ROC_mean ~ series,
    trend_formula = ~
      0 +
        gp(time, k = 5, gr = FALSE) +
        gp(time, by = trend, k = 10, gr = FALSE),
    trend_model = mvgam::AR(cor = TRUE),
    family = Gamma(),
    share_obs_params = TRUE,
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 15
    ),
    priors = data_priors,
    silent = 1,
    chains = parallelly::availableCores(logical = FALSE) - 1,
    burnin = 1000,
    samples = 5000,
    thin = 5,
    parallel = TRUE,
    backend = "cmdstanr"
  )


RUtilpol::save_latest_file(
  hg_model,
  file_name = "hg_model",
  dir = here::here("Data/Processed/")
)
