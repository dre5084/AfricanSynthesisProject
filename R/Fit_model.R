devtools::install_github("nicholasjclark/mvgam")

library(mvgam)

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


data_subset <-
  data_roc_rescaled_full %>%
  dplyr::filter(series %in% unique(data_roc_rescaled_full$series)[1:3]) %>%
  dplyr::mutate(
    series = as.character(series),
    series = as.factor(series)
  )

data_priors <-
  get_mvgam_priors(
    formula = ROC_mean ~ series,
    trend_formula = ~
      0 +
        gp(time, k = 32) +
        gp(time, by = trend, k = 20),
    trend_model = mvgam::AR(cor = TRUE),
    data = data_roc_rescaled_full,
    family = Gamma()
  )

as.data.frame(data_priors) %>%
  tibble::as_tibble() %>%
  View()

hg_model <-
  mvgam::mvgam(
    formula = ROC_mean ~ series,
    trend_formula = ~
      0 +
        gp(time, k = 32) +
        gp(time, by = trend, k = 20),
    trend_model = mvgam::AR(cor = TRUE),
    data = data_subset,
    family = Gamma(),
    share_obs_params = TRUE,
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 15
    ),
    silent = 1,
    chains = 10,
    burnin = 750,
    samples = 2000,
    thin = 5,
    parallel = TRUE
  )


RUtilpol::save_latest_file(
  hg_model,
  file_name = "hg_model",
  dir = here::here("Data/Processed/")
)
