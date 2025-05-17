# install.packages("remotes")
# remotes::install_github("paul-buerkner/brms")
# remotes::install_github("nicholasjclark/mvgam")

library(here)
library(parallelly)
library(brms)
library(mvgam)
library(tidyverse)
library(RUtilpol)

verbose <- FALSE
subset_data_to_test_model <- FALSE

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "data_to_fit_east_afrika",
    dir = here::here("Data/Processed/Models/Data_to_fit")
  )

if (
  isTRUE(subset_data_to_test_model)
) {
  n_series <- 10

  data_series_length <-
    data_to_fit %>%
    tidyr::drop_na() %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(
      n = n()
    ) %>%
    dplyr::arrange(dplyr::desc(n))

  data_to_fit <-
    data_to_fit %>%
    dplyr::filter(
      series %in% c(
        data_series_length %>%
          dplyr::slice_head(n = round(n_series / 2)) %>%
          dplyr::pull(series),
        data_series_length %>%
          dplyr::slice_tail(n = round(n_series / 2)) %>%
          dplyr::pull(series)
      )
    ) %>%
    dplyr::mutate(
      series = as.character(series) %>%
        as.factor()
    )
}

if (
  isTRUE(verbose)
) {
  mvgam::plot_mvgam_series(
    data = data_to_fit,
    y = "ROC_mean",
    series = "all"
  )

  ggplot2::ggplot(
    data = data_to_fit,
    mapping = ggplot2::aes(x = ROC_mean)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::labs(
      y = "Frequency",
      x = "ROC_mean"
    )

  summary(data_to_fit)
}

sel_formula <-
  as.formula(ROC_mean ~ series)

n_series <- length(unique(data_to_fit$series))

sel_k_general <- 10
sel_k_series <- 15

sel_trend_formula <-
  paste(
    " ~ 0 +
      # Shared smooth of year for all series
      gp(age,
        k = ", sel_k_general, "
      ) +

      # Deviation smooths for each series
      gp(age,
        by = trend,
        k = ", sel_k_series, "
      )"
  ) %>%
  as.formula()

sel_trend_model <- mvgam::AR()

sel_family <- stats::Gamma(link = "log")

base_priors <-
  mvgam::get_mvgam_priors(
    formula = sel_formula,
    trend_formula = sel_trend_formula,
    trend_model = sel_trend_model,
    use_lv = TRUE,
    data = data_to_fit,
    family = sel_family
  )

if (
  isTRUE(verbose)
) {
  View(as.data.frame(base_priors))
}

data_priors <-
  base_priors %>%
  dplyr::mutate(
    prior = dplyr::case_when(
      .default = prior,

      # Intercept
      param_name == "(Intercept)" ~
        "(Intercept) ~ normal(0, 0.1);",

      # GP marginal SDs (alpha)
      str_detect(param_name, "^real<lower=0> alpha_gp_trend\\(age\\);$") ~
        "alpha_gp_trend(age) ~ normal(0, 0.1);",
      str_detect(param_name, "^real<lower=0> alpha_gp_trend\\(age\\):trend") ~
        paste0(
          str_extract(param_name, "alpha_gp_trend\\(age\\):trend[^;]*"),
          " ~ normal(0, 0.1);"
        ),

      # GP length-scales (rho)
      str_detect(param_name, "^real<lower=0> rho_gp_trend\\(age\\);$") ~
        "rho_gp_trend(age)[1] ~ normal(0, 0.1);",
      str_detect(param_name, "^real<lower=0> rho_gp_trend\\(age\\):trend") ~
        paste0(
          str_remove(
            str_extract(param_name, "rho_gp_trend\\(age\\):trend[^;]*"),
            "\\[1\\]"
          ),
          "[1] ~ normal(0, 0.1);"
        ),

      # Latent AR(1)
      str_detect(param_name, "^vector<lower=-1,upper=1>\\[n_lv\\] ar1;$") ~
        "ar1 ~ normal(0.25, 0.1);",

      # Process SDs
      str_detect(param_name, "^vector<lower=0>\\[n_lv\\] sigma;$") ~
        "sigma ~ exponential(1);", # beta(3, 10)

      # Gamma shape
      str_detect(param_name, "^vector<lower=0>\\[n_series\\] shape;$") ~
        "shape ~ gamma(0.05, 0.05);",

      # Series effects (e.g. "series41298")
      str_detect(param_name, "^series[0-9]+$") ~
        paste0(param_name, " ~ normal(0, 0.1);")
    )
  )

if (
  isTRUE(verbose)
) {
  View(as.data.frame(data_priors))
}

set.seed(1234)
mod_mvgam_gp <-
  mvgam::mvgam(
    data = data_to_fit,
    formula = sel_formula,
    trend_formula = sel_trend_formula,
    trend_model = sel_trend_model,
    family = sel_family,
    use_lv = TRUE,
    share_obs_params = TRUE,
    noncentred = TRUE,
    priors = data_priors,
    control = list(
      adapt_delta = 0.95,
      max_treedepth = 15,
      init = 0
    ),
    chains = 12,
    # this is set to very small numbers to make fit the model quickly
    samples = 100,
    burnin = 200,
    thin = 5,
    silent = 1,
    backend = "cmdstanr",
    parallel = FALSE
  )

RUtilpol::save_latest_file(
  mod_mvgam_gp,
  file_name = "mod_mvgam_gp",
  dir = here::here("Data/Processed/Models/East_afrika/")
)
