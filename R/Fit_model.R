# install.packages("remotes")
# remotes::install_github("paul-buerkner/brms")
# remotes::install_github("nicholasjclark/mvgam")

library(here)
library(parallelly)
library(brms)
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

base_priors <-
  get_mvgam_priors(
    formula = ROC_mean ~ series,
    trend_formula = ~ 0 +
      # Shared smooth of year for all series
      s(time,
        k = 10,
        bs = "cr"
      ) +

      # Deviation smooths for each series
      s(time,
        by = trend,
        k = 15,
        bs = c("cr", "sz")
      ),
    trend_model = mvgam::AR(cor = TRUE),
    use_lv = TRUE,
    data = data_roc_rescaled_full,
    family = Gamma()
  )

data_priors <-
  base_priors %>%
  dplyr::mutate(
    prior = dplyr::case_when(
      .default = prior,

      # Intercept
      param_name == "(Intercept)" ~
        "(Intercept) ~ normal(0, 1);",
      param_name == "(Intercept)_trend" ~
        "(Intercept)_trend ~ normal(0, 1);",

      # GP marginal SDs (alpha)
      str_detect(param_name, "^real<lower=0> alpha_gp_trend\\(time\\);$") ~
        "alpha_gp_trend(time) ~ normal(0, 1);",
      str_detect(param_name, "^real<lower=0> alpha_gp_trend\\(time\\):trend") ~
        paste0(
          str_extract(param_name, "alpha_gp_trend\\(time\\):trend[^;]*"),
          " ~ normal(0, 1);"
        ),

      # GP length-scales (rho)
      str_detect(param_name, "^real<lower=0> rho_gp_trend\\(time\\);$") ~
        "rho_gp_trend(time) ~ normal(0, 1);",
      str_detect(param_name, "^real<lower=0> rho_gp_trend\\(time\\):trend") ~
        paste0(
          str_remove(
            str_extract(param_name, "rho_gp_trend\\(time\\):trend[^;]*"),
            "\\[1\\]"
          ),
          " ~ normal(0, 1);"
        ),

      # Latent AR(1)
      str_detect(param_name, "^vector<lower=-1,upper=1>\\[n_lv\\] ar1;$") ~
        "ar1 ~ normal(0, 0.5);",

      # Process SDs
      str_detect(param_name, "^vector<lower=0>\\[n_lv\\] sigma;$") ~
        "sigma ~ normal(0, 1);",

      # Gamma shape
      str_detect(param_name, "^vector<lower=0>\\[n_series\\] shape;$") ~
        "shape ~ lognormal(0, 0.5);",

      # Correlation matrix
      str_detect(param_name, "L_Omega") ~
        "L_Omega ~ lkj_corr_cholesky(2);",

      # Smoothing penalties
      str_detect(param_name, "lambda_trend") ~
        "lambda_trend ~ normal(5, 5);",

      # Series effects (e.g. "series41298")
      str_detect(param_name, "^series[0-9]+$") ~
        paste0(param_name, " ~ normal(0, 1);")
    )
  )

set.seed(1234)
hg_model <-
  mvgam::mvgam(
    data = data_roc_rescaled_full,
    formula = ROC_mean ~ series,
    trend_formula = ~ 0 +
      # Shared smooth of year for all series
      s(time,
        k = 10,
        bs = "cr"
      ) +

      # Deviation smooths for each series
      s(time,
        by = trend,
        k = 10,
        bs = c("cr", "sz")
      ),
    trend_model = mvgam::AR(cor = TRUE),
    family = Gamma(),
    use_lv = TRUE,
    share_obs_params = TRUE,
    control = list(
      adapt_delta = 0.98,
      max_treedepth = 17
    ),
    chains = parallelly::availableCores(logical = FALSE) - 1,
    samples = 1000,
    burnin = 200,
    priors = data_priors,
    silent = 1,
    backend = "rstan",
    parallel = TRUE
  )


RUtilpol::save_latest_file(
  hg_model,
  file_name = "hg_model",
  dir = here::here("Data/Processed/")
)
