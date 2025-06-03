# install.packages("remotes")
# remotes::install_github("HOPE-UIB-BIO/R-Ecopol-package")

library(here)
library(parallelly)
library(tidyverse)
library(RUtilpol)
library(REcopol)
library(mgcv)

verbose <- FALSE

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "data_to_fit_whole_afrika",
    dir = here::here("Data/Processed/Models/Data_to_fit")
  ) %>%
  tidyr::drop_na()

if (
  isTRUE(verbose)
) {
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

set.seed(1234)
mod_mgcv <-
  REcopol::fit_hgam(
    x_var = "age",
    y_var = "ROC_mean",
    group_var = "series",
    error_family = "mgcv::tw(link = 'log')",
    smooth_basis = "cr",
    sel_k = 15,
    data_source = data_to_fit,
    common_trend = TRUE,
    use_parallel = TRUE,
    max_iterations = 1e3,
    verbose = verbose
  )

if (
  isTRUE(verbose)
) {
  summary(mod_mgcv)
}

RUtilpol::save_latest_file(
  mod_mgcv,
  file_name = "mod_mgcv_whole_afrika",
  dir = here::here("Data/Processed/Models/")
)
