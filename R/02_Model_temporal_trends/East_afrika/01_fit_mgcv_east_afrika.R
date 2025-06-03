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
    file_name = "data_to_fit_east_afrika",
    dir = here::here("Data/Processed/Data_to_fit/East_afrika")
  ) %>%
  tidyr::drop_na()


if (
  isTRUE(verbose)
) {
  ggplot2::ggplot(
    data = data_to_fit,
    mapping = ggplot2::aes(x = ROC)
  ) +
    ggplot2::geom_histogram() +
    ggplot2::labs(
      y = "Frequency",
      x = "ROC"
    )

  summary(data_to_fit)
}

set.seed(1234)
mod_mgcv <-
  REcopol::fit_hgam(
    x_var = "age",
    y_var = "ROC",
    group_var = "series",
    weights_var = "roc_error",
    error_family = "mgcv::tw(link = 'log')",
    smooth_basis = "cr",
    sel_k = 15,
    data_source = data_to_fit,
    common_trend = TRUE,
    use_parallel = FALSE,
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
  file_name = "mod_mgcv_east_afrika",
  dir = here::here("Data/Processed/Models/East_afrika/")
)
