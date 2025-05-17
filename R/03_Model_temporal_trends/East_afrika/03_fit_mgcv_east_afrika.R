# install.packages("remotes")
# remotes::install_github("HOPE-UIB-BIO/R-Ecopol-package")

library(here)
library(parallelly)
library(tidyverse)
library(RUtilpol)
library(REcopol)
library(mgcv)

verbose <- FALSE
subset_data_to_test_model <- FALSE

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "data_to_fit_east_afrika",
    dir = here::here("Data/Processed/Models/Data_to_fit")
  ) %>%
  tidyr::drop_na()

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
  file_name = "mod_mgcv",
  dir = here::here("Data/Processed/Models/East_afrika/")
)
