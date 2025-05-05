library(tidyverse)
library(here)
library(RUtilpol)
library(REcopol)

age_max <- 22.5e3
age_min <- 0
age_step <- 500

vec_dataset_east <-
  RUtilpol::get_latest_file(
    file_name = "data_east",
    dir = here::here("Data/Processed/")
  ) %>%
  dplyr::pull(dataset_id)

data_roc <-
  vec_dataset_east %>%
  rlang::set_names() %>%
  purrr::map(
    .x = ,
    .f = ~ {
      message(
        "Loading ROC for dataset_id: ",
        ..1, "\n"
      )

      RUtilpol::get_latest_file(
        file_name = ..1,
        dir = here::here("Data/Processed/Roc"),
        verbose = FALSE
      )
    }
  ) %>%
  # filter the list so that only entries which are data frames are kept
  purrr::keep(~ is.data.frame(.x)) %>%
  dplyr::bind_rows(.id = "dataset_id")

data_roc_binned <-
  REcopol:::add_age_bin(
    data_roc,
    bin_size = age_step,
    age_var_name = "Age",
    sel_method = "backward"
  ) %>%
  dplyr::group_by(dataset_id, BIN) %>%
  dplyr::summarise(
    ROC_mean = mean(ROC, na.rm = TRUE),
    .groups = "drop"
  )

data_age_ref <-
  tibble::tibble(
    BIN = seq(age_min, age_max, age_step)
  ) %>%
  dplyr::mutate(
    time = 1 + (BIN / age_step - (age_max / age_step)) * (-1)
  ) %>%
  dplyr::arrange(time)

data_roc_rescaled <-
  data_roc_binned %>%
  dplyr::left_join(
    data_age_ref,
    by = "BIN"
  )

RUtilpol::save_latest_file(
  data_roc_rescaled,
  file_name = "data_roc_rescaled",
  dir = here::here("Data/Processed/")
)
