library(tidyverse)
library(here)
library(RUtilpol)
library(REcopol)

age_step <- 500

vec_datasets <-
  readr::read_rds(
    here::here(
      "Data/Input/data_assembly_2023-06-05__4085c7bda5670c4d0b58c91c41b23382__.rds"
    )
  ) %>%
  dplyr::pull(dataset_id)

data_roc <-
  vec_datasets %>%
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

RUtilpol::save_latest_file(
  data_roc_binned,
  file_name = "data_roc_binned",
  dir = here::here("Data/Processed/Roc_binned"),
)
