library(here)
library(tidyverse)
library(RUtilpol)

verbose <- FALSE

# subset the data to the East Africa region
vec_dataset_east <-
  readr::read_rds(
    here::here(
      "Data/Input/data_assembly_2023-06-05__4085c7bda5670c4d0b58c91c41b23382__.rds"
    )
  ) %>%
  dplyr::filter(long <= 40) %>%
  dplyr::filter(lat <= 10) %>%
  dplyr::filter(lat >= -15) %>%
  dplyr::filter(long >= 25) %>%
  dplyr::pull(dataset_id)

data_roc_east_afrika <-
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

data_to_fit_east_afrika <-
  data_roc_east_afrika %>%
  dplyr::mutate(
    age = as.integer(Age),
    series = as.factor(dataset_id)
  ) %>%
  # add magnitude of error, which can be used to weight the model
  dplyr::mutate(
    roc_range = ROC_up - ROC_dw,
    roc_error = scales::rescale(
      roc_range,
      to = c(2, 1),
      from = c(0, max(roc_range, na.rm = TRUE))
    )
  )


if (
  isTRUE(verbose)
) {
  message(
    paste0(
      "Number of unique series: ",
      length(unique(data_to_fit_east_afrika$series))
    )
  )

  summary(data_to_fit_east_afrika) %>%
    print()

  data_to_fit_east_afrika %>%
    drop_na() %>%
    dplyr::group_by(series) %>%
    dplyr::summarise(
      n_samples = n(),
      min_age = min(age),
      max_age = max(age)
    ) %>%
    dplyr::arrange(n_samples)
}

RUtilpol::save_latest_file(
  data_to_fit_east_afrika,
  file_name = "data_to_fit_east_afrika",
  dir = here::here("Data/Processed/Models/Data_to_fit"),
)
