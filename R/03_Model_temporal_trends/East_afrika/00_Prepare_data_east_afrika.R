library(here)
library(tidyverse)
library(RUtilpol)

# load needed functions
c(
  "add_age_reference",
  "fill_age_holes"
) %>%
  purrr::walk(
    .f = ~ here::here(
      "R/Functions",
      paste0(.x, ".R")
    ) %>%
      source()
  )

verbose <- FALSE

data_roc_binned <-
  RUtilpol::get_latest_file(
    file_name = "data_roc_binned",
    dir = here::here("Data/Processed/Roc_binned")
  )

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

data_roc_binned_east <-
  data_roc_binned %>%
  dplyr::filter(
    dataset_id %in% vec_dataset_east
  )

data_to_fit_east_afrika <-
  data_roc_binned_east %>%
  dplyr::mutate(
    age = BIN
  ) %>%
  # add age rescaling
  add_age_reference() %>%
  # fill in hole in the age series by interpolating the ROC_mean values
  fill_age_holes() %>%
  add_age_reference(
    link_by = "time"
  )  %>% 
  dplyr::mutate(
    time = as.integer(time),
    age = as.integer(age),
    series = as.factor(dataset_id)
  ) %>%
  dplyr::select(-dataset_id)

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
      length = n(),
      min_time = min(time),
      max_time = max(time)
    ) %>%
    dplyr::arrange(length)
}

RUtilpol::save_latest_file(
  data_to_fit_east_afrika,
  file_name = "data_to_fit_east_afrika",
  dir = here::here("Data/Processed/Models/Data_to_fit"),
)
