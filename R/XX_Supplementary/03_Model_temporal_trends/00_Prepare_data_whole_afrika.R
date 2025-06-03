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
 
data_to_fit_whole_afrika <-
  data_roc_binned %>%
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
      length(unique(data_to_fit_whole_afrika$series))
    )
  )

  summary(data_to_fit_whole_afrika) %>%
    print()

  data_to_fit_whole_afrika %>%
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
  data_to_fit_whole_afrika,
  file_name = "data_to_fit_whole_afrika",
  dir = here::here("Data/Processed/Models/Data_to_fit"),
)

