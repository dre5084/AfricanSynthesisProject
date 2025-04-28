library(tidyverse)
library(RUtilpol)
library(here)

data_assembly <-
  readr::read_rds(
    here::here(
      "Data/Input/data_assembly_2023-06-05__4085c7bda5670c4d0b58c91c41b23382__.rds"
    )
  )

vec_dataset_east <-
  data_assembly %>%
  dplyr::select(dataset_id, lat, long) %>%
  rlang::set_names(
    nm = c("dataset_id", "site_lat", "site_lon")
  ) %>%
  dplyr::filter(site_lon <= 40) %>%
  dplyr::filter(site_lat <= 10) %>%
  dplyr::filter(site_lat >= -15) %>%
  dplyr::filter(site_lon >= 25) %>%
  dplyr::pull(dataset_id)

data_east <-
  data_assembly %>%
  dplyr::filter(
    dataset_id %in% vec_dataset_east
  )

RUtilpol::save_latest_file(
  object_to_save = data_east,
  file_name = "data_east",
  dir = here::here("Data/Processed/"),
)
