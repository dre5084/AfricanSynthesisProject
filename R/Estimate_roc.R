library(tidyverse)
library(RUtilpol)
library(here)
library(RRatepol)

n_rand <- 10e3
use_parallel <- TRUE

data_to_estimate_roc <-
  RUtilpol::get_latest_file(
    file_name = "data_east",
    dir = here::here("Data/Processed/")
  ) %>%
  dplyr::select(
    dataset_id,
    levels,
    counts_harmonised,
    age_uncertainty
  )

purrr::pwalk(
  .l = list(
    data_to_estimate_roc$dataset_id, # ..1
    data_to_estimate_roc$levels, # ..2
    data_to_estimate_roc$counts_harmonised, # ..3
    data_to_estimate_roc$age_uncertainty # ..4
  ),
  .f = purrr::possibly(
    .f = ~ {
      message(
        "Estimating ROC for dataset_id: ",
        ..1, "\n"
      )

      set.seed(42)

      res <-
        RRatepol::estimate_roc(
          data_source_age = ..2,
          data_source_community = ..3,
          age_uncertainty = ..4,
          dissimilarity_coefficient = "chisq", # Chi-Squared
          working_units = "MW", # Moving Window approach
          bin_size = 500, # Bin size, in years, for computation
          number_of_shifts = 5, # Moving window frames within bin
          time_standardisation = 500, # Output bin size
          smooth_method = "shep",
          standardise = TRUE,
          n_individuals = 150, # Number of required pollen grains
          rand = n_rand,
          use_parallel = use_parallel
        ) %>%
        RRatepol::fc_detect_peak_points(
          sel_method = "trend_non_linear",
          sd_threshold = 2
        )

      RUtilpol::save_latest_file(
        object_to_save = res,
        file_name = ..1,
        dir = here::here("Data/Processed/Roc")
      )
    },
    otherwise = NA_real_, # Return NA if error occurs
  )
)
