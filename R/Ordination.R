# Incomplete as of now
########### Recycled code
library(tidyverse)
library(here)
library(RUtilpol)
library(REcopol)

age_max <- 22.5e3
age_min <- 0
age_step <- 500

data_roc_rescaled <-
  RUtilpol::get_latest_file(
    file_name = "data_roc_rescaled", # File name needs changed
    dir = here::here("Data/Processed")
  ) %>%
  dplyr::mutate(
    dataset_id = as.character(dataset_id),
    dataset_id = as.factor(dataset_id)
  )

######### Novel Code
# Loop that check for gaps in time for each dataset, interpolating the gaps, and
# adding the results to a new tibble so no gaps exist for each dataset
data_interp <- tibble()

vec_dataset_id <- unique(data_roc_rescaled$dataset_id)

for (i in seq_along(vec_dataset_id)) {
  message("Processing dataset_id: ", vec_dataset_id[i])

  data_subset <-
    data_roc_rescaled %>%
    dplyr::filter(
      dataset_id %in% vec_dataset_id[i]
    )

  time_complete <-
    tibble::tibble(
      time = seq(min(data_subset$time),
        max(data_subset$time),
        by = 1
      )
    )

  joined_subset <-
    time_complete %>%
    dplyr::left_join(data_subset, by = "time")

  interp <-
    joined_subset %>%
    dplyr::arrange(time) %>%
    dplyr::mutate(
      ROC_mean = approx(
        x = time,
        y = ROC_mean,
        xout = time
      )$y
    )

  data_interp <-
    bind_rows(data_interp, interp)

  rm(data_subset, time_complete, joined_subset, interp)
}

data_interp %>%
  ggplot(
    mapping = aes(x = time, y = ROC_mean, color = dataset_id)
  ) +
  geom_line() +
  theme_bw() +
  facet_wrap(~dataset_id)


vec_long_cores <-
  data_interp %>%
  dplyr::group_by(dataset_id) %>%
  dplyr::summarise(
    has_data = sum(ROC_mean > 0)
  ) %>%
  dplyr::arrange(desc(has_data)) %>%
  slice(1:3) %>%
  pull(dataset_id)

data_interp_wide <-
  data_interp %>%
  dplyr::filter(dataset_id %in% vec_long_cores) %>%
  tidyr::pivot_wider(
    names_from = dataset_id,
    values_from = ROC_mean
  )

data_to_fit <-
  data_interp_wide %>%
  dplyr::filter(BIN != 21e3) %>%
  dplyr::select(-time)

data_to_fit_roc <-
  data_to_fit %>%
  tibble::column_to_rownames(var = "BIN")

data_to_fit_age <-
  data_to_fit %>%
  dplyr::select(BIN) %>%
  dplyr::mutate(
    age = BIN
  ) %>%
  tibble::column_to_rownames(var = "BIN")


# Ordination w/DCCA
res_dcca <-
  REcopol:::fit_ordination_dcca(
    data_source_resp = data_to_fit_roc,
    data_source_pred = data_to_fit_age,
    sel_complexity = "poly_3",
    downweight = FALSE
  )

# Visualize the results
res_dcca$case_r %>%
  ggplot2::ggplot(
    ggplot2::aes(y = axis_1, x = as.numeric(sample_id))
  ) +
  ggplot2::geom_line() +
  ggplot2::theme_bw()
