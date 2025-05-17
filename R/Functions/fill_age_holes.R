fill_age_holes <- function(data_source) {
  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source must be a data frame"
  )

  assertthat::assert_that(
    "dataset_id" %in% colnames(data_source),
    msg = "data_source must contain a column named 'dataset_id'"
  )

  assertthat::assert_that(
    "time" %in% colnames(data_source),
    msg = "data_source must contain a column named 'time'"
  )

  assertthat::assert_that(
    "ROC_mean" %in% colnames(data_source),
    msg = "data_source must contain a column named 'ROC_mean'"
  )

  data_source %>%
    dplyr::select(dataset_id, time, ROC_mean) %>%
    dplyr::full_join(
      tidyr::expand_grid(
        dataset_id = unique(data_source$dataset_id),
        time = seq(
          min(data_source$time),
          max(data_source$time),
          by = 1
        )
      ),
      by = dplyr::join_by(dataset_id, time)
    ) %>%
    dplyr::arrange(dataset_id, time) %>%
    tidyr::nest(
      data_nested = !dataset_id
    ) %>%
    # interpolate the ROC_mean values for each series (only fill in gaps, do not
    #   extrapolate)
    dplyr::mutate(
      data_nested = purrr::map(
        .progress = TRUE,
        .x = data_nested,
        .f = ~ .x %>%
          dplyr::mutate(
            ROC_mean = approx(
              x = time,
              y = ROC_mean,
              xout = time
            ) %>%
              purrr::chuck("y")
          )
      )
    ) %>%
    tidyr::unnest(
      cols = c(data_nested)
    ) %>%
    return()
}
