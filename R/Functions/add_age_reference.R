add_age_reference <- function(
    data_source,
    age_max = 22.5e3,
    age_min = 0,
    age_step = 500,
    link_by = c("age", "time")) {
  `.data` <- rlang::.data
  `%>%` <- magrittr::`%>%`

  assertthat::assert_that(
    is.data.frame(data_source),
    msg = "data_source must be a data frame"
  )

  link_by <- match.arg(link_by)

  assertthat::assert_that(
    all(link_by %in% colnames(data_source)),
    msg = paste("data_source must contain column '", link_by, "'", sep = "")
  )

  assertthat::assert_that(
    is.character(link_by),
    msg = "link_by must be a character vector"
  )

  assertthat::assert_that(
    link_by %in% c("age", "time"),
    msg = paste("link_by must be one of 'age' or 'time', not ", link_by)
  )

  assertthat::assert_that(
    is.numeric(age_max),
    msg = "age_max must be numeric"
  )

  assertthat::assert_that(
    is.numeric(age_min),
    msg = "age_min must be numeric"
  )

  assertthat::assert_that(
    is.numeric(age_step),
    msg = "age_step must be numeric"
  )

  assertthat::assert_that(
    age_max > age_min,
    msg = "age_max must be greater than age_min"
  )

  assertthat::assert_that(
    age_step > 0,
    msg = "age_step must be greater than 0"
  )

  data_age_ref <-
    tibble::tibble(
      age = seq(age_min, age_max, age_step)
    ) %>%
    dplyr::mutate(
      time = 1 + (.data$age / age_step - (age_max / age_step)) * (-1)
    ) %>%
    dplyr::arrange(time)

  res <-
    data_source %>%
    dplyr::left_join(
      data_age_ref,
      by = link_by
    )

  return(res)
}
