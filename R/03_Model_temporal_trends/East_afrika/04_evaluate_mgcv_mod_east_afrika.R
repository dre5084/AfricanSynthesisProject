library(tidyverse)
library(RUtilpol)
library(REcopol)
library(mgcv)

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "data_to_fit_east_afrika",
    dir = here::here("Data/Processed/Models/Data_to_fit")
  ) %>%
  tidyr::drop_na()

mod_mgcv <-
  RUtilpol::get_latest_file(
    file_name = "mod_mgcv",
    dir = here::here("Data/Processed/Models/East_afrika/")
  )

data_pred <-
  REcopol::predic_model(
    model_source = mod_mgcv,
    data_source = tibble::tibble(
      age = seq(
        from = 0,
        to = 22e3,
        length.out = 100
      )
    ) %>%
      dplyr::mutate(
        series = data_to_fit$series[1]
      ),
    exclude_var = mod_mgcv %>%
      gratia::smooths() %>%
      stringr::str_subset(., "series")
  ) %>%
  dplyr::rename(
    ROC_mean = fit
  ) %>%
  dplyr::select(-series)

data_pred_indiv <-
  REcopol::predic_model(
    model_source = mod_mgcv,
    data_source = tidyr::expand_grid(
      age = seq(
        from = 0,
        to = 22e3,
        length.out = 100
      ),
      series = unique(data_to_fit$series)
    )
  ) %>%
  dplyr::rename(
    ROC_mean = fit
  )

p1 <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age,
      y = ROC_mean
    ),
  ) +
  ggplot2::geom_point(
    data = insight::get_data(mod_mgcv),
    ggplot2::aes(
      col = series
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1.25)
  ) +
  ggplot2::scale_x_continuous(
    transform = "reverse"
  ) +
  ggplot2::theme(
    legend.position = "none"
  )


p1 +
  ggplot2::geom_ribbon(
    data = data_pred,
    ggplot2::aes(
      ymin = upr,
      ymax = lwr
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = as.data.frame(data_pred),
    linewidth = 1.5,
    col = "black"
  )

p1 +
  ggplot2::geom_ribbon(
    data = as.data.frame(data_pred_indiv),
    ggplot2::aes(
      ymin = upr,
      ymax = lwr,
      fill = series
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = as.data.frame(data_pred_indiv),
    ggplot2::aes(
      col = series
    ),
    lty = 2,
    linewidth = 1
  ) +
  ggplot2::facet_wrap(~series)
