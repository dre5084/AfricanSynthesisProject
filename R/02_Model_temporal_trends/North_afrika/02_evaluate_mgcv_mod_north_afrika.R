library(tidyverse)
library(RUtilpol)
library(REcopol)
library(mgcv)

data_to_fit <-
  RUtilpol::get_latest_file(
    file_name = "data_to_fit_north_afrika",
    dir = here::here("Data/Processed/Data_to_fit/North_afrika")
  ) %>%
  tidyr::drop_na()

mod_mgcv <-
  RUtilpol::get_latest_file(
    file_name = "mod_mgcv_north_afrika",
    dir = here::here("Data/Processed/Models/North_afrika/")
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
        series = unique(data_to_fit$series)[2]
      ),
    exclude_var = mod_mgcv %>%
      gratia::smooths() %>%
      stringr::str_subset(., "series")
  ) %>%
  dplyr::rename(
    ROC = fit
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
    ROC = fit
  )

p1 <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age,
      y = ROC
    ),
  ) +
  ggplot2::geom_point(
    data = data_to_fit,
    ggplot2::aes(
      col = series
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 2)
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
      ymin = lwr,
      ymax = upr
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


# Note REcopol::predic_model() is using marginal effects
# under the hood. I try to confirm that the predictions are the same
# by comparing them with the marginal effects

data_pred_marginal <-
  marginaleffects::predictions(
    model = mod_mgcv,
    newdata = tibble::tibble(
      age = seq(
        from = 0,
        to = 22e3,
        length.out = 100
      )
    ) %>%
      dplyr::mutate(
        series = unique(data_to_fit$series)[2]
      ),
    exclude = mod_mgcv %>%
      gratia::smooths() %>%
      stringr::str_subset(., "series")
  ) %>%
  tibble::as_tibble() %>%
  dplyr::select(age, estimate, conf.low, conf.high)

p1 +
  ggplot2::geom_ribbon(
    data = data_pred_marginal,
    ggplot2::aes(
      y = estimate,
      ymin = conf.low,
      ymax = conf.high
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = as.data.frame(data_pred_marginal),
    mapping = ggplot2::aes(
      y = estimate
    ),
    linewidth = 1.5,
    col = "black"
  )
