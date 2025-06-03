library(tidyverse)
library(RUtilpol)
library(mvgam)

mod_mvgam <-
  RUtilpol::get_latest_file(
    file_name = "mod_mvgam_smooth",
    dir = here::here("Data/Processed/Models/East_afrika/")
  )


# Diagnostics
summary(
  mod_mvgam,
  include_betas = FALSE,
  smooth_test = FALSE
)

plot(mod_mvgam, series = 1)

mvgam:::plot.mvgam(mod_mvgam, type = "series", series = 1)
mvgam:::plot.mvgam(mod_mvgam, type = "pterms")
mvgam:::plot.mvgam(mod_mvgam, type = "trend", series = 1)


mvgam::mcmc_plot(
  mod_mvgam,
  type = "rhat_hist"
)

mvgam::mcmc_plot(
  mod_mvgam,
  variable = "sigma",
  regex = TRUE,
  type = "hist"
)

# Unconditional posterior check
mvgam::pp_check(mod_mvgam,
  type = "resid_ribbon_grouped",
  group = "series",
  ndraws = 50
)

data_pred <-
  marginaleffects::avg_predictions(
    mod_mvgam,
    by = "age",
    type = "response",
    conf_level = 0.95,
    newdata = datagrid(
      age = seq(0, 22.5e3, length.out = 100)
    )
  ) %>%
  as.data.frame()

data_pred_indiv <-
  marginaleffects::predictions(
    mod_mvgam,
    type = "response",
    conf_level = 0.95,
    newdata = datagrid(
      age = seq(0, 22.5e3, length.out = 100),
      series = insight::get_data(mod_mvgam) %>%
        dplyr::distinct(series) %>%
        dplyr::pull(series)
    )
  ) %>%
  as.data.frame() 

p1 <-
  ggplot2::ggplot(
    mapping = ggplot2::aes(
      x = age,
      y = ROC_mean
    ),
  ) +
  ggplot2::geom_point(
    data = insight::get_data(mod_mvgam),
    ggplot2::aes(
      col = series
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1.25),
    xlim = c(22e3, 0)
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
      y = estimate,
      ymin = conf.high,
      ymax = conf.low
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = data_pred,
    mapping = ggplot2::aes(
      y = estimate
    ),
    linewidth = 1.5,
    col = "black"
  )

p1 +
  ggplot2::geom_ribbon(
    data = data_pred_indiv,
    ggplot2::aes(
      y = estimate,
      ymin = conf.high,
      ymax = conf.low,
      fill = series
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = data_pred_indiv,
    ggplot2::aes(
      y = estimate,
      col = series
    ),
    lty = 2,
    linewidth = 1
  ) +
  ggplot2::facet_wrap(~series)
