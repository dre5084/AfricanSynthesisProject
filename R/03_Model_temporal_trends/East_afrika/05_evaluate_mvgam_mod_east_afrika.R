library(tidyverse)
library(RUtilpol)
library(mvgam)


mod_mvgam_gp <-
  RUtilpol::get_latest_file(
    file_name = "mod_mvgam_gp",
    dir = here::here("Data/Processed/Models/East_afrika/")
  )

# Diagnostics
summary(
  mod_mvgam_gp,
  include_betas = FALSE,
  smooth_test = FALSE
)

plot(mod_mvgam_gp, series = 1)

mvgam:::plot.mvgam(mod_mvgam_gp, type = "series", series = 1)
mvgam:::plot.mvgam(mod_mvgam_gp, type = "pterms")
mvgam:::plot.mvgam(mod_mvgam_gp, type = "trend", series = 1)


mvgam::mcmc_plot(
  mod_mvgam_gp,
  type = "rhat_hist"
)

mvgam::mcmc_plot(
  mod_mvgam_gp,
  variable = "sigma",
  regex = TRUE,
  type = "hist"
)

# Unconditional posterior check
mvgam::pp_check(mod_mvgam_gp,
  type = "resid_ribbon_grouped",
  group = "series",
  ndraws = 50
)

data_pred <-
  marginaleffects::avg_predictions(
    mod_mvgam_gp,
    by = "time"
  ) %>%
  as.data.frame()

data_pred_indiv <-
  marginaleffects::predictions(
    mod_mvgam_gp,
    newdata = datagrid(
      time = 1:45,
      series = insight::get_data(mod_mvgam_gp) %>%
        dplyr::distinct(series) %>%
        dplyr::pull(series)
    )
  )

ggplot2::ggplot(
  mapping = ggplot2::aes(
    x = time
  ),
) +
  ggplot2::geom_ribbon(
    data = data_pred,
    ggplot2::aes(
      ymin = conf.low,
      ymax = conf.high
    ),
    alpha = 0.2
  ) +
  ggplot2::geom_line(
    data = as.data.frame(data_pred),
    ggplot2::aes(
      y = estimate
    ),
    linewidth = 1.5,
    col = "black"
  ) +
  ggplot2::geom_line(
    data = as.data.frame(data_pred_indiv),
    ggplot2::aes(
      y = estimate,
      col = series
    ),
    lty = 2,
    linewidth = 1
  ) +
  ggplot2::geom_point(
    data = insight::get_data(mod_mvgam_gp),
    ggplot2::aes(
      y = ROC_mean,
      col = series
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1.25)
  ) +
  ggplot2::facet_wrap(~series)
