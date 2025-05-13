library(tidyverse)
library(RUtilpol)
library(mvgam)


hg_model <-
  RUtilpol::get_latest_file(
    file_name = "hg_model",
    dir = here::here("Data/Processed/")
  )

# Diagnostics
summary(
  hg_model,
  include_betas = FALSE,
  smooth_test = FALSE
)

plot(hg_model)


mvgam:::plot.mvgam(hg_model, type = "series", series = 1)
mvgam:::plot.mvgam(hg_model, type = "pterms")
mvgam:::plot.mvgam(hg_model, type = "trend", series = 1)


mvgam::mcmc_plot(
  hg_model,
  type = "rhat_hist"
)

mvgam::mcmc_plot(
  hg_model,
  variable = "sigma",
  regex = TRUE,
  type = "trace"
)

# Unconditional posterior check
mvgam::pp_check(hg_model,
  type = "dens_overlay_grouped",
  group = "series",
  ndraws = 50
)


data_pred <-
  marginaleffects::avg_predictions(
    hg_model,
    by = "time"
  ) %>%
  as.data.frame()

data_pred_indiv <-
  marginaleffects::predictions(
    hg_model,
    newdata = datagrid(
      time = 1:45,
      series = insight::get_data(hg_model) %>%
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
    data = insight::get_data(hg_model),
    ggplot2::aes(
      y = ROC_mean,
      col = series
    )
  ) +
  ggplot2::coord_cartesian(
    ylim = c(0, 1.25) 
  ) +
  ggplot2::facet_wrap(~series)
