library(tidyverse)
library(RUtilpol)
library(mvgam)


hg_model <-
RUtilpol::get_latest_file(
  file_name = "hg_model",
  dir = here::here("Data/Processed/")
)

# Diagnostics
summary(hg_model,
  include_betas = FALSE,
  smooth_test = FALSE
)

mcmc_plot(hg_model,
  type = "rhat_hist"
)

mcmc_plot(hg_model,
  variable = c(
    "sigma",
    "ar1",
    "shape"
  ),
  regex = TRUE,
  type = "trace"
)

# Unconditional posterior check
pp_check(hg_model,
  type = "dens_overlay_grouped",
  group = "series",
  ndraws = 50
)

# Inferences and unconditional predictions
gratia::draw(hg_model, trend_effects = TRUE)

plot_predictions(hg_model,
  condition = c("time", "series"),
  type = "response",
  conf_level = 0.95
)

plot_predictions(hg_model,
  condition = c("time", "series", "series"),
  points = 0.95
)


marginaleffects::avg_predictions(hg_model,
  variable = "series"

)

p1 <-
conditional_effects(
  hg_model,
  effects = "time",
  type = "expected",
  points = TRUE,
  rug = TRUE
) +
  ggplot2::geom_point(
    data = data_subset
  )

class(p1)


global_predictions <- marginaleffects::predictions(
  model = hg_model,
  newdata = data.frame(
    time = 1:45
  ),
  by = "time"
)

# Plot
library(ggplot2)
ggplot(global_predictions, aes(x = time, y = estimate)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  labs(title = "Posterior Global Trend", x = "Time", y = "Rate of Change") +
  theme_minimal()