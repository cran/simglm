## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(dplyr)
library(simglm)
library(splines)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----ns_fixed-----------------------------------------------------------------
set.seed(321)

sim_arguments <- list(
  formula = y ~ 1 + x1 + ns(x2, df = 4),
  fixed = list(
    x1 = list(var_type = 'continuous', mean = 0, sd = 1),
    x2 = list(var_type = 'continuous', mean = 0, sd = 1)
  ),
  sample_size = 10
)

simulate_fixed(data = NULL, sim_arguments)

## ----ns_outcome---------------------------------------------------------------
set.seed(321)

sim_arguments <- list(
  formula = y ~ 1 + x1 + ns(x2, df = 4),
  fixed = list(
    x1 = list(var_type = 'continuous', mean = 0, sd = 1),
    x2 = list(var_type = 'continuous', mean = 0, sd = 1)
  ),
  error = list(variance = 1),
  sample_size = 100,
  reg_weights = list(
    `(Intercept)` = 0,
    x1 = 0.25,
    `ns(x2, df = 4)` = c(0.75, 0.5, 0, 0)
  )
)

sim_data <- simulate_fixed(data = NULL, sim_arguments) |>
  simulate_error(sim_arguments) |>
  generate_response(sim_arguments)

head(sim_data)

## ----bs_outcome---------------------------------------------------------------
set.seed(321)

sim_arguments_bs <- list(
  formula = y ~ 1 + x1 + bs(x2, df = 4),
  fixed = list(
    x1 = list(var_type = 'continuous', mean = 0, sd = 1),
    x2 = list(var_type = 'continuous', mean = 0, sd = 1)
  ),
  error = list(variance = 1),
  sample_size = 100,
  reg_weights = list(
    `(Intercept)` = 0,
    x1 = 0.25,
    `bs(x2, df = 4)` = c(0.75, 0.5, 0, 0)
  )
)

bs_data <- simulate_fixed(data = NULL, sim_arguments_bs) |>
  simulate_error(sim_arguments_bs) |>
  generate_response(sim_arguments_bs)

head(bs_data)

## ----ns_model_fit-------------------------------------------------------------
set.seed(321)

sim_arguments_fit <- list(
  formula = y ~ 1 + x1 + ns(x2, df = 4),
  fixed = list(
    x1 = list(var_type = 'continuous', mean = 0, sd = 1),
    x2 = list(var_type = 'continuous', mean = 0, sd = 1)
  ),
  error = list(variance = 1),
  sample_size = 100,
  reg_weights = list(
    `(Intercept)` = 0,
    x1 = 0.25,
    `ns(x2, df = 4)` = c(0.75, 0.5, 0, 0)
  ),
  model_fit = list(model_function = 'lm')
)

fit <- simulate_fixed(data = NULL, sim_arguments_fit) |>
  simulate_error(sim_arguments_fit) |>
  generate_response(sim_arguments_fit) |>
  model_fit(sim_arguments_fit)

broom::tidy(fit)

## ----ns_power-----------------------------------------------------------------
set.seed(321)

sim_arguments_power <- list(
  formula = y ~ 1 + x1 + ns(x2, df = 4),
  fixed = list(
    x1 = list(var_type = 'continuous', mean = 0, sd = 1),
    x2 = list(var_type = 'continuous', mean = 0, sd = 1)
  ),
  error = list(variance = 1),
  sample_size = 80,
  reg_weights = list(
    `(Intercept)` = 0,
    x1 = 0.25,
    `ns(x2, df = 4)` = c(0.75, 0.5, 0, 0)
  ),
  model_fit = list(model_function = 'lm'),
  power = list(
    thresholds = list(
      x1 = 0.25,
      `ns(x2, df = 4)1` = c(0.5, 0.75),
      `ns(x2, df = 4)2` = 0.5
    )
  ),
  replications = 10,
  extract_coefficients = TRUE
)

power_out <- replicate_simulation(sim_arguments_power)

compute_statistics(
  power_out,
  sim_arguments_power,
  type_1_error = FALSE,
  alternative_power = TRUE
)

