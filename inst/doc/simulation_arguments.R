## ----setup, include=FALSE------------------------------------------------
library(knitr)
library(dplyr)
library(simglm)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----fixed_sim-----------------------------------------------------------
library(simglm)

set.seed(321) 

# To-DO: Add knot variable and debug

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  sample_size = list(level1 = 10, level2 = 20)
)

fixed_data <- simulate_fixed(data = NULL, sim_arguments)
head(fixed_data, n = 20)

## ----time_var------------------------------------------------------------
set.seed(321) 

# To-DO: Add knot variable and debug

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time',
                           time_levels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6)),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  sample_size = list(level1 = 10, level2 = 20)
)

fixed_data <- simulate_fixed(data = NULL, sim_arguments)
head(fixed_data, n = 20)

## ----rbeta---------------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time',
                           time_levels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6)),
               weight = list(var_type = 'continuous', dist = 'rgamma', 
                             shape = 3),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  sample_size = list(level1 = 10, level2 = 20)
)

fixed_data <- simulate_fixed(data = NULL, sim_arguments)
head(fixed_data, n = 20)

## ----dist_beta-----------------------------------------------------------
library(ggplot2)

ggplot(fixed_data, aes(x = weight)) + 
  geom_density() + 
  theme_bw()

## ----random_error--------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  sample_size = list(level1 = 10, level2 = 20)
)

error_data <- simulate_error(data = NULL, sim_arguments)
head(error_data, n = 20)

## ----random_error_t------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  error = list(dist = 'rt', df = 4),
  sample_size = list(level1 = 10, level2 = 20)
)

error_data <- simulate_error(data = NULL, sim_arguments)
head(error_data, n = 20)

## ----re_10---------------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  error = list(dist = 'rt', df = 4, variance = 10),
  sample_size = list(level1 = 10, level2 = 20)
)

error_data <- simulate_error(data = NULL, sim_arguments)
var(error_data$error)

## ----re_10_ts------------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  error = list(dist = 'rt', df = 4, variance = 10, ther_sim = TRUE),
  sample_size = list(level1 = 10, level2 = 20)
)

error_data <- simulate_error(data = NULL, sim_arguments)
var(error_data$error)

## ----re_10_specify-------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  error = list(dist = 'rt', df = 4, variance = 10, ther_val = c(0, sqrt(2))),
  sample_size = list(level1 = 10, level2 = 20)
)

error_data <- simulate_error(data = NULL, sim_arguments)
var(error_data$error)

## ----random_args---------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  randomeffect = list(int_id = list(variance = 8, var_level = 2),
                time_id = list(variance = 3, var_level = 2)),
  sample_size = list(level1 = 10, level2 = 20)
)

random_data <- simulate_randomeffect(data = NULL, sim_arguments)
head(random_data, n = 20)

## ----cross-class---------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id) +
    (1 | neighborhood_id),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  randomeffect = list(int_id = list(variance = 8, var_level = 2),
                time_id = list(variance = 3, var_level = 2),
                int_nid = list(variance = 5, var_level = 2,
                               cross_class = TRUE,
                               num_ids = 12)),
  sample_size = list(level1 = 10, level2 = 20)
)

random_data <- simulate_randomeffect(data = NULL, sim_arguments)
head(random_data, n = 20)

## ----missing_random------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  reg_weights = c(4, 0.5, 0.75, 0, 0.33),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  randomeffect = list(int_id = list(variance = 8, var_level = 2),
                      time_id = list(variance = 3, var_level = 2)),
  missing_data = list(miss_prop = .25, new_outcome = 'y_missing',
                      type = 'random'),
  sample_size = list(level1 = 10, level2 = 20)
)

data_w_missing <- sim_arguments %>%
  simulate_fixed(data = NULL, .) %>%
  simulate_randomeffect(sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments) %>%
  generate_missing(sim_arguments)

head(data_w_missing, n = 10)

## ----amount_missing------------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_mar---------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  reg_weights = c(4, 0.5, 0.75, 0, 0.33),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30,
                             var_level = 1),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  randomeffect = list(int_id = list(variance = 8, var_level = 2),
                      time_id = list(variance = 3, var_level = 2)),
  missing_data = list(new_outcome = 'y_missing', miss_cov = 'weight', 
                      mar_prop = seq(from = 0, to = .9, length.out = 200),
                      type = 'mar'),
  sample_size = list(level1 = 10, level2 = 20)
)

data_w_missing <- sim_arguments %>%
  simulate_fixed(data = NULL, .) %>%
  simulate_randomeffect(sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments) %>%
  generate_missing(sim_arguments)

head(data_w_missing, n = 10)

## ----amount_missing_mar--------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_dropout-----------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  reg_weights = c(4, 0.5, 0.75, 0, 0.33),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  randomeffect = list(int_id = list(variance = 8, var_level = 2),
                      time_id = list(variance = 3, var_level = 2)),
  missing_data = list(miss_prop = .25, new_outcome = 'y_missing',
                      clust_var = 'id', type = 'dropout'),
  sample_size = list(level1 = 10, level2 = 20)
)

data_w_missing <- sim_arguments %>%
  simulate_fixed(data = NULL, .) %>%
  simulate_randomeffect(sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments) %>%
  generate_missing(sim_arguments)

head(data_w_missing, n = 10)

## ----amount_missing_dropout----------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_by_time-----------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing), data_w_missing$time))

## ----missing_dropout_spec------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
  reg_weights = c(4, 0.5, 0.75, 0, 0.33),
  fixed = list(time = list(var_type = 'time'),
               weight = list(var_type = 'continuous', mean = 180, sd = 30),
               age = list(var_type = 'ordinal', levels = 30:60, var_level = 2),
               treat = list(var_type = 'factor', 
                            levels = c('Treatment', 'Control'),
                            var_level = 2)),
  randomeffect = list(int_id = list(variance = 8, var_level = 2),
                      time_id = list(variance = 3, var_level = 2)),
  missing_data = list(new_outcome = 'y_missing',
      dropout_location = c(3, 9, 1, 6, 7, 8, 6, 9, 2, 4, 6, 5, 8, 9, 4, 5, 
                           6, 7, 2, 9),
                      clust_var = 'id', type = 'dropout'),
  sample_size = list(level1 = 10, level2 = 20)
)

data_w_missing <- sim_arguments %>%
  simulate_fixed(data = NULL, .) %>%
  simulate_randomeffect(sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments) %>%
  generate_missing(sim_arguments)

head(data_w_missing, n = 10)

## ----amount_missing_dropout_spec-----------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_by_time_dropout_spec----------------------------------------
prop.table(table(is.na(data_w_missing$y_missing), data_w_missing$time))

