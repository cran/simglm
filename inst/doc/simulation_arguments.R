## ----setup, include=FALSE-----------------------------------------------------
library(knitr)
library(dplyr)
library(simglm)
knit_print.data.frame = function(x, ...) {
  res = paste(c('', '', kable(x, output = FALSE)), collapse = '\n')
  asis_output(res)
}

## ----fixed_sim----------------------------------------------------------------
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

## ----time_var-----------------------------------------------------------------
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

## ----rbeta--------------------------------------------------------------------
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

## ----dist_beta----------------------------------------------------------------
library(ggplot2)

ggplot(fixed_data, aes(x = weight)) + 
  geom_density() + 
  theme_bw()

## ----knot-args----------------------------------------------------------------
sim_args <- list(
    formula = y ~ 1  + age + age_knot,
    fixed = list(age = list(var_type = 'ordinal', levels = 30:60)),
    knot = list(age_knot = list(variable = 'age', 
                                knot_locations = 50)),
    sample_size = 500,
    error = list(variance = 10),
    reg_weights = c(2, .5, 1.5)
  )

## ----knot-sim-----------------------------------------------------------------
simulate_fixed(data = NULL, sim_args = sim_args) %>%
  head()

## ----knot-args-int------------------------------------------------------------
sim_args <- list(
    formula = y ~ 1  + age + age_knot + age:age_knot,
    fixed = list(age = list(var_type = 'ordinal', levels = 30:60)),
    knot = list(age_knot = list(variable = 'age', 
                                knot_locations = 50)),
    sample_size = 500,
    error = list(variance = 1000),
    reg_weights = c(2, .5, 1.5, 10)
  )

simulate_fixed(data = NULL, sim_args = sim_args) %>% 
  head()

## ----random_error-------------------------------------------------------------
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

## ----random_error_t-----------------------------------------------------------
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

## ----re_10--------------------------------------------------------------------
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

## ----re_10_ts-----------------------------------------------------------------
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

## ----re_10_specify------------------------------------------------------------
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

## ----heterogeneity------------------------------------------------------------
simulation_arguments <- list(
  formula = y ~ 1 + group,
  fixed = list(group = list(var_type = 'factor', 
                            levels = c('male', 'female'))),
  sample_size = 500,
  error = list(variance = 1),
  heterogeneity = list(variable = 'group',
                       variance = c(1, 8)),
  reg_weights = c(0, .15)
)

hetero_data <- simulate_fixed(data = NULL, simulation_arguments) %>%
  simulate_error(simulation_arguments) %>%
  simulate_heterogeneity(simulation_arguments)

## ----heterogeneity-var--------------------------------------------------------
hetero_data %>% 
  group_by(group) %>% 
  summarise(var_error = var(error), 
            var_o_error = var(orig_error))

## ----random_args--------------------------------------------------------------
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

## ----cross-class--------------------------------------------------------------
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

## ----correlate-fixed----------------------------------------------------------
sim_args <- list(formula = y ~ 1 + act + gpa + commute_time, 
                 fixed = list(act = list(var_type = 'ordinal',
                                         levels = 15:36),
                              gpa = list(var_type = 'continuous',
                                         mean = 2, 
                                         sd = .5),
                              commute_time = list(var_type = 'continuous',
                                         mean = 15, 
                                         sd = 6)),
                 correlate = list(fixed = data.frame(x = c('act', 'act', 'gpa'), 
                                                     y = c('gpa', 'commute_time', 'commute_time'), 
                                                     corr = c(0.5, .6, .2))),
                 sample_size = 1000)

correlate_attribute <- simulate_fixed(data = NULL, sim_args) %>%
  correlate_variables(sim_args)

head(correlate_attribute)

## ----correlate-fixed-values---------------------------------------------------
select(correlate_attribute, -X.Intercept., -level1_id) %>%
  cor(.)

## ----random-correlate---------------------------------------------------------
sim_args <- list(formula = y ~ 1 + act + gpa + sat + (1 + act | id), 
                 fixed = list(act = list(var_type = 'continuous',
                                         mean = 20, 
                                         sd = 4),
                              gpa = list(var_type = 'continuous',
                                         mean = 2, 
                                         sd = .5),
                              sat = list(var_type = 'continuous',
                                         mean = 500, 
                                         sd = 100)),
                 
                 randomeffect = list(int_id = list(variance = 8, var_level = 2),
                                     act_id = list(variance = 3, var_level = 2)),
                 sample_size = list(level1 = 100, level2 = 5000),
                 correlate = list(random = data.frame(x = 'int_id', y = 'act_id',
                                                      corr = .3))
                 )

random_correlate <- simulate_randomeffect(data = NULL, sim_args) %>%
  correlate_variables(sim_args)

head(random_correlate)

## ----random-correlate-values--------------------------------------------------
select(random_correlate, -level1_id, -id) %>%
  cor(.)

## ----missing_random-----------------------------------------------------------
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

## ----amount_missing-----------------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_mar--------------------------------------------------------------
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

## ----amount_missing_mar-------------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_dropout----------------------------------------------------------
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

## ----amount_missing_dropout---------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_by_time----------------------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing), data_w_missing$time))

## ----missing_dropout_spec-----------------------------------------------------
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

## ----amount_missing_dropout_spec----------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing)))

## ----missing_by_time_dropout_spec---------------------------------------------
prop.table(table(is.na(data_w_missing$y_missing), data_w_missing$time))

## ----binomial_logit-----------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + weight + age + sex,
  fixed = list(weight = list(var_type = 'continuous', mean = 0, sd = 30),
               age = list(var_type = 'ordinal', levels = 0:30),
               sex = list(var_type = 'factor', levels = c('male', 'female'))),
  error = list(variance = 25),
  sample_size = 10,
  reg_weights = c(2, 0.3, -0.1, 0.5),
  outcome_type = 'binary',
  model_fit = list(
    model_function = 'glm',
    family = binomial
  )
)

simulate_fixed(data = NULL, sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments) %>%
  model_fit(sim_arguments) %>% .$family

## ----binomial_probit----------------------------------------------------------
set.seed(321) 

sim_arguments <- list(
  formula = y ~ 1 + weight + age + sex,
  fixed = list(weight = list(var_type = 'continuous', mean = 0, sd = 30),
               age = list(var_type = 'ordinal', levels = 0:30),
               sex = list(var_type = 'factor', levels = c('male', 'female'))),
  error = list(variance = 25),
  sample_size = 10,
  reg_weights = c(2, 0.3, -0.1, 0.5),
  outcome_type = 'binary',
  model_fit = list(
    model_function = 'glm',
    family = binomial(link = 'probit')
  )
)

simulate_fixed(data = NULL, sim_arguments) %>%
  simulate_error(sim_arguments) %>%
  generate_response(sim_arguments) %>%
  model_fit(sim_arguments) %>% .$family

## ----gee, eval = FALSE--------------------------------------------------------
#  set.seed(321)
#  
#  # To-DO: Add knot variable and debug
#  
#  sim_arguments <- list(
#    formula = y ~ 1 + time + weight + age + treat + (1 + time| id),
#    fixed = list(time = list(var_type = 'time',
#                             time_levels = c(0, 0.5, 1, 1.5, 2, 2.5, 3, 4, 5, 6)),
#                 weight = list(var_type = 'continuous', mean = 0, sd = 30),
#                 age = list(var_type = 'ordinal', levels = 0:30, var_level = 2),
#                 treat = list(var_type = 'factor',
#                              levels = c('Treatment', 'Control'),
#                              var_level = 2)),
#    reg_weights = c(0.4, 0.2, -0.5, 1, -0.6),
#    randomeffect = list(int_id = list(variance = 8, var_level = 2),
#                  time_id = list(variance = 3, var_level = 2)),
#    error = list(variance = 5),
#    outcome_type = 'binary',
#    sample_size = list(level1 = 10, level2 = 20),
#    model_fit = list(
#      model_function = geepack::geeglm,
#      formula = y ~ 1 + time + weight + age + treat,
#      id = 'level1_id',
#      family = binomial,
#      corstr = 'ar1'
#    )
#  )
#  
#  simulate_fixed(data = NULL, sim_arguments) %>%
#    simulate_error(sim_arguments) %>%
#    simulate_randomeffect(sim_arguments) %>%
#    generate_response(sim_arguments) %>%
#    model_fit(sim_arguments) %>%
#    extract_coefficients()

## ----vary_simulation----------------------------------------------------------
library(future)
plan(sequential)

sim_arguments <- list(
  formula =  resp_var ~ 1 + time + factor(trt) + time:factor(trt) + 
    (1 + time | individual),
  reg_weights = c(4, 0.5, 0.75, 0),
  fixed = list(time = list(var_type = 'time'),
               trt = list(var_type = 'factor', levels = c('Drug', 'Placebo'), 
                          var_level = 2)
               ),
  randomeffect = list(int_clust = list(variance = .2, var_level = 2),
                      time_clust = list(variance = .3, var_level = 2)),
  replications = 3,
  error = list(variance = 1),
  vary_arguments = list(
    sample_size = list(list(level1 = 5, level2 = 50),
                       list(level1 = 5, level2 = 60))
  )
)

replicate_simulation(sim_arguments)

