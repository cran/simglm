# context('fixef_sim')
# 
# test_that('sim_factor are discrete', {
#   expect_length(table(sim_factor(n = 100, numlevels = 4, 
#                                  prob = c(.25, .25, .25, .25), 
#                                  replace = TRUE,
#                           var_type = 'single')), 4)
#   expect_length(table(sim_factor(n = 100, p = rep(3, 100),numlevels = 4, 
#                                  prob = c(.25, .25, .25, .25), 
#                                  replace = TRUE,
#                                      var_type = 'level1')), 4)
#   expect_length(table(sim_factor(n = 100, p = rep(3, 100), numlevels = 4, 
#                                  prob = c(.25, .25, .25, .25), 
#                                  replace = TRUE,
#                                      var_type = 'level2')), 4)
#   expect_length(table(sim_factor(n = 100, p = rep(3, 100), k = 30, 
#                                  numlevels = 4, 
#                                  prob = c(.25, .25, .25, .25), 
#                                  replace = TRUE,
#                                      var_type = 'level3')), 4)
# })
# 
# test_that('sim_factor errors', {
#   expect_error(sim_factor(n = 100, p = rep(3, 100), numlevels = 4, 
#                           replace = FALSE,
#                           prob = c(.25, .25, .25, .25), 
#                           var_type = 'level2'))
#   expect_error(sim_factor(n = 100, numlevels = 4, replace = FALSE,
#                           prob = c(.25, .25, .25, .25),
#                           var_type = 'single'))
#   expect_error(sim_factor(n = 100, p = rep(3, 100), numlevels = 4, 
#                           replace = FALSE,
#                           prob = c(.25, .25, .25, .25),
#                           var_type = 'level1'))
#   expect_error(sim_factor(n = 100, p = rep(3, 100), k = 30, numlevels = 4, 
#                           replace = FALSE,
#                           prob = c(.25, .25, .25, .25), 
#                           var_type = 'level3'))
# })
# 
# test_that('sim_continuous are continuous', {
#   expect_length(table(sim_continuous(n = 100, dist_fun = 'rnorm', 
#                                      mean = 0, sd = 1, 
#                                      var_type = 'single')), 100)
#   expect_length(table(sim_continuous(n = 100, p = rep(3, 100), 
#                                      dist_fun = 'rnorm', mean = 0, sd = 1, 
#                                      var_type = 'level1')), 300)
#   expect_length(table(sim_continuous(n = 100, p = rep(3, 100), 
#                                      dist_fun = 'rnorm', mean = 0, sd = 1, 
#                                      var_type = 'level2')), 100)
#   expect_length(table(sim_continuous(n = 100, p = rep(3, 100), k = 30, 
#                                      dist_fun = 'rnorm',
#                                      mean = 0, sd = 1, 
#                                      var_type = 'level3')), 30)
# })
# 
test_that('number of columns, fixed generation', {
  sim_arguments <- list(
    formula = y ~ 1 + sex + type + sex:type,
    fixed = list(sex = list(var_type = 'factor', levels=c('fem','male')),
                 type = list(var_type = 'factor', levels = c('type1', 'type2', 'type3'))),
    error = list(variance = 0.2),
    sample_size = 150,
    reg_weights = c(0,0,1,2,0, 0)    ## less parameters than previous example!
    # outcome_type = 'count'
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 150)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 9)
  
  
  sim_arguments <- list(
    formula = y ~ 1 + sex + type + sex:type,
    fixed = list(sex = list(var_type = 'factor', levels=c('fem','male')),
                 type = list(var_type = 'factor', levels = c('type1', 'type2'))),
    error = list(variance = 0.2),
    sample_size = 250,
    reg_weights = c(0,0,1,2,0, 0)    ## less parameters than previous example!
    # outcome_type = 'count'
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 250)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 7)
  
  sim_arguments <- list(
    formula = y ~ 1 + sex + type ,
    fixed = list(sex = list(var_type = 'factor', levels=c('fem','male')),
                 type = list(var_type = 'factor', levels = c('type1', 'type2', 'type3'))),
    error = list(variance = 0.2),
    sample_size = 250,
    reg_weights = c(0,0,1,2,0, 0)    ## less parameters than previous example!
    # outcome_type = 'count'
  )
  
  expect_equal(nrow(simulate_fixed(data = NULL, sim_arguments)), 250)
  expect_equal(ncol(simulate_fixed(data = NULL, sim_arguments)), 7)
})
