# demo objective

# Define the objective to optimize
# The 'budget' here simulates averaging 'b' samples from a noisy function

# single objective
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), b = p_int(1)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

# single objective
objective_no_noise  <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), b = p_int(1)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)

search_space = objective$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
))

# multi-objective
objective.mo <- ObjectiveRFun$new(
  fun = function(xs) {
    list(
      obj1 = xs$x * sin(xs$y) + rnorm(1, sd = 1 / sqrt(xs$b)),
      obj2 = xs$x * cos(xs$y) + rnorm(1, sd = 1 / sqrt(xs$b))
    )
  },
  domain = ps(x = p_dbl(0, 1), y = p_dbl(0, 2 * pi), b = p_int(1)),
  codomain = ps(obj1 = p_dbl(tags = "maximize"), obj2 = p_dbl(tags = "maximize"))
)

objective.mo_no_noise <- ObjectiveRFun$new(
  fun = function(xs) {
    list(
      obj1 = xs$x * sin(xs$y),
      obj2 = xs$x * cos(xs$y)
    )
  },
  domain = ps(x = p_dbl(0, 1), y = p_dbl(0, 2 * pi), b = p_int(1)),
  codomain = ps(obj1 = p_dbl(tags = "maximize"), obj2 = p_dbl(tags = "maximize"))
)

search_space.mo = objective.mo$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
))






# Example call:
# res <- opt_objective(objective.mo, search_space.mo, budget_limit = 2^13, budget_log_step = log(2), survival_fraction = .5, mu = 20, sample = "random", filter_algorithm = "tournament", surrogate_learner = lrn("regr.ranger"), filter_with_max_budget = FALSE, filter_factor_first = 1, filter_factor_last = 100, filter_select_per_tournament = 1, random_interleave_fraction = 0.1, random_interleave_random = TRUE)

# res <- opt_objective(objective, search_space, budget_limit = 2^13, budget_log_step = log(2), survival_fraction = .5, mu = 20, sample = "random", filter_algorithm = "tournament", surrogate_learner = lrn("regr.ranger"), filter_with_max_budget = FALSE, filter_factor_first = 1, filter_factor_last = 100, filter_select_per_tournament = 1, random_interleave_fraction = 0.1, random_interleave_random = TRUE)




## res <- opt_objective_optimizable(objective.mo, objective.mo_no_noise, search_space.mo, budget_limit = 2^13, budget_log_step = log(2), survival_fraction = .5, mu = 20, sample = "random", filter_algorithm = "tournament", surrogate_learner = lrn("regr.ranger"), filter_with_max_budget = FALSE, filter_factor_first = 1, filter_factor_last = 100, filter_select_per_tournament = 1, random_interleave_fraction = 0.1, random_interleave_random = TRUE, highest_budget_only = TRUE, nadir = c(-1, -1))

## res <- opt_objective_optimizable(objective, objective_no_noise, search_space, budget_limit = 2^13, budget_log_step = log(2), survival_fraction = .5, mu = 20, sample = "random", filter_algorithm = "tournament", surrogate_learner = lrn("regr.ranger"), filter_with_max_budget = FALSE, filter_factor_first = 1, filter_factor_last = 100, filter_select_per_tournament = 1, random_interleave_fraction = 0.1, random_interleave_random = TRUE, highest_budget_only = TRUE)

# Example call for tuning
# numopts = 10
# calls <- generate_design_random(suggested_meta_searchspace, numopts)$transpose()
# ## multiobjective
# res <- lapply(calls, function(ci) mlr3misc::invoke(opt_objective, objective.mo, search_space.mo, budget_limit = 2^13, .args = ci))
# ## singleobjective
# res <- lapply(calls, function(ci) mlr3misc::invoke(opt_objective, objective, search_space, budget_limit = 2^13, .args = ci))


calls <- calls[[1]]

calls

calls$filter_factor_first <- 100
calls$filter_factor_first.end <- 100

calls$filter_factor_last <- 100
calls$filter_factor_last.end <- 100

calls$filter_select_per_tournament <- 1
calls$filter_select_per_tournament.end <- 1

calls$filter_algorithm <- "tournament"

calls$budget_log_step <- .01

prr <- profvis::profvis(res <- lapply(calls, function(ci) mlr3misc::invoke(opt_objective, objective, search_space, budget_limit = 2^13, .args = ci)), interval = .005)

prr
