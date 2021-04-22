
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

search_space.mo = objective.mo$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
))

suggested_meta_searchspace = ps(
  budget_log_step = p_dbl(log(2) / 4, log(2) * 4, logscale = TRUE),
  mu = p_int(2, 200, logscale = TRUE),
  sample = p_fct(c("random")),  # we could try lhs, but (1) probably not that important and (2) very slow
  survival_fraction = p_dbl(0, 1),  # values close to 1 may fail depending on mu; somehow interpolate that.
  filter_algorithm = p_fct(c("tournament", "progressive")),
  surrogate_learner = p_fct(list(
    ranger = mlr3::lrn("regr.ranger"),
    knn = mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))),  # try others as well? # the k = 2 is necessary because kknn crashes when k < trainingset size
  filter_with_max_budget = p_lgl(),
  filter_factor_first = p_dbl(1, 100, logscale = TRUE),
  filter_factor_last = p_dbl(1, 100, logscale = TRUE),
  filter_select_per_tournament = p_int(1, 10, logscale = TRUE),
  random_interleave_fraction = p_dbl(0, 1),
  random_interleave_random = p_lgl()
)




opt_objective <- function(objective, search_space, budget_limit, budget_log_step,
    survival_fraction, mu, sample,
    filter_algorithm, surrogate_learner, filter_with_max_budget, filter_factor_first, filter_factor_last, filter_select_per_tournament,
    random_interleave_fraction, random_interleave_random) {
  library("checkmate")
  library("miesmuschel")
  library("mlr3learners")

  # Objective Parameters
  assert_r6(objective, "Objective")  # to optimize, single- or multi-objective
  assert_r6(search_space, "ParamSet")  # search space, has one parameter tagged 'budget', with *** 'logscale = TRUE' ***
  assert_number(budget_limit, lower = 0)  # Total 'budget' to optimize. Not log-transformed.

  # HB Parameters
  assert_number(budget_log_step, lower = 0)  # log() of budget fidelity steps to make. E.g. log(2) for doubling
  assert_int(mu, lower = 2)  # population size
#  assert_number(survival_fraction, lower = 0, upper = 1 - 0.5 / mu)  # fraction of individuals that survive. round(mu * survival_fraction) must be < survival_fraction
  assert_choice(sample, c("random", "lhs"))  # sample points randomly or using LHS. I'm pretty sure this is not very important.

  # Surrogate Options
  assert_choice(filter_algorithm, c("tournament", "progressive"))  # The two implemented filter algorithms
  assert_r6(surrogate_learner, "LearnerRegr")
  # Whether to use surrogate predictions at the largest budget so far evaluated, or at the budget of the last evaluated budget.
  # (This only makes a difference after HB "restarts", i.e. when max-budget configs were already evaluated and HB samples new low-budget individuals.)
  assert_flag(filter_with_max_budget)
  # How big is the pool from which the first individual / of the last individual is sampled from? (Relative to select_per_tournament)
  assert_number(filter_factor_first, lower = 1)
  assert_number(filter_factor_last, lower = 1)
  assert_int(filter_select_per_tournament, lower = 1)  # tournament size, only really used if `filter_algorithm` is "tournament"

  assert_number(random_interleave_fraction, lower = 0, upper = 1)  # fraction of individuals sampled with random interleaving
  assert_flag(random_interleave_random)  # whether the number of random interleaved individuals is drawn from a binomial distribution, or the same each generation

  # We change the lower limit of the budget parameter:
  # suppose: budget_step is 2, budget param goes from 1 to 6
  # we want steps of length 2, and highest step should be 6, so we want to eval with 6, 4, 2
  # --> there are 2 budget_steps. lower bound needs to be adjusted to 6 - 2 (# of budget steps) * 2 (budget step size) --> 2
  budget_param = search_space$ids(tags = "budget")
  fidelity_steps = floor((search_space$upper[budget_param] - search_space$lower[budget_param]) / budget_log_step)
  search_space$params[[budget_param]]$lower = search_space$upper[budget_param] - fidelity_steps * budget_log_step

  survivors = max(round(survival_fraction * mu), 1)
  lambda = mu - survivors
  if (lambda < 1) return("infeasible: no new samples per generation")

  oiclass = if (objective.mo$codomain$length == 1) OptimInstanceSingleCrit else OptimInstanceMultiCrit
  oi <- oiclass$new(objective, search_space,
    terminator = trm("budget", budget = budget_limit, aggregate = function(x) sum(exp(as.numeric(x))))  # budget in archive is in log-scale!
  )

  # scalor: scalarizes multi-objective results. "one": take the single objective. "nondom": nondominated sorting w/ crowding distance tie breaker
  scalor = if (objective.mo$codomain$length == 1) scl("one") else scl("nondom")
  # selector: take the best, according to scalarized objective
  selector = sel("best", scalor)
  # filtor: use surtour or surprog, depending on filter_algorithm config argument
  filtor = switch(filter_algorithm,
    tournament = ftr("surtour", surrogate_learner = surrogate_learner, surrogate_selector = selector,
      filter.per_tournament = filter_select_per_tournament,
      filter.tournament_size = filter_factor_first * filter_select_per_tournament,
      filter.tournament_size_last = filter_factor_last * filter_select_per_tournament
    ),
    progressive = ftr("surprog", surrogate_learner = surrogate_learner, surrogate_selector = selector,
      filter.pool_factor = filter_factor_first,
      filter.pool_factor_last = filter_factor_last
    )
  )

  interleaving_filtor = ftr("maybe", filtor, p = random_interleave_fraction, random_choice = random_interleave_random)

  sampling_fun = switch(sample, random = paradox::generate_design_random, lhs = paradox::generate_design_lhs)

  optimizer = opt("sumohb", filtor = interleaving_filtor, selector = selector,
    mu = mu, survival_fraction = survival_fraction, sampling = sampling_fun,
    fidelity_steps = fidelity_steps + 1, filter_with_max_budget = filter_with_max_budget
  )

  optimizer$optimize(oi)
}


# Example call:
# res <- opt_objective(objective.mo, search_space.mo, budget_limit = 2^13, budget_log_step = log(2), survival_fraction = .5, mu = 20, sample = "random", filter_algorithm = "tournament", surrogate_learner = lrn("regr.ranger"), filter_with_max_budget = FALSE, filter_factor_first = 1, filter_factor_last = 100, filter_select_per_tournament = 1, random_interleave_fraction = 0.1, random_interleave_random = TRUE)

# Example call for tuning
# numopts = 10
# calls <- generate_design_random(suggested_meta_searchspace, numopts)$transpose()
# ## multiobjective
# res <- lapply(calls, function(ci) mlr3misc::invoke(opt_objective, objective.mo, search_space.mo, budget_limit = 2^13, .args = ci))
# ## singleobjective
# res <- lapply(calls, function(ci) mlr3misc::invoke(opt_objective, objective, search_space, budget_limit = 2^13, .args = ci))
