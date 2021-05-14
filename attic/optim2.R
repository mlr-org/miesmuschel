

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

  filter_factor_first.end = p_dbl(1, 100, logscale = TRUE),
  filter_factor_last.end = p_dbl(1, 100, logscale = TRUE),
  filter_select_per_tournament.end = p_int(1, 10, logscale = TRUE),
  random_interleave_fraction.end = p_dbl(0, 1),

  random_interleave_random = p_lgl()
)


# for simulated annealing
get_progress <- function(inst) {
  prog = inst$terminator$status(inst$archive)
  if (prog["max_steps"] <= 0) return(1)
  min(1, max(0, prog["current_steps"] / prog["max_steps"]))
}

# interpolate context param value
interpolate_cpv <- function(beginning, end, logscale = FALSE, round = FALSE) {
  ContextPV(function(inst) {
    prog = get_progress(inst)
    result = if (logscale) {
      exp(prog * log(end) + (1 - prog) * log(beginning))
    } else {
      prog * end + (1 - prog) * beginning
    }
    if (round) result = round(result)
    result
  }, beginning, end, get_progress, logscale, round)
}

opt_objective <- function(objective, search_space, budget_limit, budget_log_step,
    survival_fraction, mu, sample,
    filter_algorithm, surrogate_learner, filter_with_max_budget,
    filter_factor_first, filter_factor_last, filter_select_per_tournament, random_interleave_fraction,
    filter_factor_first.end = filter_factor_first, filter_factor_last.end = filter_factor_last,
    filter_select_per_tournament.end = filter_select_per_tournament, random_interleave_fraction.end = random_interleave_fraction,
    random_interleave_random) {
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
  assert_number(filter_factor_first.end, lower = 1)
  assert_number(filter_factor_last, lower = 1)
  assert_number(filter_factor_last.end, lower = 1)
  assert_int(filter_select_per_tournament, lower = 1)  # tournament size, only really used if `filter_algorithm` is "tournament"
  assert_int(filter_select_per_tournament.end, lower = 1)

  assert_number(random_interleave_fraction, lower = 0, upper = 1)  # fraction of individuals sampled with random interleaving
  assert_number(random_interleave_fraction.end, lower = 0, upper = 1)  # fraction of individuals sampled with random interleaving
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
  if (lambda < 1) {
    # return("infeasible: no new samples per generation")
    survival_fraction <- 1 - 1 / mu
  }

  oiclass = if (objective$codomain$length == 1) OptimInstanceSingleCrit else OptimInstanceMultiCrit
  oi <- oiclass$new(objective, search_space,
    terminator = trm("budget", budget = budget_limit, aggregate = function(x) sum(exp(as.numeric(x))))  # budget in archive is in log-scale!
  )

  # scalor: scalarizes multi-objective results. "one": take the single objective. "nondom": nondominated sorting w/ crowding distance tie breaker
  scalor = if (objective$codomain$length == 1) scl("one") else scl("nondom")
  # selector: take the best, according to scalarized objective
  selector = sel("best", scalor)
  # filtor: use surtour or surprog, depending on filter_algorithm config argument
  filtor = switch(filter_algorithm,
    tournament = ftr("surtour", surrogate_learner = surrogate_learner, surrogate_selector = selector,
      filter.per_tournament = interpolate_cpv(filter_select_per_tournament, filter_select_per_tournament.end, logscale = TRUE, round = TRUE),
      filter.tournament_size = interpolate_cpv(
        filter_factor_first * filter_select_per_tournament,
        filter_factor_first.end * filter_select_per_tournament.end,
        logscale = TRUE
      ),
      filter.tournament_size_last = interpolate_cpv(
        filter_factor_last * filter_select_per_tournament,
        filter_factor_last.end * filter_select_per_tournament.end,
        logscale = TRUE
      )
    ),
    progressive = ftr("surprog", surrogate_learner = surrogate_learner, surrogate_selector = selector,
      filter.pool_factor = interpolate_cpv(filter_factor_first, filter_factor_first.end, logscale = TRUE),
      filter.pool_factor_last = interpolate_cpv(filter_factor_last, filter_factor_last.end, logscale = TRUE)
    )
  )

  random_interleave_fraction_cpv  = interpolate_cpv(random_interleave_fraction, random_interleave_fraction.end)  # linear scale

  interleaving_filtor = ftr("maybe", filtor, p = random_interleave_fraction_cpv, random_choice = random_interleave_random)

  sampling_fun = switch(sample, random = paradox::generate_design_random, lhs = paradox::generate_design_lhs)

  optimizer = opt("sumohb", filtor = interleaving_filtor, selector = selector,
    mu = mu, survival_fraction = survival_fraction, sampling = sampling_fun,
    fidelity_steps = fidelity_steps + 1, filter_with_max_budget = filter_with_max_budget
  )

  optimizer$optimize(oi)
  oi
}

opt_objective_optimizable <- function(objective, test_objective, search_space, ..., highest_budget_only, nadir = 0) {

  assert_flag(highest_budget_only)

  multiobjective <- objective$codomain$length > 1

  oi <- opt_objective(objective, search_space, ...)

  om <- oi$objective_multiplicator

  archdata <- oi$archive$data
  budgetparam <- oi$search_space$ids(tags = "budget")
  if (highest_budget_only) {
    archdata <- archdata[get(budgetparam) == max(get(budgetparam))]
  }

  objvalues <- archdata[, names(om), with = FALSE]
  objmat <- as.matrix(sweep(objvalues, 2, om, `*`)) * -1

  ndo <- miesmuschel::order_nondominated(objmat)$fronts
  selarch <- ndo == 1

  if (!multiobjective) {
    selarch <- which(selarch)[[1]]
  }

  design <- archdata[selarch, x_domain]

  fitnesses <- as.matrix(sweep(test_objective$eval_many(design)[, test_objective$codomain$ids(), with = FALSE], 2, om, `*`)) * -1

  if (multiobjective) {
    miesmuschel:::domhv(fitnesses, nadir = nadir)
  } else {
    fitnesses
  }
}



