
# demo objective

# Define the objective to optimize
# The 'budget' here simulates averaging 'b' samples from a noisy function
objective <- ObjectiveRFun$new(
  fun = function(xs) {
    z <- exp(-xs$x^2 - xs$y^2) + 2 * exp(-(2 - xs$x)^2 - (2 - xs$y)^2)
    z <- z + rnorm(1, sd = 1 / sqrt(xs$b))
    list(Obj = z)
  },
  domain = ps(x = p_dbl(-2, 4), y = p_dbl(-2, 4), b = p_int(1)),
  codomain = ps(Obj = p_dbl(tags = "maximize"))
)


# objective;
# search space: has one parameter tagged 'budget', with 'logscale = TRUE'


opt_objective <- function(objective, max_budget, budget_step, survival_fraction, mu, sample, filter_with_max_budget, filter_pool_first, filter_pool_last, random_interleave_fraction, random_interleave_random) {
  library("checkmate")
  library("miesmuschel")
  library("mlr3learners")


  assert_r6(objective, "Objective")
  assert_number(max_budget, lower = 0)
  assert_number(budget_step, lower = 0)
  assert_number(survival_fraction, lower = 0, upper = 1)
  assert_integerish(mu, lower = 2)
  assert_choice(sample, c("random", "lhs"))
  assert_flag(filter_with_max_budget)
  assert_number(filter_pool_first, lower = 1)
  assert_number(filter_pool_last, lower = 1)
  assert_number(random_interleave_fraction, lower = 0, upper = 1)
  assert_flag(random_interleave_random)
  oi <- OptimInstanceSingleCrit$new(objective,
    terminator = trm("budget", budget = max_budget, aggregate = function(x) sum(exp(x)))
  )

  budget_param = oi$param_set$ids(tags = "budget")
  fidelity_steps = floor((oi$param_set$upper[budget_param] - oi$param_set$lower[budget_param]) / budget_step)
  # suppose: budget_step is 2, budget param goes from 1 to 6
  # we want steps of length 2, and highest step should be 6, so we want to eval with 6, 4, 2
  # --> there are 2 budget_steps. lower bound needs to be adjusted to 6 - 2 (# of budget steps) * 2 (budget step size) --> 2

  oi$param_set$params[[budget_param]]$lower = oi$param_set$upper[budget_param] - fidelity_steps * budget_param

  survivors = max(round(survival_fraction * mu), 1)
  lambda = mu - survivors

  if (lambda < 1) stop("infeasible: no new samples per generation")

  # 1st stample: from filter_pool_first
  # 2nd sample: from filter_pool_first + filter_pool_per_sample - 1
  # 3rd sample: from filter_pool_first + 2 * filter_pool_per_sample - 2
  # .,...
  # nth sample: from filter_pool_first + (n - 1) * filter_pool_per_sample - (n - 1) === ! === filter_pool_last
  # want to set this equal to filter_pool_last
  # (filter_pool_last - filter_pool_first + (n - 1)) / (n - 1)
  # (filter_pool_last - filter_pool_first) / (n - 1) + 1

  if (lambda == 1) {
    filter_pool_per_sample = 0
  } else {
    filter_pool_per_sample = 1 + (filter_pool_last - filter_pool_first) / (lambda - 1)
  }

  surprogfilter = ftr("surprog", surrogate_learner = mlr3::lrn("regr.ranger"),
    filter_pool_first = filter_pool_first, filter_pool_per_sample = filter_pool_per_sample)

  filtor = ftr("maybe", surprogfilter, p = random_interleave_fraction, random_choice = random_interleave_random)

  sampling_fun = switch(sample, random = paradox::generate_design_random, lhs = paradox::generate_design_lhs)

  optimizer = opt("smashy", mu = mu, survival_fraction = survival_fraction, sampling = sampling_fun,
    fidelity_steps = fidelity_steps + 1, filter_with_max_budget = filter_with_max_budget,
    filtor = filtor
  )

  optimizer$opt(oi)
}
