source("./attic/irace_omni.R")

xs = list(budget_log_step = 0.0646, mu = 4.6069, sample = "random", survival_fraction = 0.2767,
  filter_algorithm = "progressive", surrogate_learner = "ranger", filter_with_max_budget = FALSE,  
  filter_factor_first = 2.7849, filter_factor_last = 3.9019,  filter_select_per_tournament = 0.3001, 
  random_interleave_fraction = 0.2, random_interleave_random = FALSE, filter_factor_first.end = 1,
  filter_factor_last.end = 2, filter_select_per_tournament.end = 0.2, random_interleave_fraction.end = 0.1)

meta_search_space = ps(
  budget_log_step = p_dbl(log(2) / 4, log(2) * 4, logscale = TRUE),
  mu = p_int(2, 200, logscale = TRUE),
  sample = p_fct(c("random")),
  survival_fraction = p_dbl(0, 1),
  filter_algorithm = p_fct(c("tournament", "progressive")),
  surrogate_learner = p_fct(list(
    ranger = mlr3::lrn("regr.ranger"),
    knn = mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))),
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

xs = meta_search_space$trafo(xs)


workdir = "./attic/data/"
#cfg = cfgs("rbv2_super", workdir = workdir)
#objective = cfg$get_objective(task = "1040", target_variables = "logloss")

cfg = cfgs("lcbench", workdir = workdir)
objective = cfg$get_objective(task = "3945", target_variables = "val_balanced_accuracy")

# get search space
domain = objective$domain
param_ids = domain$ids()
budget_idx = which(domain$tags %in% c("budget", "fidelity"))
budget_id = param_ids[budget_idx]
budget_lower = domain$params[[budget_idx]]$lower
budget_upper = domain$params[[budget_idx]]$upper
params_to_keep = param_ids[- budget_idx]
search_space = ParamSet$new(domain$params[params_to_keep])
search_space$add(ParamDbl$new(id = budget_id, lower = log(budget_lower), upper = log(budget_upper), tags = "budget"))
domain_tafo = domain$trafo
search_space$trafo = function(x, param_set) {
  if (!is.null(domain_tafo)) x = domain_tafo(x, param_set)
  x[budget_id] = as.integer(exp(x[[budget_id]]))
  x
}

parameter = c(xs, list(objective = objective, search_space = search_space, budget_limit = 10))

do.call(opt_objective, parameter)