
reticulate::conda_install("r-reticulate", packages = "onnx", pip = TRUE)
reticulate::conda_install("r-reticulate", packages = "onnxruntime", pip = TRUE)

source("load_objectives.R")

imitate_hyperband <- function(search_space, eta = 3) {
  budget_param = search_space$ids(tags = "budget")
  fidelity_steps = floor((search_space$upper[budget_param] - search_space$lower[budget_param]) / log(eta))

  setup_smashy(search_space,
    budget_log_step = log(eta),
    survival_fraction = 1 / eta,
    mu = eta ^ fidelity_steps,
    sample = "random",
    batch_method = "hb",
    random_interleave_fraction = 1,
   ## - mandatory arguments that don't have an effect with interleave fraction == 1
    filter_algorithm = "tournament",  # doesn't matter
    surrogate_learner = lrn("regr.featureless"),  # doesn't matter
    filter_with_max_budget = FALSE,  # doesn't matter
    filter_factor_first = 1,  # doesn't matter
    random_interleave_random = FALSE  # doesn't matter
  )
}

imitate_bohb <- function(search_space, eta = 3, rho = 1 / 3, ns = 64) {

  budget_param = search_space$ids(tags = "budget")
  fidelity_steps = floor((search_space$upper[budget_param] - search_space$lower[budget_param]) / log(eta))

  setup_smashy(search_space,
    budget_log_step = log(eta),
    survival_fraction = 1 / eta,
    mu = eta ^ fidelity_steps,
    sample = "bohb",
    batch_method = "hb",
    random_interleave_fraction = rho,
    filter_algorithm = "tournament",
    surrogate_learner = "bohb",
    filter_with_max_budget = TRUE,
    filter_factor_first = ns,
    random_interleave_random = TRUE
  )
}
