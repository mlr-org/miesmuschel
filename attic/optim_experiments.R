
reticulate::conda_install("r-reticulate", packages = "onnx", pip = TRUE)
reticulate::conda_install("r-reticulate", packages = "onnxruntime", pip = TRUE)

source("optim2.R")
library("bbotk")
library("mlr3")

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
    surrogate_learner = "ranger",  # doesn't matter
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
    surrogate_learner = "bohblrn",
    filter_with_max_budget = TRUE,
    filter_factor_first = ns,
    random_interleave_random = TRUE
  )
}


search_space = objective$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 8, logscale = TRUE, tags = "budget"))
))


os <- imitate_hyperband(search_space, eta = 2)

oi <- setup_oi(objective, 2^13, search_space)



os$optimize(oi)

head(oi$archive$data, 40)


search_space = objective$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
))



os <- imitate_bohb(search_space, eta = 2)

oi <- setup_oi(objective, 2^17, search_space)



os$optimize(oi)

head(oi$archive$data, 40)

library("ggplot2")

oi$archive$data[, trueval := unlist(sapply(oi$archive$data$x_domain, objective_no_noise$fun))]

ggplot(oi$archive$data, aes(x = dob, y = Obj)) + geom_point()
ggplot(oi$archive$data, aes(x = dob + rnorm(length(x)) / 10, y = trueval)) + geom_point()

ggplot(oi$archive$data, aes(x = dob, y = trueval, group = as.factor(dob))) + geom_boxplot()

ggplot(oi$archive$data, aes(x = x, y = y, color = dob)) + geom_point()

ggplot(oi$archive$data, aes(x = dob, y = b, color = y)) + geom_point()

ggplot(oi$archive$data[b == min(b)], aes(x = x, y = y, color = dob)) + geom_point()


search_space = objective$domain$search_space(list(
  x = to_tune(),
  y = to_tune(),
  b = to_tune(p_int(1, 2^10, logscale = TRUE, tags = "budget"))
))


metao <- get_meta_objective(objective, objective, search_space, budget_limit = 2^13)


ss <- get_searchspace(FALSE, FALSE, "rs", FALSE, FALSE)

allss <- CJ(
  include.mu = c(FALSE, TRUE),
  include.batchmethod = c(FALSE, TRUE),
  infill = c("rs", "vario", "all"),
  include.siman = c(FALSE, TRUE)
)

allss <- allss[include.mu | !include.batchmethod][, include.mo := FALSE]

asx <- lapply(seq_len(nrow(allss)), function(i) do.call(get_searchspace, allss[i]))

results <- list()
for (ss in asx) {
  calls <- generate_design_random(ss, 10)$transpose()
  results[[length(results) + 1]] <- lapply(calls, function(conf) {
    conf$mu <- conf$mu %??% 32
    conf$batch_method = conf$batch_method %??% "smashy"
    do.call(metao, conf)
  })
}

metaoc <- get_meta_objective(objective_complex, objective_complex, search_space_complex, budget_limit = 2^13)
allss <- CJ(
  include.mu = c(FALSE, TRUE),
  include.batchmethod = c(FALSE, TRUE),
  infill = c("rs", "vario", "all"),
  include.siman = c(FALSE, TRUE)
)
allss <- allss[include.mu | !include.batchmethod][, include.mo := FALSE]
asx <- lapply(seq_len(nrow(allss)), function(i) do.call(get_searchspace, allss[i]))

results <- list()
for (ss in asx) {
  calls <- generate_design_random(ss, 10)$transpose()
  results[[length(results) + 1]] <- lapply(calls, function(conf) {
    conf$mu <- conf$mu %??% 32
    conf$batch_method = conf$batch_method %??% "smashy"
    do.call(metaoc, conf)
  })
}


