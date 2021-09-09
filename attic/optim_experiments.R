
reticulate::conda_install("r-reticulate", packages = "onnx", pip = TRUE)
reticulate::conda_install("r-reticulate", packages = "onnxruntime", pip = TRUE)

source("optim2.R")
library("bbotk")
library("mlr3")

source("load_objectives.R")

imitate_hyperband <- function(search_space, eta = 3) {
  budget_param = search_space$ids(tags = "budget")
  fidelity_steps = floor(log(search_space$upper[budget_param] - search_space$lower[budget_param]) / log(eta))

  list(
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
  fidelity_steps = floor(log(search_space$upper[budget_param] - search_space$lower[budget_param]) / log(eta))

  list(
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

profiled <- profvis::profvis({

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


})

# -----

cursur <- surrogates[[1]]

config <- imitate_hyperband(cursur$domain)
optimizer <- optimize_from_surrogate(cursur, 30)

results <- replicate(30, do.call(optimizer, config), simplify = FALSE)

dats <- lapply(results, function(x) x$archive$data)

configbohb <- imitate_bohb(cursur$domain)

resultsbohb <- replicate(30, do.call(optimizer, configbohb), simplify = FALSE)

datsbohb <- lapply(resultsbohb, function(x) x$archive$data)

##

lcbench_3945 <- readRDS("data/lcbench_3945.rds")

lcbench_3945[, budget.expended := cumsum(budget), by = "job.id"]
lcbench_3945[, bestperf := cummin(performance), by = "job.id"]

library("ggplot2")
ggplot(lcbench_3945, aes(x = budget.expended, y = bestperf, color = algorithm,
  group = as.factor(job.id))) + geom_line()

ggplot(lcbench_3945[, .(meanperf = mean(bestperf), job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")],
  aes(x = budget.expended, y = meanperf, color = algorithm,
    group = as.factor(job.id))) + geom_line()


hbe <- readRDS("data/hyperbandemulation2.rds")

lapply(seq_along(hbe), function(x) hbe[[x]][, id := x])


hbel <- rbindlist(hbe)
nn <- cbind(hbel, x = rbindlist(hbel$x_domain))

nn

nn[, budget.expended := cumsum(x.epoch), by = "id"]
nn[, bestperf := cummin(val_cross_entropy), by = "id"]


ggplot(nn, aes(x = budget.expended, y = bestperf,
  group = as.factor(id))) + geom_line()

ggplot(nn[, .(meanperf = mean(bestperf), job.id = id[[1]]),
  by = c( "budget.expended")],
  aes(x = budget.expended, y = meanperf,
    group = as.factor(job.id))) + geom_line()




ggplot() +
  geom_line(data = lcbench_3945[, .(meanperf = mean(bestperf),
    job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")],
  aes(x = budget.expended, y = meanperf, color = algorithm,
    group = as.factor(job.id))) +
  geom_line(data = nn, aes(x = budget.expended, y = bestperf,
    group = as.factor(id)))



dat <- lcbench_3945[, .(meanperf = mean(bestperf), sdperf = sd(bestperf),
  job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")]
nndat <- nn[, .(meanperf = mean(bestperf), sdperf = sd(bestperf), job.id = id[[1]]),
    by = c( "budget.expended")]


log10 <- function(x) log(x, 10)
log10 <- function(x) x


ggplot() +
  geom_line(data = dat,
    aes(x = log10(budget.expended), y = meanperf, color = algorithm,
      group = algorithm)) +
  geom_ribbon(data = dat,
    aes(x = log10(budget.expended), fill = algorithm,
    ymin = meanperf - sdperf / sqrt(30), ymax = meanperf + sdperf / sqrt(30),
    group = algorithm), alpha = 0.3) +
  geom_line(data = nndat,
    aes(x = log10(budget.expended), y = meanperf,
      group = as.factor(job.id))) +
  geom_ribbon(data = nndat,
    aes(x = log10(budget.expended),
    ymin = meanperf - sdperf / sqrt(30), ymax = meanperf + sdperf / sqrt(30)), alpha = 0.3) +
  geom_vline(xintercept = log10(27)) +
  geom_vline(xintercept = log10(27 + 18)) +
  geom_vline(xintercept = log10(27 * 2))


ggplot(data = dat, aes(x = log10(budget.expended), color = algorithm, group = algorithm, y = meanperf)) +
  geom_line() +
  geom_ribbon(aes(ymin = meanperf - sdperf / sqrt(30), ymax = meanperf + sdperf / sqrt(30)),
    alpha = 0.3)



hist(lcbench_3945[
    algorithm == "hpbster_hb" & budget == 2 & budget.expended <= 11610
  ]$performance, breaks = 1000)

hist(nn[x.epoch == 1]$val_cross_entropy, breaks = 1000)

nrow(nn)

nrow(nn[x.epoch == 1])


nrow(lcbench_3945[algorithm == "hpbster_hb" & budget == 2 & budget.expended <= 11610])
