
reticulate::conda_install("r-reticulate", packages = "onnx", pip = TRUE)
reticulate::conda_install("r-reticulate", packages = "onnxruntime", pip = TRUE)

source("optim2.R")
library("bbotk")
library("mlr3")

source("load_objectives2.R")



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


metaoc <- get_meta_objective(objective_complex, objective_complex, search_space_complex, budget_limit = 2^15)

profiled <- profvis::profvis({

mm <- do.call(metaoc, imitate_bohb(search_space_complex, budget_is_logscale = TRUE))

})

profiled
# -----

cursur <- surrogates[[1]]  # lcbench 3945
cursur <- surrogates[[37]]  # rbv2_super 46

config <- imitate_hyperband(cursur$domain)
optimizer <- optimize_from_surrogate(cursur, 30)

results <- replicate(30, do.call(optimizer, config), simplify = FALSE)

dats <- lapply(results, function(x) x$archive$data)

configbohb <- imitate_bohb(cursur$domain)

resultsbohb <- replicate(30, do.call(optimizer, configbohb), simplify = FALSE)

datsbohb <- lapply(resultsbohb, function(x) x$archive$data)




##








lcbench_3945 <- readRDS("data/lcbench_3945.rds")

lcbench_3945 <- readRDS("data/rbv2_super_46.rds")



lcbench_3945[, budget.expended := cumsum(budget), by = "job.id"]
lcbench_3945[, bestperf := cummin(performance), by = "job.id"]

library("ggplot2")
ggplot(lcbench_3945, aes(x = budget.expended, y = bestperf, color = algorithm,
  group = as.factor(job.id))) + geom_line()

ggplot(lcbench_3945[, .(meanperf = mean(bestperf), job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")],
  aes(x = budget.expended, y = meanperf, color = algorithm,
    group = as.factor(job.id))) + geom_line()


hbe <- c(list(hb1 = readRDS("data/hyperbandemulation2.rds")), readRDS("data/tryouts_repaired2.rds"))

hbe <- list(hb = readRDS("data/hbrbv2.rds"), bohb = readRDS("data/bohbrbv2_repair.rds"))

hbe <- list(hb = readRDS("data/hbrbv2_repair2.rds"), bohb = readRDS("data/bohbrbv2_repair2.rds"))


hbe <- sapply(names(hbe), function(y) lapply(Filter(Negate(is.null), hbe[[y]]), function(x) x[, algorithm := y]), simplify = FALSE)

hbe <- unlist(hbe, recursive = FALSE)

invisible(lapply(seq_along(hbe), function(x) hbe[[x]][, id := x]))

hbel <- rbindlist(hbe)
nn <- cbind(hbel, x = rbindlist(hbel$x_domain))

nn

if ("x.trainsize" %in% names(nn)) {
  nn$x.epoch <- nn$x.trainsize
  nn$val_cross_entropy <- nn$logloss
}

nn[, budget.expended := cumsum(x.epoch), by = c("id", "algorithm")]
nn[, bestperf := cummin(val_cross_entropy), by = c("id", "algorithm")]


ggplot(nn, aes(x = log10(budget.expended), y = bestperf,
  group = paste(id, algorithm), color = algorithm)) + geom_line()

ggplot(nn, aes(x = log10(budget.expended), y = bestperf,
  group = paste(id, algorithm), color = (id == 43))) + geom_line()

ggplot(nn[, .(meanperf = mean(bestperf), job.id = id[[1]]),
  by = c("budget.expended", "algorithm")],
  aes(x = budget.expended, y = meanperf,
    group = as.factor(job.id))) + geom_line()

ggplot(nn[, .(meanperf = mean(bestperf), job.id = id[[1]]),
  by = c("budget.expended", "algorithm")],
  aes(x = log10(budget.expended), y = meanperf,
    group = as.factor(job.id))) + geom_line()




ggplot() +
  geom_line(data = lcbench_3945[, .(meanperf = mean(bestperf),
    job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")],
  aes(x = budget.expended, y = meanperf, color = algorithm,
    group = as.factor(job.id))) +
  geom_line(data = nn, aes(x = budget.expended, y = bestperf,
    group = as.factor(id)))


# mean +- sd
dat <- lcbench_3945[, .(meanperf = mean(bestperf),
  ub = mean(bestperf) + sd(bestperf) / sqrt(.N),
  lb = mean(bestperf) - sd(bestperf) / sqrt(.N),
  job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")]
nndat <- nn[, .(meanperf = mean(bestperf),
  ub = mean(bestperf) - sd(bestperf) / sqrt(.N),
  lb = mean(bestperf) + sd(bestperf) / sqrt(.N),
  job.id = id[[1]]),
    by = c( "budget.expended", "algorithm")]



qbinom(.975, 30, .5)
qbinom(.025, 30, .5)
qbinom(.5, 30, .5)




dat <- lcbench_3945[, .(meanperf = median(bestperf),
  lb = sort(bestperf)[[10]],
  ub = sort(bestperf)[[20]],
  job.id = job.id[[1]]),
  by = c( "budget.expended", "algorithm")]
nndat <- nn[, .(meanperf = median(bestperf),
  lb = sort(bestperf)[[10]],
  ub = sort(bestperf)[[20]],
  job.id = id[[1]]),
    by = c( "budget.expended", "algorithm")]


log10 <- function(x) log(x, 10)
log10 <- function(x) x


ggplot() +# xlim(0, 13000) +
  ylim(0, .25) +
  geom_line(data = dat[budget.expended < 1200],
    aes(x = log10(budget.expended), y = meanperf, color = algorithm,
      group = algorithm)) +
  geom_ribbon(data = dat[budget.expended < 1200],
    aes(x = log10(budget.expended), fill = algorithm,
    ymin = lb, ymax = ub,
    group = algorithm), alpha = 0.3) +
  geom_line(data = nndat,
    aes(x = log10(budget.expended), y = meanperf,
      color = algorithm)) +
  geom_ribbon(data = nndat,
    aes(x = log10(budget.expended),
      ymin = lb, ymax = ub,
      fill = algorithm), alpha = 0.3) +
  geom_vline(xintercept = log10(1)) +
  geom_vline(xintercept = log10(2)) +
  geom_vline(xintercept = log10(3)) +
  geom_vline(xintercept = log10(4)) +
  geom_vline(xintercept = log10(70/9)) +
  geom_vline(xintercept = log10(142/9)) +
  geom_vline(xintercept = log10(142/9 * 2))


ggplot() +# xlim(0, 13000) +
  geom_line(data = dat,
    aes(x = log10(budget.expended), y = meanperf, color = algorithm,
      group = algorithm)) +
  geom_ribbon(data = dat,
    aes(x = log10(budget.expended), fill = algorithm,
    ymin = lb, ymax = ub,
    group = algorithm), alpha = 0.3) +
  geom_line(data = nndat,
    aes(x = log10(budget.expended), y = meanperf,
      color = algorithm)) +
  geom_ribbon(data = nndat,
    aes(x = log10(budget.expended),
      ymin = lb, ymax = ub,
      fill = algorithm), alpha = 0.3) +
  geom_vline(xintercept = log10(27 * 2)) +
  geom_vline(xintercept = log10(27 * 2 + 6 * 9)) +
  geom_vline(xintercept = log10(27 * 2 + 6 * 9 + 17 * 3)) +
  geom_vline(xintercept = log10(27 * 2 + 6 * 9 + 17 * 3 + 52)) +
  geom_vline(xintercept = log10(824)) +
  geom_vline(xintercept = log10(824 + 27 * 2 + 6 * 9 + 17 * 3 + 52)) +
  geom_vline(xintercept = log10(824 * 2))


ggplot(data = dat, aes(x = log10(budget.expended), color = algorithm, group = algorithm, y = meanperf)) +
  geom_line() +
  geom_ribbon(aes(ymin = meanperf - sdperf / sqrt(30), ymax = meanperf + sdperf / sqrt(30)),
    alpha = 0.3)



ggplot() +
  geom_point(dat = lcbench_3945[, .SD[40], by = "job.id"],
    aes(x = algorithm, y = bestperf))

started <- lcbench_3945[, .SD[40], by = "job.id"]
ended <- lcbench_3945[, .SD[1062], by = "job.id"]

ggplot() +
  geom_point(dat = rbind(
      nn[, .SD[40], by = "id"][, .(algorithm, performance = bestperf)],
      started[, .(algorithm, performance = bestperf)]
    ), aes(x = algorithm, y = performance)
  )

t.test(
  started[algorithm == "hpbster_hb", performance],
  started[algorithm == "hpbster_bohb", performance]
)

data.table(we = head(nn$x.epoch, 1090), they = head(lcbench_3945$budget, 1090))

plot(head(nn$x.epoch, 1090), type = "l")

plot(head(lcbench_3945$budget, 1090), type = "l")

plot(head(nn$budget.expended, 1090), type = "l")
lines(head(lcbench_3945$budget.expended, 1090))

max(nn$budget.expended)

max(which(head(lcbench_3945$budget.expended, 1090) < 12556))

lcbench_3945[1062]



hist(lcbench_3945[
    algorithm == "hpbster_hb" & budget == 2 & budget.expended <= 11610
  ]$performance, breaks = 1000)

hist(nn[x.epoch == 1]$val_cross_entropy, breaks = 1000)

nrow(nn)

nrow(nn[x.epoch == 1])



nrow(lcbench_3945[algorithm == "hpbster_hb" & budget == 2 & budget.expended <= 11610])

ggplot() +
  geom_point(dat = rbind(
      nn[, .SD[1090], by = "id"][, .(algorithm, performance = bestperf)],
      ended[, .(algorithm, performance = bestperf)]
    ), aes(x = algorithm, y = performance)
  )


wassampled <- function(budget) {
  Reduce(function(x,y) {
    if (y == -1) {
      1
    } else if (y == 1) {
      0
    } else {
      x
    }
  }, sign(diff(budget)), init = 1, accumulate = TRUE)
}

head(cbind(nn, nn[, wassampled(x.epoch)]), 100)[, .(x.epoch, V2)]

nn[, wassampled := wassampled(x.epoch), by = c("id", "algorithm")]

lcbench_3945[, wassampled := wassampled(budget), by = c("job.id", "algorithm")]


ggplot() +
  geom_point(data = lcbench_3945[wassampled == 1][,
    x := seq_along(algorithm), by = "job.id"][x <= 1062],
    aes(x = x, y = performance, color = as.factor(budget)), alpha = 0.3)

ggplot() +
  geom_point(data = lcbench_3945[wassampled == 1][,
    x := seq_along(algorithm), by = "job.id"][x <= 106],
    aes(x = x, y = performance, color = as.factor(budget)), alpha = 0.3)


ggplot() +
  geom_point(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][x <= 1062],
    aes(x = x, y = val_cross_entropy, color = as.factor(x.epoch)), alpha = 0.3)

ggplot() +
  geom_point(data = nn[wassampled == 1][algorithm == "bohb"][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][x <= 106],
    aes(x = x, y = val_cross_entropy, color = as.factor(x.learner)), alpha = 0.3)


ggplot() +
  geom_point(data = nn[wassampled == 1][algorithm == "bohb"][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][x <= 106],
    aes(x = x, y = val_cross_entropy, color = as.factor(x.epoch)), alpha = 0.3)

ggplot() +
  geom_point(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][x <= 106],
    aes(x = x, y = val_cross_entropy, color = as.factor(x.epoch)), alpha = 0.3)

ggplot() +
  geom_point(data = nn[wassampled == 1][algorithm == "bohb"][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][x <= 106],
    aes(x = x.glmnet.alpha, y = log(x.glmnet.s)), alpha = 0.3)

colnames(nn)


ggplot() +
  geom_point(data = nn[wassampled == 1][algorithm == "bohb"][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][x <= 5000],
    aes(x = x, y = val_cross_entropy, color = as.factor(x.epoch)), alpha = 0.3)

ggplot() +
  geom_point(data = lcbench_3945[wassampled == 1][,
    x := seq_along(algorithm), by = c("job.id")][x <= 5000],
    aes(x = x, y = performance, color = as.factor(budget)), alpha = 0.3)



nn[wassampled == 1][algorithm == "bohb"][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][, max(x)]


colnames(nn)


ggplot() +
  geom_point(data = lcbench_3945[wassampled == 1 & algorithm == "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x <= 717],
    aes(x = x, y = performance, color = algorithm), alpha = 0.3) +
  geom_point(data = nn[wassampled == 1 & algorithm %in% c("hb", "hb1")][,
    x := seq_along(x.epoch), by = c("id", "algorithm")],
    aes(x = x, y = val_cross_entropy, color = algorithm), alpha = 0.3)


ggplot() +
  geom_histogram(data = lcbench_3945[wassampled == 1 & algorithm == "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x <= 717],
    aes(x = log(performance), color = algorithm), alpha = 0.3, position = "dodge") +
  geom_histogram(data = nn[wassampled == 1 & algorithm %in% c("hb", "hb1")][,
    x := seq_along(x.epoch), by = c("id", "algorithm")],
    aes(x = log(val_cross_entropy), color = algorithm), alpha = 0.3, position = "dodge")


ggplot() +
  geom_histogram(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x <= 717],
    aes(x = log(performance), color = algorithm), alpha = 0.3, position = "dodge") +
  geom_histogram(data = nn[wassampled == 1 & !algorithm %in% c("hb", "hb1")][,
    x := seq_along(x.epoch), by = c("id", "algorithm")],
    aes(x = log(val_cross_entropy), color = algorithm), alpha = 0.3, position = "dodge")



ggplot() +
  geom_histogram(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x <= 717],
    aes(x = log(performance), color = algorithm), alpha = 0.3, position = "dodge") +
  geom_histogram(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")],
    aes(x = log(val_cross_entropy), color = algorithm), alpha = 0.3, position = "dodge")


ggplot() +
  geom_histogram(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x <= 717][budget.expended < 100],
    aes(x = log(performance), color = algorithm), alpha = 0.3, position = "dodge") +
  geom_histogram(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][budget.expended < 100],
    aes(x = log(val_cross_entropy), color = algorithm), alpha = 0.3, position = "dodge")


ggplot() +
  geom_violin(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x <= 717][budget.expended < 100],
    aes(x = as.factor(x), y = log(performance), color = algorithm), alpha = 0.3) +
  geom_violin(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][budget.expended < 100],
    aes(x = as.factor(x), y = log(val_cross_entropy), color = algorithm), alpha = 0.3)

ggplot() +
  geom_point(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x >= 717][budget.expended > 12500],
    aes(x = (x), y = log(performance), color = algorithm), alpha = 0.3) +
  geom_point(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][budget.expended > 12000],
    aes(x = (x), y = log(val_cross_entropy), color = algorithm), alpha = 0.3)



ggplot() +
  geom_point(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x >= 2000][budget.expended > 12500],
    aes(x = (x), y = log(performance), color = as.factor(budget)), alpha = 0.3)


ggplot() +
  geom_histogram(data = lcbench_3945[wassampled == 1 & algorithm != "hpbster_hb"][,
    x := seq_along(algorithm), by = "job.id"][x >= 2000][budget.expended > 12500],
    aes(x = log(performance), color = as.factor(budget)), alpha = 0.3,
    position = "dodge")


ggplot() +
  geom_point(data = nn[wassampled == 1][,
    x := seq_along(x.epoch), by = c("id", "algorithm")][budget.expended > 10000][
                algorithm == "bohb"],
    aes(x = x, y = log(val_cross_entropy), color = as.factor(x.epoch)), alpha = 0.3)




nn

lcbench_3945


wassampled(c(1, 1, 1, 2, 2, 3, 3, 2, 2, 3, 2, 3))




##


dat <- readRDS("experiments/data/run_0000000_hb______m0__hb_____rsinfill_______.rds")$oi$archive$data

plot(dat$yval)
plot(dat$mu)

plot(dat$mu, dat$yval)




