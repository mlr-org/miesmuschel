

# Want: map from <perf value> to <quantile>
# <quantile> should be uniformly accurate on the log scale? want 1/q uniformly accurate?
# What would things look like on linear scale?

# Confidence interval for percentile estimator: https://stats.stackexchange.com/a/284970
# nonparametric 1-alpha estimator is between X(l) and X(u) where
# Binom(u-1; n, q) - B(l-1; n, q) >= 1-alpha --> prob that Binom(n, q) has l .. u-1,
# i.e. that the X right below the true F^{-1}(q) is X(l) or .. or X(u-1).
# Choose based on Binom(n, q) with symmetric probability:
# qbinom(log(probability of error / 2), n, q, lower.tail = TRUE/FALSE, log.p = TRUE)

# P(qTRUE < X(l)) = dbinom(l - 1, n, q) should be <= <prob of error>/2
#   --> l - 1 = qbinom(log(<prob of error>/2), n, q, lower.tail = TRUE, log.p = TRUE) - 1 (!)
#   --> l = qbinom(log(<prob of error>/2), n, q, lower.tail = TRUE, log.p = TRUE)
# P(qTRUE > X(u)) = 1 - dbinom(u - 1, n, q) should be <= <prob of error>/2
#   --> dbinom(u - 1, n, q) should be >= 1 - <prob of error>/2
#   --> u - 1 = qbinom(log(<prob of error>/2), n, q, lower.tail = FALSE, log.p = TRUE) (?? relatively sure about the -1 here)
#   --> u = qbinom(log(<prob of error>/2), n, q, lower.tail = FALSE, log.p = TRUE) + 1


# given <p> :: log(<prob of error>/2)
# --> quantile q is between x[[qbinom(<p>, n, q, lower.tail = TRUE, log.p = TRUE)]] and x[[qbinom(<p>, n, q, lower.tail = FALSE, log.p = TRUE) + 1]]

# F^{-1}(x) -> linearly interpolate between two known q -> error is mostly error in known q, with approximation error ~ F^{-1}'' * delta_q
# --> freeze when
#     * x[[qbinom(<p>, n, q, lower.tail = FALSE, log.p = TRUE) + 1]] - x[[qbinom(<p>, n, q, lower.tail = TRUE, log.p = TRUE)]]  < delta(q)
#     * x[[qbinom(<p>, n, q, lower.tail = TRUE, log.p = TRUE)]] > x[[qbinom(<p>, n, q - delta(q), lower.tail = FALSE, log.p = TRUE) + 1]]
#       <==> qbinom(<p>, n, q, lower.tail = TRUE, log.p = TRUE) > qbinom(<p>, n, q - delta(q), lower.tail = FALSE, log.p = TRUE) + 1


# delta(q) such that 1 / q - 1 / (q + delta(q)) ~ DELTA --> delta(q) = 1 / (1 / q - DELTA) - q = (1 - 1 + DELTA q) / (1 / q - DELTA) = DELTA q^2 / (1 - DELTA q) ~ DELTA q^2









# Random search on uniform distribution:
# P( min_{i 1 .. n}(X_i) < q ) = 1 - P(min > q) = 1 - P(X_i > q)^n = 1 - (1 - q)^n
# distribution function: d/dq (1 - (1 - q)^n) = n * (1 - q)^(n - 1)

# for large n:
# q --> x / n => P = 1 - (1 - x/n)^n ~ 1 - exp(-x)
# --> 1 - (1 - q)^n = 1 - (1 - n * q / n)^n ~ 1 - exp(- n * q)

# Mean: 1 / (1 + n)
# Sd: sqrt( integral(n (1 - q)^(n-1) q^2) - mean^2 ) = sqrt(2/((n+1)(n+2)) - mean^2) =
#   Sqrt(n / ( (1 + n)^2 (2 + n) )) = 1 / (1 + n) * sqrt(n / (2 + n))


# with p(x < q) ~ 1 - exp(-n q), q ~ n exp(-n q) we get likelihood product_{q_i} n exp(-n q_i) --> log likelihood -n q_i + log(n)
# --> maximum at n = number of obs / sum(q_i) = 1 / mean(q_i).  <==> 1/n = mean(q_i).
#
# what is the distribution of 1/n?
# what is the distribution of 1/x, if x ~ exp(n)? "inverse exponential", cdf exp(-n / q), pdf 1/q^2 n exp(-n / q).
# what is the distribution of 1 / mean(q_i)?  "inverse gamma", InvGamma(alpha = <numb of obs>, beta = <num of obs> * n)
#                                              -> mean n / (1 - <num of obs>^2); s.d. <num of obs> * n / ((<num of obs> - 1) * sqrt(<num of obs> - 2))
#                                              -> sd / mean = 1 / sqrt(<num of obs> - 2)
# This is if n, i.e. the equivalent randomsearch power, is constant.
# - what if it depends on the problem? Does that even mean anything?
# - what if we care about log(n), not n?

# log(1/x) --> distributed with *Gumbel Dist*, beta = 1, mu = log(n)
#          --> exp(mean of log(1/x) - euler mascheroni const) estimates n, with uncertainty factor exp(pi/sqrt(6)) ~ 3.6


# How to compare optimization runs?
# - average time it would take random search to reach a similar / better value?
# - absolute accuracy?
# - log of something?
#   - averaging logs implies multiplicative utility: needing having twice as many evals on problem A, half as many on problem B has the same value. Using non-log implies linear utility: the total number of evals needed to get overall similar performance.
#
# Problem: using mean of 1/n is very skewed, using logs much less so
# --> going to use log without a good justification except that it will probably work...

# qi <- QuantileInfo(deltafn = function(q) min(0.1 * q, 1e-3), quantile.min = .5)



# dd <- readRDS("/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/rbv2_super/dicts.rds")
# lapply(dd, function(x) rm(list = "x", envir = environment(x$trafo)))
# saveRDS(dd, "/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/rbv2_super/dicts_compressed.rds")

options(width=170)

options(warn = 1)

library("mfsurrogates2")
library("data.table")
workdir <- "/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates"

# lapply(cfgs()$keys(), function(x) cfgs(x, workdir = workdir)$setup())

instances <- readRDS("~/instances.rds")


instances[cfg == "lcbench", target := "val_cross_entropy"]
instances[cfg == "lcbench", multiplier := 1]
instances[cfg == "rbv2_super", target := "logloss"]
instances[cfg == "rbv2_super", multiplier := 1]

tinst <- instances[test == FALSE]

i <- 3
tinstnao <- tinst[i]
problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)

i <- 37

tinstnao <- tinst[i]
problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)

i <- 10



library("parallelMap")





ll <- lapply(seq_len(nrow(tinst)), function(i) {
  tinstnao <- tinst[i]
  problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)
})




# get surrogate training data
# cfgs(tinst[10]$cfg, workdir = workdir)$setup(data=TRUE)

traindata <- cfgs(tinstnao$cfg, workdir = workdir)$data



i <- 37

tinstnao <- tinst[i]

tinst

cx <- cfgs(tinstnao$cfg, workdir = workdir)
rawdata <- readRDS(cx$data_path)
rbdict <- readRDS("/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/rbv2_super/dicts_compressed.rds")

problem <- cx$get_objective(tinstnao$level, target_variables = tinstnao$target)
randomval <- rawdata[sample(which(get(cx$task_col) == tinstnao$level), 100000)]
# randomval <- rawdata[sample(which(get(cx$task_col) == tinstnao$level & repl == 10), 10000)]
# randomval <- rawdata[get(cx$task_col) == tinstnao$level & repl == 10]
# randomval <- rawdata[repl == 10]
randomvalin <- randomval[, problem$domain$ids(), with = FALSE]


._ <- lapply(names(randomvalin), function(x) { if (is.factor(randomvalin[[x]])) randomvalin[[x]] <<- as.character(randomvalin[[x]]) ; NULL})
randomvalin$repl <- as.integer(randomvalin$repl)

results <- problem$eval_dt(randomvalin)

trueres <- randomval[, names(results), with = FALSE]
sampled <- as.data.table(results)

plot((trueres$logloss), (sampled$logloss), pch = ".")

plot(sort(trueres$logloss), sort(sampled$logloss[!is.na(trueres$logloss)]), pch = ".")

abline(0, 1)

abline(0, 1)
min(trueres$logloss, na.rm = TRUE)
min(sampled$logloss, na.rm = TRUE)


plot(log(trueres$logloss[!is.na(trueres$logloss)]), log(sampled$logloss[!is.na(trueres$logloss)]))
abline(0, 1)


plot(sort(log(trueres$logloss[!is.na(trueres$logloss)])), sort(log(sampled$logloss[!is.na(trueres$logloss)])), pch = ".")
abline(0, 1)

plot(log(seq_len(sum(!is.na(trueres$logloss)))), sort(log(trueres$logloss[!is.na(trueres$logloss)])), pch = ".")
plot(log(seq_len(sum(!is.na(trueres$logloss)))), sort((trueres$logloss[!is.na(trueres$logloss)])), pch = ".")

plot(log(seq_len(sum(1264452))), head(sort((trueres$logloss[!is.na(trueres$logloss)])), 1264452), pch = ".")

points(log(seq_len(sum(!is.na(trueres$logloss)))), sort(log(sampled$logloss[!is.na(trueres$logloss)])), pch = ".")
points(log(seq_len(sum(!is.na(trueres$logloss)))), sort((sampled$logloss[!is.na(trueres$logloss)])), pch = ".")

points(log(seq_len(sum(1264452))), head(sort((sampled$logloss[!is.na(trueres$logloss)])), 1264452), pch = ".")

plot(exp(-trueres$logloss), exp(-sampled$logloss), pch = 'o')

plot(trueres$timetrain, sampled$timetrain)
plot(log(trueres$timetrain), log(sampled$timetrain))




scaledtarget <- mfsurrogates2:::scale_sigmoid(rawdata[[tinstnao$target]])

min(scaledtarget$trafo(rawdata[[tinstnao$target]]))
median(scaledtarget$trafo(rawdata[[tinstnao$target]]))

abline(min(rawdata[[tinstnao$target]]), 0)
abline(median(rawdata[[tinstnao$target]]), 0)

min(scaledtarget$retrafo(rawdata[[tinstnao$target]]))
median(scaledtarget$retrafo(rawdata[[tinstnao$target]]))

lcb <- readRDS("/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/lcbench/data.rds")
names(lcb)

lcb[[tinstnao$target]]
list.files("/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/lcbench/")
lcbdicts <- readRDS("/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/lcbench/dicts.rds")

min(lcb[[tinstnao$target]])
median(lcb[[tinstnao$target]])

quantile(lcb[[tinstnao$target]], c(0, .5, 1))
lcbdicts[[tinstnao$target]]$trafo(quantile(lcb[[tinstnao$target]], c(0, .5, 1)))
lcbdicts[[tinstnao$target]]$retrafo(quantile(lcb[[tinstnao$target]], c(0, .5, 1)))

tinstnao





tdtest <- traindata$ytrain[traindata$xtrain$task_id == "458" & traindata$xtrain$trainsize == 1, ]

head(tdtest)


problem$eval_dt(head(traindata$xtrain[task_id == "458" & trainsize == 1], 1000))



i <- 36
tinstnao <- tinst[i]   # task 458
problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)

xt <- traindata$xtrain[task_id == "458" & trainsize == 1, problem$domain$ids(), with = FALSE]
tdtest <- traindata$ytrain[traindata$xtrain$task_id == "458" & traindata$xtrain$trainsize == 1, ]

# sample 1000 trainingset items w/ trainsize 1
srows <- sample.int(nrow(xt), 1000)
xt <- xt[srows]

# transform
des <- paradox::Design$new(problem$domain, xt, remove_dupl = FALSE)
manies <- des$transpose(filter_na = FALSE)

plot(problem$eval_many(manies)$mmce, tdtest[srows, "mmce"])




str(tdtest)


tdtest


traindata[trainsize == 1, .N, by = task_id][order(N)]



# do vectorized trafos work?

  tinstnao <- tinst[i]

  problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)

  budgetparams <- problem$domain$ids(tags = "budget")
  budgetparamval <- problem$domain$upper[[budgetparams]]

  design <- paradox::generate_design_random(problem$domain, 1e3)

  design2 <- design$clone(deep = TRUE)

  # yes, but need the following fix:
  environment(design2$param_set$trafo)$trafos$glmnet.alpha <- function(x) pmax(0, pmin(1, x))

  design$data <- copy(design$data)[, (budgetparams) := budgetparamval]


  protolist <- lapply(design$data, function(x) c(NA, x)[[1]])
  manies <- lapply(design$transpose(), mlr3misc::insert_named, x = protolist)


  all.equal(rbindlist(manies), design2$param_set$trafo(design2$data, design2$param_set)[, (budgetparams) := budgetparamval])

  vals <- problem$eval_many(manies)



i <- 36

qi <- readRDS(sprintf("oldmodels/mfrunner/problem_%i_seed_1.rds", i))

i <- 3
qi <- readRDS(sprintf("mfrunner/problem_%i_seed_1.rds", i))


problem <- ll[[i]]
budgetparams <- problem$domain$ids(tags = "budget")
budgetparamval <- problem$domain$upper[[budgetparams]]


design2 <- paradox::generate_design_random(problem$domain, 1e5)
manies <- design2$param_set$trafo(design2$data, design2$param_set)[, (budgetparams) := budgetparamval]
vals <- problem$eval_dt(manies)




mean(vals$logloss < qi$quantile.map[100]$val)

qi$quantile.map[100]$q

qi$quantile.map[100]$q

qi$quantile.map[100]$val
qi$quantile.map[100]$q

quants <- toQuantile(vals$logloss, qi)
quants <- toQuantile(vals$val_cross_entropy, qi)


plot(sort(quants))


abline(0, 1e-3)


myreps <- sapply(1:100, function(xx) {
design2 <- paradox::generate_design_random(problem$domain, 1e3)
manies <- design2$param_set$trafo(design2$data, design2$param_set)[, (budgetparams) := budgetparamval]
vals <- problem$eval_dt(manies)
quants <- toQuantile(vals$val_cross_entropy, qi)
plot(sort(quants))
abline(0, 1e-3)


min(quants)
})

plot(sort(pmin(runif(1e3), 0.5)))
abline(0, 1e-3)


unifreps <- replicate(100, min(runif(1e3)))
plot(sort(log(myreps)), sort(log(unifreps)))

hist(log(myreps))

abline(0, 1)


plot(qi$quantile.map[, .(log10_quantile = log10(q), crossentropy = val)], ylim = c(0, max(qi$quantile.map$val)), xlim = c(-log10(qi$n+1), 0), type= "l")
points(log10(seq_along(qi$empiricals$empiricals) / (qi$n + 1)), qi$empiricals$empiricals, pch = ".")


plot(qi$quantile.map[, .(log10_quantile = (q), crossentropy = val)], ylim = c(0, max(qi$quantile.map$val)), xlim = c(1/(qi$n+1), 0.5), type= "l")
lines((seq_along(qi$empiricals$empiricals) / (qi$n + 1)), qi$empiricals$empiricals)


# we: 1.1722171  they: 0.2231249
# we: 1.031834   they: 0.1926129

1.1722171 / exp(log(1.1722171 / 1.031834) / (0.2231249 - 0.1926129) * (0.2231249 - 0.1718510))

qi$quantile.map[q == .125, roll = Inf]

min(qi$empiricals)








empiricals <- readRDS("/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates/lcbench/data.rds")
randomval <- empiricals[sample(which(OpenML_task_id == "126026"), 10000)]

randomvalin <- randomval[, problem$domain$ids(), with = FALSE]


results <- problem$eval(randomvalin)

trueres <- randomval[, names(results), with = FALSE]
sampled <- as.data.table(results)
plot(trueres$val_cross_entropy, sampled$val_cross_entropy, pch = 'o')

plot(trueres$val_cross_entropy, sampled$val_cross_entropy, pch = 'o')

plot(log(trueres$val_cross_entropy), log(sampled$val_cross_entropy), pch = 'o')

plot(lcbdicts[[tinstnao$target]]$retrafo(trueres$val_cross_entropy), log(sampled$val_cross_entropy), pch = 'o')
plot(lcbdicts[[tinstnao$target]]$trafo(trueres$val_cross_entropy), log(sampled$val_cross_entropy), pch = 'o')
plot(trueres$val_cross_entropy, log(sampled$val_cross_entropy), pch = 'o')
plot(trueres$val_cross_entropy, (sampled$val_cross_entropy), pch = 'o')
plot(log(trueres$val_cross_entropy), (sampled$val_cross_entropy), pch = 'o')

plot(mfsurrogates2:::scale_base_0_1(empiricals$val_cross_entropy)$trafo(trueres$val_cross_entropy),  mfsurrogates2:::scale_base_0_1(empiricals$val_cross_entropy)$trafo(sampled$val_cross_entropy), pch = 'o')
plot((trueres$val_cross_entropy),  mfsurrogates2:::scale_base_0_1(empiricals$val_cross_entropy)$trafo(sampled$val_cross_entropy), pch = 'o')
plot(mfsurrogates2:::scale_base_0_1(empiricals$val_cross_entropy)$trafo(trueres$val_cross_entropy),  (sampled$val_cross_entropy), pch = 'o')

for (n in names(sampled)) {
  plot(sort(trueres[[n]]), sort(sampled$val_cross_entropy), pch = 'o')
  Sys.sleep(2)
}


randomval

lcbdicts[[tinstnao$target]]$trafo

plot(log(trueres$time), log(sampled$time))
abline(log(5), 1)





plot(log(trueres$val_cross_entropy), log(sampled$val_cross_entropy))





qs <- replicate(2000000, sum(runif(10, -1, 1)^2))

plot(log10(seq_along(qs) / (length(qs) + 1)), sort(qs), type = "l", ylim = c(0, 20))
plot((seq_along(qs) / (length(qs) + 1)), sort(qs), type = "l", ylim = c(0, 20))


qs2 <- replicate(1000, sum(runif(1000, -1, 1)^2))
points(log10(seq_along(qs2) / (length(qs2) + 1)), sort(qs2))

#plot(trueres$val_accuracy, ourres$val_accuracy)

problem$eval_dt(randomval)

best <- empiricals[OpenML_task_id == "126026"][which.min(val_cross_entropy)]
worst <- empiricals[OpenML_task_id == "126026"][which.max(val_cross_entropy)]

problem$domain

best <- best[, problem$domain$ids(), with = FALSE]
worst <- worst[, problem$domain$ids(), with = FALSE]

problem$eval(best)

problem$eval(worst)
empiricals[OpenML_task_id == "126026"][which.max(val_cross_entropy)]





mean(myreps)
mean(unifreps)

mean(log(myreps))
mean(log(unifreps))

plot(qi$empiricals$empniricals)

min(qi$empiricals$empiricals)

min(vals$logloss)

qq <- QuantileInfo(deltafn = mlr3misc::crate(function(q) min(0.1 * q, 1e-3)), quantile.min = .5)
for (i in seq_len(100)) qq <- updateQI(qq, rnorm(1e6))

qq <- updateQI(qq, rnorm(1e6))

qq

rnorm(1e3)


normalreps <- replicate(100, min(toQuantile(rnorm(1e3), qq)))

plot(sort(unifreps), sort(normalreps))
abline(0, 1)


hist(quants[quants < 0.5])


sort(table(qi$empiricals$empiricals))

sum(table(qi$empiricals$empiricals))




qq <- QuantileInfo(deltafn = mlr3misc::crate(function(q) min(0.1 * q, 1e-3)), quantile.min = .5)
for (i in seq_len(100)) qq <- updateQI(qq, round(rnorm(1e5), 3))


tail(sort(table(qq$empiricals$empiricals)))
head(sort(table(qq$empiricals$empiricals)))

normalreps <- replicate(100, min(toQuantile(round(rnorm(1e3), 3), qq)))

plot(sort(unifreps), sort(normalreps))
abline(0, 1)


qq

names(qi)
qi$quantile.map
qq$quantile.map


cx <- cfgs(tinstnao$cfg, workdir = workdir)

cx

loglosses <- sapply(seq_len(nrow(tinst)), function(i) {

  tinstnao <- tinst[i]
  print(tinstnao)

  problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)

  budgetparams <- problem$domain$ids(tags = "budget")
  budgetparamval <- problem$domain$upper[[budgetparams]]

  design <- paradox::generate_design_random(problem$domain, 10000)

  design$data[, (budgetparams) := budgetparamval]
  protonames <- colnames(design$data)

  manies <- lapply(design$transpose(), mlr3misc::insert_named, x = lapply(design$data, function(x) c(NA, x)[[1]]))

  problem$eval_many(manies)[[tinstnao$target]]


})



colnames(loglosses) <- seq_len(nrow(tinst))


hist(log(1 - loglosses[, 20]), breaks = 1000)
hist(loglosses[, 9], breaks = 1000)
hist(loglosses[, 10], breaks = 1000)
hist(loglosses[, 11], breaks = 1000)
hist(loglosses[, 12], breaks = 1000)
hist(loglosses[, 13], breaks = 1000)
hist(loglosses[, 14], breaks = 1000)
hist(loglosses[, 15], breaks = 1000)
hist(loglosses[, 16], breaks = 1000)
hist(loglosses[, 17], breaks = 1000)
hist(loglosses[, 18], breaks = 1000)
hist(loglosses[, 19], breaks = 1000)



cm <- colMeans(loglosses == 1)

max(loglosses)

sort(apply(loglosses, 2, function(x) length(unique(x))))


sort(cm)

problem$eval_many(manies[1:100])

tinst[36]

i <- 36



hist(loglosses[, 1], breaks = 1000)

dt <- problem$eval_many(manies)

plot(dt$auc, dt$logloss)







problem$domain






problem$codomain$params$logloss$tags





instances[test == FALSE & cfg == "lcbench"]
instances$cfog
instances[test == FALSE & cfg == "nb301"]



tinst

cfg = cfgs("lcbench", workdir = workdir)


cfg$get_objective("7593")

cfg = cfgs("shekel", workdir = workdir)

cfg$get_objective

tinst[cfg == "shekel"]$level


instances[is.na(level)]

class(cfgs("shekel", workdir = workdir))

BenchmarkConfigShekel$public_methods$get_objective


# can currently use:
# nasbench, lcbench

