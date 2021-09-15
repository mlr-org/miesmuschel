library("data.table")
library("ggplot2")
source("optim2.R")


lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")


# read in all archives

allresults <- lapply(list.files("data/results", full.names = TRUE, recursive = TRUE, pattern = "\\.rds$"), readRDS)



metaconfs <- rbindlist(lapply(allresults, `[[`, "config"))

archives <- lapply(allresults, function(x) cbind(x$oi$archive$data, x = rbindlist(x$oi$archive$data$x_domain)))
spaces <- lapply(allresults, function(x) x$oi$search_space)

metaconfs[, evaluated := vapply(archives, nrow, 0)]
metaconfs[, evaluated.mbo := vapply(archives, function(x) sum(!is.na(x$crit.vals)), 0)]

metaconfs[, `:=`(reps = NULL, budgetfactor = NULL, algo = NULL, mu = ifelse(mu == 0, "muvary", "mufix"), searchspace.numeric = NULL, help = NULL)]

metaconfs[, filename := NULL]

getOptimalLambda <- function(ca, space, seed = 1) {
  if (nrow(ca) == 0 || nrow(ca) <= ncol(ca)) {
    return(list(lambda = list(), performance = NA_real_, se = NA_real_))
  }

  library("data.table")
  library("mlr3learners")
  library("mlr3")
  library("mlr3pipelines")
  library("bbotk")
  library("miesmuschel")

  lgr::get_logger("mlr3")$set_threshold("warn")
  lgr::get_logger("bbotk")$set_threshold("warn")

  set.seed(1)

  space$values <- list()  # names get dropped sometimes, make sure this has a named list.
  space_no_trafo <- space$clone(deep = TRUE)
  space_no_trafo$trafo <- NULL

  ts <- TaskRegr$new("surrogate", ca[, c(space$ids(), "yval"), with = FALSE], "yval")

  lr <- as_learner(po("colapply", applicator = as.factor, affect_columns = selector_type("character")) %>>%
    po("encode") %>>%
    lrn("regr.km", predict_type = "se",
      covtype = "matern3_2", optim.method = "gen", nugget.estim = TRUE, jitter = 1e-12,
      control = list(max.generations = 100, wait.generations = 20, pop.size = 100)
    ))

  lr$train(ts)


  if (FALSE) {

  surrogateObjective <- ObjectiveRFunDt$new(fun = function(xdt) {
    data.table(yval = lr$predict_newdata(xdt)$response)
  }, domain = space_no_trafo, codomain = ps(yval = p_dbl(tags = "maximize")))

  oi <- OptimInstanceSingleCrit$new(surrogateObjective, search_space = space_no_trafo,
    terminator = trm("gens", generations = 50)
  )

  op.m <- mut("combine",
    operators = list(
        ParamFct = mut("cmpmaybe", mut("unif"), p = 0.1),
        ParamLgl = mut("cmpmaybe", mut("unif"), p = 0.1),
        ParamDbl = mut("cmpmaybe", mut("gauss", sdev_is_relative = TRUE, sdev = 0.1), p = 0.1)
    ), on_type_not_present = "quiet")

  op.m.small <- mut("combine",
    operators = list(
        ParamFct = mut("cmpmaybe", mut("unif"), p = 0.03),
        ParamLgl = mut("cmpmaybe", mut("unif"), p = 0.03),
        ParamDbl = mut("cmpmaybe", mut("gauss", sdev_is_relative = TRUE, sdev = 0.03), p = 0.1)
    ), on_type_not_present = "quiet")

  op.r <- rec("maybe", rec("xounif", p = 0.1), p = 0.1)
  op.parent <- sel("random")
  op.survival <- sel("best")

  surrogateOptimizer <- opt("mies", recombinator = op.r,
    parent_selector  = op.parent, survival_selector = op.survival, mu = 100, lambda = 100)

  surrogateOptimizer$param_set$values$mutator.operation <- op.m
  oi$terminator$param_set$values$generations <- 50

  surrogateOptimizer$optimize(oi)

  surrogateOptimizer$param_set$values$mutator.operation <- op.m.small
  oi$terminator$param_set$values$generations <- 100

  surrogateOptimizer$optimize(oi)

  oi$terminator = trm("evals", n_evals = nrow(oi$archive$data) + 10000)

  result <- opt("random_search", batch_size = 1000)$optimize(oi)

  predicted <- lr$predict_newdata(result)

  list(lambda = space$trafo(result$x_domain[[1]]), performance = predicted$response, se = predicted$se)
  }
  predicted <- lr$predict(ts)

  best <- which.max(predicted$response)

  list(lambda = space$trafo(as.list(ca[best, space$ids(), with = FALSE])), performance = predicted$response[[best]], se = predicted$se[[best]], performance.measured = ca[best, yval])

}


metaconfs[, archive := archives]
metaconfs[, search_space := spaces]

combined <- metaconfs[, .(curseed = -1, archive = list(rbindlist(archive, use.names = TRUE, fill = TRUE)), search_space = search_space[1],
  evaluated = sum(evaluated), evaluated.mbo = sum(evaluated.mbo)),
  by = c("objective", "mu", "siman", "batchmethod", "infillsearch")]

allconfs <- rbind(metaconfs, combined)

library("parallelMap")

parallelStartSocket(cpus = 30, load.balancing = TRUE)

lambdas <- do.call(parallelMap, c(list(getOptimalLambda), as.list(allconfs[, .(ca = archive, space = search_space)])))

collected <- cbind(allconfs, rbindlist(lapply(lambdas, function(x) list(lambda = list(x$lambda), lperf = x$performance, lse = x$se, lpm = x$performance.measured))))


saveRDS(collected, "collected.rds")

####

## collected[curseed < 0 & !is.na(lperf)][order(lperf)]

## rbindlist(collected[curseed > 0 & !is.na(lperf)][order(lperf)][14:16]$lambda)


## collected[curseed < 0 & !is.na(lperf)][order(lperf)][13]$lambda[[1]]



## ggplot(oi$archive$data, aes(x = filter_factor_first, y = yval, color = ifelse(is.na(dob), "random", ifelse(dob <= 50, "first", "second")))) + geom_point()
## ggplot(oi$archive$data, aes(x = mu, y = yval, color = ifelse(is.na(dob), "random", ifelse(dob <= 50, "first", "second")))) + geom_point()


## metaconfs[curseed < 3e5, .(mine = min(evaluated), maxe = max(evaluated)),
##   by = c("objective", "mu", "siman", "batchmethod", "infillsearch")]


## ggplot(metaconfs, aes(x = mu, y = evaluated)) + geom_point()
## ggplot(metaconfs, aes(x = as.numeric(as.factor(objective)) + mu / 8, y = evaluated)) + geom_point()



