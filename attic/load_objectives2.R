WORKDIR <- "./data/motherfucking_surrogates"
INSTANCES <- "./data/instances.rds"

options(warn = 1)

library("mfsurrogates2")
library("data.table")
library("miesmuschel")
library("checkmate")

source("optim2.R")

lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

options(width=170)
options(error=function()traceback(2))

instances <- readRDS(INSTANCES)
instances[cfg == "lcbench", target := "val_cross_entropy"]
instances[cfg == "lcbench", multiplier := 1]
instances[cfg == "rbv2_super", target := "logloss"]
instances[cfg == "rbv2_super", multiplier := 1]
tinst <- instances[test == FALSE]


surrogates <- lapply(seq_len(nrow(tinst)), function(i) {
  tinstnao <- tinst[i]
  cfgs(tinstnao$cfg, workdir = WORKDIR)$get_objective(tinstnao$level, target_variables = tinstnao$target)
})


evaluate_miesmuschel <- function(problem, metaconf, seed, budgetfactor = 30) {
  set.seed(seed)
  assertInt(problem, lower = 1, upper = nrow(tinst))

  cursur <- surrogates[[problem]]

  meta_objective <- get_meta_objective_from_surrogate(cursur, budgetfactor)

  mlr3misc::invoke(meta_objective, .args = metaconf)

}

