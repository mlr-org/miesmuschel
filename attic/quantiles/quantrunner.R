
source("quantileinfo.R")

# call: <seed> <index>

inseed <- as.numeric(commandArgs(trailingOnly = TRUE)[[1]])

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

evaling <- suppressWarnings(split(seq_len(nrow(tinst)), 1:2))

evx <- evaling[[commandArgs(trailingOnly = TRUE)[[2]]]]

qis <- parallel::mclapply(evx, function(i) {
  set.seed(inseed)
  tinstnao <- tinst[i]

  problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)

  budgetparams <- problem$domain$ids(tags = "budget")
  budgetparamval <- problem$domain$upper[[budgetparams]]

  qi <- QuantileInfo(deltafn = mlr3misc::crate(function(q) min(0.1 * q, 1e-3)), quantile.min = .5)

  filename <- sprintf("problem_%s_seed_%s.rds", i, inseed)
  tmpname <- paste0(filename, ".tmp")
  saveRDS(Sys.time(), tmpname)

  repeat {
    for (j in 1:100) {
      cat(sprintf("%s %s round %s (%s)\n", inseed, i, j, qi$n))

      design <- paradox::generate_design_random(problem$domain, 1e5)


      # fix for vectorized trafo
      environment(design$param_set$trafo)$trafos$glmnet.alpha <- function(x) pmax(0, pmin(1, x))
      evaling <- design$param_set$trafo(design$data, design$param_set)[, (budgetparams) := budgetparamval]

      # maybe need the copy to avoid invalid selfref warning?
      results <- problem$eval_dt(copy(evaling))[[tinstnao$target]]

      qi <- updateQI(qi, results * tinstnao$multiplier)
    }
    saveRDS(qi, tmpname)
    file.rename(tmpname, filename)
  }
  qi
}, mc.preschedule = FALSE, mc.cores = 56)

