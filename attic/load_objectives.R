
options(warn = 1)

library("mfsurrogates2")
library("data.table")
library("miesmuschel")
library("checkmate")

source("quantileinfo.R")
source("optim2.R")

lgr::get_logger("mlr3")$set_threshold("warn")
lgr::get_logger("bbotk")$set_threshold("warn")

options(width=170)
options(error=function()traceback(2))

workdir <- "/dss/dsshome1/lxc08/di25pic2/motherfucking_surrogates"

instances <- readRDS("~/instances.rds")
instances[cfg == "lcbench", target := "val_cross_entropy"]
instances[cfg == "lcbench", multiplier := 1]
instances[cfg == "rbv2_super", target := "logloss"]
instances[cfg == "rbv2_super", multiplier := 1]

tinst <- instances[test == FALSE]


surrogates <- lapply(seq_len(nrow(tinst)), function(i) {
  tinstnao <- tinst[i]
  problem <- cfgs(tinstnao$cfg, workdir = workdir)$get_objective(tinstnao$level, target_variables = tinstnao$target)
})

qis <- lapply(seq_len(nrow(tinst)), function(i) {
  readRDS(sprintf("problem_%i_seed_1.rds", i))
})

evaluate_miesmuschel <- function(problem, metaconf, seed, budgetfactor = 30) {
  set.seed(1)
  assertInt(problem, lower = 1, upper = nrow(tinst))

  cursur <- surrogates[[problem]]
  curqi <- qis[[problem]]

  budget_id <- cursur$domain$ids(tags = "budget")
  fulleval_equivalents <- budgetfactor * cursur$domain$length
  budget_limit <- fulleval_equivalents * cursur$domain$upper[[budget_id]]


  prevtrafo <- cursur$domain$trafo

  search_space <- cursur$domain$clone(deep = TRUE)
  id_order <- search_space$ids()
  proto_dt <- generate_design_random(search_space, 1)$data

  if (cursur$codomain$ids() == "val_cross_entropy") {
    # lcbench special code

    budgetupper <- search_space$params[[budget_id]]$upper

    search_space$.__enclos_env__$private$.params[[budget_id]] <- ParamDbl$new(budget_id,
      lower = log(search_space$params[[budget_id]]$lower),
      upper = log(search_space$params[[budget_id]]$upper + 1),
      tags = "budget"
    )

    search_space$trafo <- mlr3misc::crate(function(x, param_set) {
      x <- prevtrafo(x)
      x[[budget_id]] <- min(floor(exp(x[[budget_id]])), budgetupper)
      rbind(proto_dt, x, fill = TRUE)[2]
    }, prevtrafo, budget_id, proto_dt, budgetupper)

  } else {
    # randombot special code
    search_space$params[["trainsize"]]$lower <- 3^-3

    search_space$params[[budget_id]]$lower <- log(search_space$params[[budget_id]]$lower)
    search_space$params[[budget_id]]$upper <- log(search_space$params[[budget_id]]$upper)

    cursur$trafo_dict$logloss$retrafo <- function(x) { ret <- -log(x) ; ret[ret > 1e20] <- 1e20 ; ret }  # way faster than pmin
    search_space$trafo <- mlr3misc::crate(function(x, param_set) {
      x <- prevtrafo(x)
      x[[budget_id]] <- exp(x[[budget_id]])
      rbind(proto_dt, x, fill = TRUE)[2]
    }, prevtrafo, budget_id, proto_dt)

  }

  result <- mlr3misc::invoke(opt_objective_optimizable, cursur, cursur, search_space, budget_limit = budget_limit, highest_budget_only = TRUE, sample = "random", .args = metaconf)


  log2.improvement.over.rs <- -log2(fulleval_equivalents * toQuantile(-c(result), curqi))

  log2.improvement.over.rs
}

