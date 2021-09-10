library("parallelMap")
library("mlrintermbo")


curseed <- as.numeric(commandArgs(trailingOnly = TRUE)[[1]])
algo <- commandArgs(trailingOnly = TRUE)[[2]]
searchspace <- commandArgs(trailingOnly = TRUE)[[3]]
runlen <- commandArgs(trailingOnly = TRUE)[[4]]
objective <- commandArgs(trailingOnly = TRUE)[[5]]
fixedmu <- commandArgs(trailingOnly = TRUE)[[6]]
siman <- commandArgs(trailingOnly = TRUE)[[7]]
batchmethod <- commandArgs(trailingOnly = TRUE)[[8]]
cores <- as.numeric(commandArgs(trailingOnly = TRUE)[[9]])
infillsearch <- commandArgs(trailingOnly = TRUE)[[10]]

checkmate::assertInt(as.integer(curseed))
checkmate::assertChoice(algo, c("intermbo", "random_search", "design_points"))
checkmate::assertChoice(searchspace, c("discrete", "numeric"))
checkmate::assertChoice(runlen, c("longrun", "shortrun"))
short <- runlen == "shortrun"
checkmate::assertChoice(objective, c("lcbench", "rbv2_super", "all"))
checkmate::assertChoice(fixedmu, c("mufix", "muvary"))
fixedmu <- fixedmu == "mufix"
checkmate::assertChoice(siman, c("siman", "nosiman"))
siman <- siman == "siman"
checkmate::assertChoice(batchmethod, c("smashy", "hb", "any"))
checkmate::assertCount(cores, tol = 1e-100)
checkmate::assertChoice(infillsearch, c("infill_rs", "infill_vario", "infill_all"))


### arguments:
# <curseed> <algo> <searchspace> <runlen | short> <objective> <fixedmu> <siman> <batchmethod> <cores> <infillsearch>

# evaluation modality:
# - curseed
# - algo
# - runlen
# - objective
# - cores

# searchspace config:
# - searchspace (only relevant for algo == "intermbo"
# - fixedmu
# - siman
# - batchmethod
# - infillsearch

if (fixedmu && batchmethod != "smashy") stop("fixed mu only with smashy")

problem_count <- switch(objective, lcbench = 8, rbv2_super = 30, all = 38, stop())

if (problem_count >= cores) {
  parallelity <- cores
  multiplier <- 1
} else {
  multiplier <- floor(cores / problem_count)
  parallelity <- multiplier * problem_count
}

budgetfactor <- (if (short) 3 else 30) * (if (fixedmu) 32 else 1)

parallelStartSocket(cpus = problem_count, load.balancing = TRUE)

parallelSource("load_objectives2.R")

if (objective == "all") {
  problem_ids = seq_len(nrow(tinst))
} else {
  problem_ids = tinst[, which(cfg == objective)]
}

assertTRUE(length(problem_ids) == problem_count)

filename <- sprintf("run_%s_%s_%s_%s%s%s_%s_seed_%s.rds", algo, searchspace, objective,
  if (fixedmu) "mufix_" else "muvary_", if (!siman) "nosiman_" else "siman_", batchmethod, gsub(":", "-", gsub(" ", "_", Sys.time())), curseed)
tmpname <- paste0(filename, ".tmp")
saveRDS(Sys.time(), tmpname)

lgr::get_logger("mlr3")$set_threshold("info")
lgr::get_logger("bbotk")$set_threshold("info")

set.seed(curseed)

evaluate_metaconf <- function(metaconf) {

  saveRDS(oi, tmpname)
  file.rename(tmpname, filename)

  curseed <<- curseed + problem_count
  if (fixedmu) metaconf$mu <- 32
  if (batchmethod != "any") metaconf$batch_method <- batchmethod

  callseed = seq(curseed, length.out = problem_count)
  more.args = list(metaconf = metaconf, budgetfactor = budgetfactor)

  while (!tryCatch({
    evalresults <- parallelMap(evaluate_miesmuschel, problem_ids, seed = callseed, more.args = more.args)
    TRUE
  }, error = function(e) {
      # retry once when parallelMap crashes for some reason
    cat("error in parallelMap\n")
    cat(e$message)
    cat("\n")
    FALSE
  })) {
    parallelStop()
    parallelStartSocket(cpus = problem_count, load.balancing = TRUE)
    parallelSource("load_objectives2.R")
    lgr::get_logger("mlr3")$set_threshold("info")
    lgr::get_logger("bbotk")$set_threshold("info")
    parallelMap(evaluate_miesmuschel, problem_ids, seed = callseed, more.args = more.args)
  }

  c(list(yval = mean(unlist(evalresults)), curseed = curseed), structure(evalresults, names = tinst[problem_ids, sprintf("%s.%s", cfg, level)]))
}

objective <- bbotk::ObjectiveRFun$new(
  fun = function(xs) {
    evaluate_metaconf(xs)
  },
  domain = suggested_meta_domain,
  codomain = ps(yval = p_dbl(tags = "minimize"))
)

space <- get_searchspace(
  include.mu = !fixedmu, include.batchmethod = batchmethod == "any",
  infill = substr(infillsearch, 8, 100), include.siman = siman,
  include.mo = FALSE, numeric.only = searchspace == "numeric"
)

oi <- bbotk::OptimInstanceSingleCrit$new(objective, search_space = space, terminator = bbotk::trm("run_time", secs = if (short) 60 * 10 else 60 * 60 * 24 * 7))

if (algo == "intermbo") {
  opter <- bbotk::opt(algo, infill.opt = "focussearch", infill.opt.focussearch.maxit = 20)
  if (short) opter$param_set$values$initial.design.size = 5
} else if (algo == "design_points") {
  design <- rbindlist(rep(list(generate_design_random(space, 10)$data), 100))
  opter <- bbotk::opt(algo, design = design)
} else {
  opter <- bbotk::opt(algo)
}

opter$optimize(oi)

saveRDS(oi, tmpname)
file.rename(tmpname, filename)
