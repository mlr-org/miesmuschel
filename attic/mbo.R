library("parallelMap")
library("mlrintermbo")

problem_count <- 38


parallelStartSocket(cpus = problem_count, load.balancing = TRUE)

parallelSource("load_objectives.R")

curseed <- as.numeric(commandArgs(trailingOnly = TRUE)[[1]])

algo <- commandArgs(trailingOnly = TRUE)[[2]]

filename <- sprintf("run_%s_%s_seed_%s.rds", algo, gsub(":", "-", gsub(" ", "_", Sys.time())), curseed)
tmpname <- paste0(filename, ".tmp")
saveRDS(Sys.time(), tmpname)

lgr::get_logger("mlr3")$set_threshold("info")
lgr::get_logger("bbotk")$set_threshold("info")


evaluate_metaconf <- function(metaconf) {

  saveRDS(oi, tmpname)
  file.rename(tmpname, filename)


  metaconf <- generate_design_random(suggested_meta_searchspace, 1)$transpose()[[1]]

  curseed <- curseed + 1
  evalresults <- parallelMap(evaluate_miesmuschel, seq_len(problem_count), more.args = list(seed = curseed, metaconf = metaconf, budgetfactor = 30))


  c(list(yval = mean(unlist(evalresults)), curseed = curseed), structure(evalresults, names = tinst[, sprintf("%s.%s", cfg, level)]))
}


objective <- bbotk::ObjectiveRFun$new(
  fun = function(xs) {
    evaluate_metaconf(metaconf)
  },
  domain = suggested_meta_domain,
  codomain = ps(yval = p_dbl(tags = "maximize"))
)

oi <- bbotk::OptimInstanceSingleCrit$new(objective, suggested_meta_searchspace, terminator = bbotk::trm("run_time", secs = 60 * 60 * 70))

bbotk::opt(algo)$optimize(oi)

saveRDS(oi, tmpname)
file.rename(tmpname, filename)


## library("ggplot2")
## pl <- melt(oi$archive$data[, .SD, .SDcols = grepl("\\.", colnames(oi$archive$data))])

## ggplot(pl, aes(x = variable, y = value)) + geom_boxplot()
## colnames(oi$archive$data)
## ggplot(oi$archive$data, aes(x = , y = yval)) + geom_point()
## ggplot(oi$archive$data, aes(x = random_interleave_random, y = yval)) + geom_boxplot()
