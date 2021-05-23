library("parallelMap")
library("mlrintermbo")

problem_count <- 38


parallelStartSocket(cpus = problem_count, load.balancing = TRUE)

parallelSource("load_objectives.R")

curseed <- as.numeric(commandArgs(trailingOnly = TRUE)[[1]])

algo <- commandArgs(trailingOnly = TRUE)[[2]]
checkmate::assertChoice(algo, c("intermbo", "random_search", "design_points"))
searchspace <- commandArgs(trailingOnly = TRUE)[[3]]

short <- FALSE
if (identical(commandArgs(trailingOnly = TRUE)[4], "shortrun")) {
  cat("SHORT RUN\n")
  short <- TRUE
}

checkmate::assertChoice(searchspace, c("discrete", "numeric"))

filename <- sprintf("run_%s_%s_%s_seed_%s.rds", algo, searchspace, gsub(":", "-", gsub(" ", "_", Sys.time())), curseed)
tmpname <- paste0(filename, ".tmp")
saveRDS(Sys.time(), tmpname)

lgr::get_logger("mlr3")$set_threshold("info")
lgr::get_logger("bbotk")$set_threshold("info")


evaluate_metaconf <- function(metaconf) {

  saveRDS(oi, tmpname)
  file.rename(tmpname, filename)

  curseed <<- curseed + 1
  evalresults <- parallelMap(evaluate_miesmuschel, seq_len(problem_count), more.args = list(seed = curseed, metaconf = metaconf, budgetfactor = if (short) 3 else 30))

  c(list(yval = mean(unlist(evalresults)), curseed = curseed), structure(evalresults, names = tinst[, sprintf("%s.%s", cfg, level)]))
}

objective <- bbotk::ObjectiveRFun$new(
  fun = function(xs) {
    evaluate_metaconf(xs)
  },
  domain = suggested_meta_domain,
  codomain = ps(yval = p_dbl(tags = "maximize"))
)

space <- switch(searchspace, discrete = suggested_meta_searchspace,  numeric = suggested_meta_searchspace_numeric, stop())

oi <- bbotk::OptimInstanceSingleCrit$new(objective, search_space = space, terminator = bbotk::trm("run_time", secs = if (short) 60 * 20 else 60 * 60 * 70))

if (algo == "intermbo") {
  opter <- bbotk::opt(algo, infill.opt = "focussearch", infill.opt.focussearch.maxit = 20)
} else if (algo == "design_points") {
  design <- rbindlist(rep(list(generate_design_random(space, 10)$data), 100))
  opter <- bbotk::opt(algo, design = Design$new(space, design, FALSE))
} else {
  opter <- bbotk::opt(algo)
}

opter$optimize(oi)

saveRDS(oi, tmpname)
file.rename(tmpname, filename)


## library("ggplot2")
## pl <- melt(oi$archive$data[, .SD, .SDcols = grepl("\\.", colnames(oi$archive$data))])

## ggplot(pl, aes(x = variable, y = value)) + geom_boxplot()
## colnames(oi$archive$data)
## ggplot(oi$archive$data, aes(x = , y = yval)) + geom_point()
## ggplot(oi$archive$data, aes(x = random_interleave_random, y = yval)) + geom_boxplot()

# dse <- generate_design_random(suggested_meta_searchspace, 10)
#
# pres <- profvis::profvis(evaluate_miesmuschel(10, seed = 10, metaconf = dse$transpose()[[1]], budgetfactor = 10), simplify = FALSE)
# htmlwidgets::saveWidget(pres, "profiled.html", selfcontained = FALSE)

