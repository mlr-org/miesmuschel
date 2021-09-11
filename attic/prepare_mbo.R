source("argparse.R")
options(warn = 1)
options(width=170)
options(error=function()traceback(2))


args <- list(
  arg("Seed for evaluation", "seed", "s", "numeric", name = "curseed"),
  arg("Cores to use at most", "cores", "c", "numeric", name = "cores"),
  arg("What task set to optimize for", "objectives", "o", "choice", choices = c("lcbench", "rbv2_super", "all"), name = "objective"),
  arg("How many repeated evals to do for each config", "repeat", "r", "numeric", 1, name = "reps"),
  arg("What meta-opt algo to use", "metaopt", "m", "choice", default = "intermbo", choices = c("intermbo", "random_search", "design_points"), name = "algo"),
  arg("Fix mu to a number. 0: no fix", "mu", "M", type = "numeric", default = 0),
  arg("Use numeric search space", "numspace", "N", name = "searchspace.numeric"),
  arg("Search with simulated annealing", "search-siman", "S", name = "siman"),
  arg("Batch-method to search over", "search-batch", "B", type = "choice", default = "smashy", choices = c("smashy", "hb", "any"), name = "batchmethod"),
  arg("What infill method to use", "infill", "I", "choice", default = "rs", choices = c("rs", "vario", "all"), name = "infillsearch"),
  arg("Short test run", "dryrun", "n", name = "short"),
  arg("Help and exit", "help", "h")
)

print(argparse(args))

