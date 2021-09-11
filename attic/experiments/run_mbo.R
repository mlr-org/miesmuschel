source("argparse.R")
options(warn = 1)
options(width=170)
options(error = function() {
  traceback(3)
  if(!interactive())quit("no",status=1,runLast=FALSE)
})


args <- list(
  arg("Cores to use at most", "cores", "c", "integer", name = "cores", checkmate.args = list(lower = 1)),
  arg("File to load", "file", "f", "character", name = "filename"),
  arg("Print help and exit", "help", "h")
)

opts <- argparse(args, helparg = "help")

filename <- opts$filename

blob <- readRDS(filename)

fun <- blob$fun

if (is.null(fun)) stop(sprintf("%s was not a valid file.", filename))


library("parallelMap")
library("mlrintermbo")

if (opts$cores > 1) {
  parallelStartSocket(cpus = opts$cores, load.balancing = TRUE)
}

parallelSource("load_objectives2.R")

lgr::get_logger("mlr3")$set_threshold("info")
lgr::get_logger("bbotk")$set_threshold("info")

fun()
