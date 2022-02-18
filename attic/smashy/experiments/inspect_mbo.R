
options(warn = 1)
options(width=170)
options(error = function() {
  traceback(3)
  if(!interactive())quit("no",status=1,runLast=FALSE)
})

filename <- commandArgs(trailingOnly = TRUE)[[1]]

blob <- readRDS(filename)

cat(sprintf("Config:
%s

Evals done: %s
", paste(deparse(blob$config, width.cutoff = getOption("width")), collapse = "\n"), nrow(blob$oi$archive$data)))
