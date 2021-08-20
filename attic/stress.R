
library("parallelMap")
parallelStartSocket(cpus = 56, load.balancing = TRUE)

parallelMap(function(x) {
  for (i in 1:1000) for (j in 1:1000) for (k in 1:1000) i + j + k
}, 1:100)


