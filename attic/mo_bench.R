



library("checkmate")
library("mlr3misc")
library("microbenchmark")

source("R/utils_mo.R")

normmat <- function(n, d) matrix(abs(rnorm(n * d)), ncol = d)

nplex <- function(n, d) t(replicate(n, { x <- rexp(d) ; x / sum(x) }))


baseline = function(mat) ecr::computeHV(-t(mat), rep(0, ncol(mat)))
weighted = function(mat) domhv(mat)
unweighted = function(mat) domhv(mat, wgt = FALSE)


results3 <- lapply(3:6, function(d) {
  sapply((1:6) * 100, function(n) {
    data = nplex(n, d)
    bmr <- microbenchmark(baseline(data), weighted(data), unweighted(data),
      check = "equal", times = 3, unit = "ms")
    c(n = n, d = d, summary(bmr)[, "median"])
  })
})


profvis::profvis(
results4 <- lapply(3:5, function(d) {
  sapply((1:6) * 100, function(n) {
    data = nplex(n, d)
    bmr <- microbenchmark(baseline(data), weighted(data), unweighted(data),
      check = "equal", times = 3, unit = "ms")
    c(n = n, d = d, summary(bmr)[, "median"])
  })
})
)

library("data.table")
ptx4 <- rbindlist(lapply(results4, function(x) as.data.frame(t(x))))
library("ggplot2")
colnames(ptx4) <- c("n", "d", "baseline", "weighted", "unweighted")

ggplot(melt(ptx4, c("n", "d")),
  aes(x = log(n), y = log(value), linetype = variable, color = paste0(d),
    group = paste0(variable, d))) +
  geom_line()


exps <- sapply(3:6, function(x)
coefficients(lm(I(log(value)) ~ I(log(n)),
  data = melt(ptx3, c("n", "d"))[variable == "baseline" & d == x]))[[2]]
)

plot(exps)

lm(exps ~ I(3:6))


1.292 + 3 * 1.978

lm(I(log((1:5)^(6/3))) ~ I(log( (1:5) * 100)))




baseline(data)
weighted(data)
unweighted(data)


microbenchmark(
    minus = colSums((fitnesses_t[, which.cutpoints, drop = FALSE] < zenith) *
            weight_lut[seq.int(dimension, length.out = dim)]) - weight_lut[dimension],
    dropdim = colSums((fitnesses_t[-dimension, which.cutpoints, drop = FALSE] < zenith[-dimension]) *
            weight_lut[seq.int(dimension + 1, length.out = dim - 1)]),
    check = "equal"
)
