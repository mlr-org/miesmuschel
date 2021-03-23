
source("setup.R", local = TRUE)

rsbx = RecombinatorSimulatedBinaryCrossover$new()
expect_recombinator(rsbx, "RecombinatorSimulatedBinaryCrossover")

set.seed(1)

p = ps(x = p_dbl(-10, 10), y = p_dbl(-10, 10), z = p_dbl(-10, 10))
rsbx$prime(p)
vals = generate_design_random(p, 10)$data

rsbx$param_set$values$p = 0
expect_identical(vals, rsbx$operate(vals))

rsbx$param_set$values$p = 1
rsbx$param_set$values$n = c(0.1, 1, 10)

data = data.table(x = rep(c(-10, 10), 5), y = rep(c(-10, 10), 5), z = rep(c(-10, 10), 5))
recombined = rsbx$operate(data)
expect_identical(names(sort(apply(abs(data) - abs(recombined), MARGIN = 2L, FUN = sd))),
  c("z", "y", "x"))

