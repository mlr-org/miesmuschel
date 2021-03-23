
source("setup.R", local = TRUE)

mpoly = MutatorNumericPolynomial$new()
expect_mutator(mpoly, "MutatorNumericPolynomial")

p = ps(x = p_dbl(0, 10))

mpoly$prime(p)

set.seed(1)

# the higher the n the lower the variance
mpoly$param_set$values$n = 0
x_n0 = mpoly$operate(data.table(x = rep(5, 100)))$x
mpoly$param_set$values$n = 4
x_n4 = mpoly$operate(data.table(x = rep(5, 100)))$x
expect_true(var(x_n0) > var(x_n4))

# vector-valued n
mpoly$param_set$values = list(n = c(0, 4, 0))
mpoly$prime(ps(x = p_dbl(0, 2), y = p_dbl(0, 2)))
expect_error(mpoly$operate(data.table(x = rep(1, 100), y = rep(1, 100))),
  pattern = "n must have either length 1, or length of input")

mpoly$param_set$values = list(n = c(0, 4))
mpoly$prime(ps(x = p_dbl(0, 2), y = p_dbl(0, 2)))
operated = mpoly$operate(data.table(x = rep(1, 100), y = rep(1, 100)))
expect_true(var(operated$x) > var(operated$y))

# test that two dimensions are treated independently
mpoly$param_set$values = list(n = 0)
mpoly$prime(ps(x = p_dbl(-2, 2), y = p_dbl(-2, 2)))
operated = mpoly$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(operated[, cor.test(x, y)]$p.value >= .05)

# test integer
mpoly$param_set$values = list(n = 0)  # n = 0 is unif
mpoly$prime(ps(x = p_int(1, 5)))
operated = mpoly$operate(data.table(x = rep(3, 1000)))
expect_true(all(operated$x >= 1) && all(operated$x <= 5))
emp_ecdf = ecdf(operated$x)
expect_true(all(abs(emp_ecdf(1:5) - c(0.2, 0.4, 0.6, 0.8, 1)) < 0.05))

