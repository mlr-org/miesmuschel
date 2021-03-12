
source("setup.R", local = TRUE)

mpoly = MutatorNumericPolynomial$new()
expect_mutator(mpoly, "MutatorNumericPolynomial")

p = ps(x = p_dbl(0, 10))

mpoly$prime(p)

set.seed(1)
# max_mut limits perturbance
mpoly$param_set$values = list(max_mut = 3, n = 0)
x_n0_m3 = mpoly$operate(data.table(x = rep(5, 100)))$x
expect_true(all(abs(x_n0_m3 - 5) <= 3))

# the higher the n the lower the variance
mpoly$param_set$values$n = 0
x_n0 = mpoly$operate(data.table(x = rep(5, 100)))$x
mpoly$param_set$values$n = 4
x_n4 = mpoly$operate(data.table(x = rep(5, 100)))$x
expect_true(var(x_n0) > var(x_n4))

# vector-valued max_mut
mpoly$param_set$values = list(max_mut = c(1, 5), n = 0)
mpoly$prime(ps(x = p_dbl(0, 10), y = p_dbl(0, 10)))
operated = mpoly$operate(data.table(x = rep(5, 100), y = rep(5, 100)))
expect_true(all(abs(operated$x - 5) <= 1))
expect_true(all(abs(operated$x - 5) <= 5))
expect_true(diff(range(operated$x)) < diff(range(operated$y)))

# vector-valued n
mpoly$param_set$values = list(max_mut = 1, n = c(0, 4))
mpoly$prime(ps(x = p_dbl(0, 2), y = p_dbl(0, 2)))
operated = mpoly$operate(data.table(x = rep(1, 100), y = rep(1, 100)))
expect_true(var(operated$x) > var(operated$y))

# test that two dimensions are treated independently
mpoly$param_set$values = list(max_mut = 1, n = 0)
mpoly$prime(ps(x = p_dbl(-2, 2), y = p_dbl(-2, 2)))
operated = mpoly$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(operated[, cor.test(x, y)]$p.value >= .05)

# test integer
mpoly$param_set$values = list(max_mut = 10, n = 0)
mpoly$prime(ps(x = p_int(1, 5)))
operated = mpoly$operate(data.table(x = rep(3, 100)))
expect_true(all(operated$x >= 1) && all(operated$x <= 5))
expect_true(mean(operated$x %in% c(1, 5)) > .5)


