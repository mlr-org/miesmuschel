source("setup.R", local = TRUE)

fnull = FiltorNull$new()
expect_filtor(fnull, "FiltorNull")
expect_equal(fnull$supported, c("single-crit", "multi-crit"))

p = ps(x = p_dbl(0, 1), y = p_dbl(-1, 0))

fnull$prime(p)

expect_equal(fnull$primed_ps, p)
data = data.table(x = seq(0, 1, length.out = 10), y = seq(-1, 0, length.out = 10))

expect_equal(fnull$operate(data, data, seq(0, 1, length.out = 10), 5), 1:5)
expect_equal(fnull$needed_input(4), 4)


# multiobjective

expect_equal(fnull$operate(data, data, matrix(1:20, ncol = 2), 5), 1:5)
