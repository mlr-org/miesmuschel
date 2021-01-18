
using("checkmate")
source("helper_mutators.R", local = TRUE)

mgauss = MutatorGauss$new()

expect_mutator(mgauss, "MutatorGauss")

p = ps(x = p_dbl(0, 1))

mgauss$prime(p)

set.seed(1)

expect_true(all(mgauss$operate(data.table(x = rep(1, 10)))$x < 1))

mgauss$param_set$values$sdev = .001
expect_true(all(mgauss$operate(data.table(x = rep(1, 10)))$x > 0.9))

mgauss$param_set$values$sdev = 100
expect_true(mean(mgauss$operate(data.table(x = rep(1, 100)))$x < 0.5) > .3)
expect_true(all(mgauss$operate(data.table(x = rep(1, 10)))$x < 1))
expect_true(all(mgauss$operate(data.table(x = rep(1, 10)))$x > 0))

mgauss$param_set$values$truncated_normal = FALSE
expect_true(mean(mgauss$operate(data.table(x = rep(1, 100)))$x == 0) > .3)
expect_true(mean(mgauss$operate(data.table(x = rep(1, 100)))$x == 1) > .3)
mgauss$param_set$values$truncated_normal = TRUE

mgauss$prime(ps(x = p_dbl(0, 100)))
mgauss$param_set$values$sdev = .001
expect_false(all(mgauss$operate(data.table(x = rep(100, 10)))$x > 100 - .1))
mgauss$param_set$values$sdev_is_relative = FALSE
expect_true(all(mgauss$operate(data.table(x = rep(100, 10)))$x > 100 - .1))

mgauss$prime(ps(x = p_dbl(0, 1), y = p_dbl(-1000, 0)))
mgauss$param_set$values$sdev_is_relative = TRUE
mgauss$param_set$values$truncated_normal = FALSE
mgauss$param_set$values$sdev = 100
operated = mgauss$operate(data.table(x = rep(0, 100), y = rep(0, 100)))

expect_true(mean(operated$x == 1) > .3)
expect_true(mean(operated$x == 0) > .3)
expect_true(mean(operated$y == -1000) > .3)
expect_true(mean(operated$y == 0) > .3)

