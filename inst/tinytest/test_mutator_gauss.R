
source("setup.R", local = TRUE)

mgauss = MutatorGauss$new()
mgauss$param_set$values$sdev_is_relative = TRUE
mgauss$param_set$values$truncated_normal = TRUE
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

# vector-valued sdev

mgauss$param_set$values = list(sdev = c(.1, .0001), sdev_is_relative = TRUE, truncated_normal = TRUE)
mgauss$prime(ps(x = p_dbl(0, 1), y = p_dbl(-1000, 0)))
operated = mgauss$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(mean(operated$x > .5) < .02)
expect_true(mean(operated$x > .1) > .2)
expect_true(mean(operated$x > .1) < .4)
expect_true(mean(operated$y < -.5) < .02)
expect_true(mean(operated$y < -.1) > .2)
expect_true(mean(operated$y < -.1) < .4)

mgauss$param_set$values = list(sdev = c(.1, .0001), sdev_is_relative = TRUE, truncated_normal = TRUE)
mgauss$prime(ps(x = p_dbl(0, 1), y = p_dbl(-1000, 0)))
operated = mgauss$operate(data.table(y = rep(0, 100), x = rep(0, 100)))
expect_true(mean(operated$x > .5) < .02)
expect_true(mean(operated$x > .1) > .2)
expect_true(mean(operated$x > .1) < .4)
expect_true(mean(operated$y < -.5) < .02)
expect_true(mean(operated$y < -.1) > .2)
expect_true(mean(operated$y < -.1) < .4)


mgauss$param_set$values$sdev = c(-1e-10, -1e-20)
operated = mgauss$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(all(operated$x == 0))
expect_true(all(operated$y == 0))
expect_error({mgauss$param_set$values$sdev = -1e-3})

mgauss$prime(ps(x = p_dbl(0, 1)))
expect_error(mgauss$operate(data.table(x = 0)), "sdev must have either length 1, or length of input")

# tolerance in sdev
expect_error({mgauss$param_set$values$sdev = -1e-4}, "sdev: Element 1 is not >=")
mgauss$param_set$values$sdev = -1e-10
expect_equal(mgauss$param_set$values$sdev * 1e10, -1)
# can't use all_equal here because of all_equal's tolerance
expect_true(all(mgauss$operate(data.table(x = rep(0, 10))) == data.table(x = rep(0, 10))))



# Test that two dimensions are treated independently
mgauss$param_set$values = list(sdev = 1, sdev_is_relative = FALSE, truncated_normal = FALSE)
mgauss$prime(ps(x = p_dbl(-2, 2), y = p_dbl(-2, 2)))
operated = mgauss$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(operated[,cor.test(x, y)]$p.value >= .05)

mgauss$param_set$values = list(sdev = 1, sdev_is_relative = FALSE, truncated_normal = TRUE)
mgauss$prime(ps(x = p_dbl(-2, 2), y = p_dbl(-2, 2)))
operated = mgauss$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(operated[,cor.test(x, y)]$p.value >= .05)

# Test integer

mgauss$param_set$values$sdev_is_relative = TRUE
mgauss$param_set$values$sdev = 100
mgauss$param_set$values$truncated_normal = FALSE
mgauss$prime(ps(x = p_int(1, 5)))

operated <- mgauss$operate(data.table(x = rep(1, 100)))$x
expect_true(all(operated >= 1) && all(operated <= 5))
expect_true(mean(operated %in% c(1, 5)) > .9)

mgauss$param_set$values$truncated_normal = TRUE
mgauss$prime(ps(x = p_int(1, 3)))
operated <- mgauss$operate(data.table(x = rep(1, 100)))$x
expect_true(all(operated >= 1) && all(operated <= 3))
expect_true(all(table(operated) / 100 <= .4))


