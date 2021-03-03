source("setup.R", local = TRUE)

## general tests
mmaybe = MutatorCmpMaybe$new(MutatorDiscreteUniform$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorCmpMaybe(MutatorDiscreteUniform)")

mmaybe = MutatorCmpMaybe$new(MutatorGauss$new(), MutatorGauss$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorCmpMaybe(MutatorGauss,MutatorGauss)")

mmaybe = MutatorCmpMaybe$new(MutatorGauss$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorCmpMaybe(MutatorGauss)")

madder = MutatorDebug$new(function(n, v, p) v + p$x, "ParamDbl", ps(x = p_dbl()))
set.seed(1)
## MutatorCmpMaybe with Debug Mutator
mmaybe = MutatorCmpMaybe$new(madder)
p = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
mmaybe$prime(p)

mmaybe$param_set$values$cmpmaybe.x = 1
mmaybe$param_set$values$p = 1

operated <- mmaybe$operate(data.table(x = rep(0, 10), y = rep(0, 10)))
expect_true(all(operated$x == 1) && all(operated$y == 1))

mmaybe$param_set$values$p = 0
operated <- mmaybe$operate(data.table(x = rep(0, 10), y = rep(0, 10)))
expect_true(all(operated$x == 0) && all(operated$y == 0))

mmaybe$param_set$values$p = .5
operated = mmaybe$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(mean(operated$x == 1) > .45)
expect_true(mean(operated$x == 1) < .55)
expect_true(mean(operated$y == 1) > .45)
expect_true(mean(operated$y == 1) < .55)

expect_true(mean(operated$x == operated$y) < .55)
expect_true(mean(operated$x == operated$y) > .45)


## MutatorCmpMaybe choosing between two non-null operators
mmaybe = MutatorCmpMaybe$new(madder, madder)
p = ps(x = p_dbl(-1, 1), y = p_dbl(-1, 1))
mmaybe$prime(p)

mmaybe$param_set$values$cmpmaybe_not.x = -1
mmaybe$param_set$values$cmpmaybe.x = 1

mmaybe$param_set$values$p = 0
operated <- mmaybe$operate(data.table(x = rep(0, 10), y = rep(0, 10)))
expect_true(all(operated$x == -1) && all(operated$y == -1))

mmaybe$param_set$values$p = 1
operated <- mmaybe$operate(data.table(x = rep(0, 10), y = rep(0, 10)))
expect_true(all(operated$x == 1) && all(operated$y == 1))

mmaybe$param_set$values$p = .75
operated <- mmaybe$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(mean(operated$x == 1) > .7)
expect_true(mean(operated$x == 1) < .8)
expect_true(mean(operated$y == 1) > .7)
expect_true(mean(operated$y == 1) < .8)

expect_true(mean(operated$x == operated$y) < .675)
expect_true(mean(operated$x == operated$y) > .575)

# MutatorCmpMaybe with vector-valued p

p = ps(x = p_dbl(-1, 1), y = p_dbl(-1, 1), z = p_dbl(-1, 1))
mmaybe$prime(p)
mmaybe$param_set$values$p = c(0, 0.5, 1)
operated <- mmaybe$operate(data.table(x = rep(0, 100), y = rep(0, 100), z = rep(0, 100)))

expect_true(all(operated$x == -1))
expect_true(all(operated$z == 1))

expect_true(mean(operated$y == 1) > .4)
expect_true(mean(operated$y == 1) < .6)


mmaybe$param_set$values$p = c(0, 0.5, 1, 0.5)
expect_error(mmaybe$operate(data.table(x = 0, y = 0, z = 0)), "p must have either length 1, or length of input")
