source("setup.R", local = TRUE)

## general tests
mmaybe = MutatorMaybe$new(MutatorDiscreteUniform$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorMaybe(MutatorDiscreteUniform)")

mmaybe = MutatorMaybe$new(mut("gauss", sdev = 0.1), mut("gauss", sdev = 0.2))
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorMaybe(MutatorGauss,MutatorGauss)")

mmaybe = MutatorMaybe$new(mut("gauss", sdev = 0.3))
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorMaybe(MutatorGauss)")


set.seed(1)

madder <- MutatorDebug$new(function(n, v, p) v + p$x, "ParamDbl", ps(x = p_dbl()))
set.seed(1)
## MutatorMaybe with Debug Mutator
mmaybe = MutatorMaybe$new(madder)
p = ps(x = p_dbl(0, 1), y = p_dbl(0, 1))
mmaybe$prime(p)

mmaybe$param_set$values$maybe.x = 1
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

expect_true(all(operated$x == operated$y))

## MutatorMaybe choosing between two non-null operators
mmaybe = MutatorMaybe$new(madder, madder)
p = ps(x = p_dbl(-1, 1), y = p_dbl(-1, 1))
mmaybe$prime(p)

mmaybe$param_set$values$maybe_not.x = -1
mmaybe$param_set$values$maybe.x = 1

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

expect_true(all(operated$x == operated$y))

counter <- MutatorDebug$new(function(n, v, p) { if (n == "x") { p$env$x = p$env$x + length(v) }; v }, "ParamDbl", ps(env = p_uty()))
mmaybe = MutatorMaybe$new(counter, counter)

cnt = as.environment(list(x = 0))
mmaybe$param_set$values$maybe.env = cnt
cnt_not = as.environment(list(x = 0))
mmaybe$param_set$values$maybe_not.env = cnt_not

mmaybe$param_set$values$p = .75
mmaybe$prime(p)
operated = mmaybe$operate(data.table(x = rep(0, 100), y = rep(0, 100)))
expect_true(all(operated$x == 0) && all(operated$y == 0))
expect_true(cnt$x > 70)
expect_true(cnt$x < 80)
expect_true(cnt_not$x + cnt$x == 100)
