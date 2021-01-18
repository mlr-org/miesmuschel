
using("checkmate")
source("helper_mutators.R", local = TRUE)

## general tests
mmaybe = MutatorMaybe$new(MutatorDiscreteUniform$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorMaybe(MutatorDiscreteUniform)")

mmaybe = MutatorMaybe$new(MutatorGauss$new(), MutatorGauss$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorMaybe(MutatorGauss,MutatorGauss)")

mmaybe = MutatorMaybe$new(MutatorGauss$new())
mmaybe$param_set$values$p = 0.5
expect_mutator(mmaybe, "MutatorMaybe(MutatorGauss)")


## Maybe: MutatorGauss
p = ps(x = p_dbl(0, 1))
mmaybe$prime(p)

mmaybe$param_set$values$maybe.sdev = 100
mmaybe$param_set$values$maybe.truncated_normal = FALSE
mmaybe$param_set$values$p = 1

expect_true(mean(mmaybe$operate(data.table(x = rep(.5, 100)))$x == 0) > .3)
expect_true(mean(mmaybe$operate(data.table(x = rep(.5, 100)))$x == 1) > .3)

mmaybe$param_set$values$p = 0
expect_true(all(mmaybe$operate(data.table(x = rep(.5, 100)))$x == .5))

mmaybe$param_set$values$p = .5
operated = mmaybe$operate(data.table(x = rep(.5, 1000)))
expect_true(mean(operated$x == 1) > .2)
expect_true(mean(operated$x == 1) < .3)

expect_true(mean(operated$x == 0) > .2)
expect_true(mean(operated$x == 0) < .3)

expect_true(mean(operated$x == 0.5) > .45)
expect_true(mean(operated$x == 0.5) < .55)



## MutatorMaybe choosing between two non-null operators
mmaybe = MutatorMaybe$new(MutatorGauss$new(), MutatorGauss$new())
p = ps(x = p_dbl(0, 1))
mmaybe$prime(p)

mmaybe$param_set$values$maybe_not.sdev = 100
mmaybe$param_set$values$maybe_not.truncated_normal = FALSE

mmaybe$param_set$values$maybe.sdev = 0

mmaybe$param_set$values$p = 0
expect_true(mean(mmaybe$operate(data.table(x = rep(.5, 100)))$x == 0) > .3)
expect_true(mean(mmaybe$operate(data.table(x = rep(.5, 100)))$x == 1) > .3)

mmaybe$param_set$values$p = 1
expect_true(all(mmaybe$operate(data.table(x = rep(.5, 100)))$x == .5))

mmaybe$param_set$values$p = .75
operated = mmaybe$operate(data.table(x = rep(.5, 1000)))
expect_true(mean(operated$x == 1) > .1)
expect_true(mean(operated$x == 1) < .15)

expect_true(mean(operated$x == 0) > .1)
expect_true(mean(operated$x == 0) < .15)

expect_true(mean(operated$x == 0.5) > .7)
expect_true(mean(operated$x == 0.5) < .8)


