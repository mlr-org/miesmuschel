
source("setup.R", local = TRUE)

munif = MutatorNumericUniform$new()

expect_mutator(munif, "MutatorNumericUniform")

set.seed(1)

p = ps(x = p_int(-2, 2), y = p_dbl(0, 1))

munif$prime(p)
operated = munif$operate(data.table(x = rep(2, 10), y = rep(1, 10)))
expect_true(all(operated$x <= 2L) & all(operated$x >= -2L) & all(operated$y < 1) * all(operated$y > 0))

# works with mutator component-wise maybe
munifmaybe = MutatorCmpMaybe$new(munif)
munifmaybe$param_set$values$p = .5
munifmaybe$prime(p)
operated = munifmaybe$operate(data.table(x = rep(2, 10), y = rep(1, 10)))
expect_numeric(operated$x)
expect_numeric(operated$y)


