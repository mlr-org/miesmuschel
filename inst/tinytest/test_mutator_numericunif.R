
source("setup.R", local = TRUE)

munif = MutatorNumericUniform$new()

expect_mutator(munif, "MutatorNumericUniform")

set.seed(1)

p = ps(x = p_int(0, 2), y = p_dbl(0, 1))

munif$prime(p)
operated = munif$operate(data.table(x = rep(2, 1000), y = rep(1, 1000)))
expect_true(all(operated$x <= 2L) & all(operated$x >= -2L) & all(operated$y < 1) * all(operated$y > 0))
x_prob = prop.table(table(operated$x))   # correct uniform distribution
expect_true(all(x_prob < 0.36 & x_prob > 0.3))
y_prob = ecdf(operated$y)
y_seq = seq(0, 1, length.out = 11)
expect_true(all(abs(y_prob(y_seq) - y_seq) < 0.05))  # correct uniform distribution

# works with mutator component-wise maybe
munifmaybe = MutatorCmpMaybe$new(munif)
munifmaybe$param_set$values$p = .5
munifmaybe$prime(p)
operated = munifmaybe$operate(data.table(x = rep(2, 10), y = rep(1, 10)))
expect_numeric(operated$x)
expect_numeric(operated$y)


