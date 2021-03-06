
source("setup.R", local = TRUE)

srandom = SelectorRandom$new()
srandom$param_set$values$replace = FALSE
expect_selector(srandom, "SelectorRandom", can_oversample = FALSE)
expect_equal(srandom$supported, c("single-crit", "multi-crit"))


srandom = SelectorRandom$new()
srandom$param_set$values$replace = TRUE
expect_selector(srandom, "SelectorRandom", can_oversample = TRUE)


set.seed(1)

p = ps(x = p_dbl(0, 1))
data = data.table(x = rep(1, 100))

srandom$prime(p)
srandom$param_set$values$replace = FALSE
expect_equal(sort(srandom$operate(data, 1:100, 100)), 1:100)

srandom$prime(p)
srandom$param_set$values$replace = TRUE
selected = srandom$operate(data[1:3], 1:3, 100)
expect_true(all(table(selected) < 47))
expect_true(all(table(selected) > 20))

# multiobjective


srandom$prime(p)
srandom$param_set$values$replace = TRUE
selected = srandom$operate(data[1:3], matrix(1:6, ncol = 2), 100)
expect_true(all(table(selected) < 47))
expect_true(all(table(selected) > 20))
