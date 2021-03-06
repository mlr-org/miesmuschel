
source("setup.R", local = TRUE)

sbest = SelectorBest$new()
expect_selector(sbest, "SelectorBest", can_oversample = TRUE)
expect_equal(sbest$supported, "single-crit")

p = ps(x = p_dbl(0, 1))
data = data.table(x = rep(1, 100))

sbest$prime(p)
expect_equal(sbest$operate(data, seq(0, 1, length.out = 100), 100), 100:1)
expect_equal(sbest$operate(data, seq(1, 0, length.out = 100), 100), 1:100)
expect_equal(sbest$operate(data, seq(0, 1, length.out = 100), 200), c(100:1, 100:1))
expect_equal(sbest$operate(data, seq(0, 1, length.out = 100), 150), c(100:1, 100:51))

