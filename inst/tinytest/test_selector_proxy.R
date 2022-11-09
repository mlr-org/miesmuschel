
source("setup.R", local = TRUE)

sp = SelectorProxy$new()
sp$param_set$values$operation = SelectorRandom$new()
expect_selector(sp, "SelectorProxy", can_oversample = FALSE)

sp = SelectorProxy$new()
sp$param_set$values$operation = SelectorRandom$new()
sp$param_set$values$operation$param_set$values$sample_unique = "no"
expect_selector(sp, "SelectorProxy", can_oversample = TRUE)

p = ps(x = p_dbl(0, 1), y = p_dbl(-1, 0))

sp$prime(p)

expect_equal(sp$primed_ps, p)
data = data.table(x = rep(1, 100), y = seq(-1, 0, length.out = 100))

set.seed(1)
expect_true(!isTRUE(all.equal(sp$operate(data, seq(0, 1, length.out = 100), 100), 100:1)) && !isTRUE(all.equal(sp$operate(data, seq(0, 1, length.out = 100), 100), 100:1)))

# auto-priming
sp$param_set$values$operation = R6::R6Class("anon", inherit = SelectorBest,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new()
sp$param_set$values$operation$param_set$values$shuffle_selection = FALSE
expect_stdout({operated = sp$operate(data, seq(0, 1, length.out = 100), 100)}, "^priming$")
expect_true(all.equal(operated, 100:1))

expect_stdout({operated = sp$operate(data, seq(0, 1, length.out = 100), 100)}, "^$")
expect_true(all.equal(operated, 100:1))


# auto-priming works after clone
sp = sp$clone(deep = TRUE)
expect_stdout({operated = sp$operate(data, seq(0, 1, length.out = 100), 100)}, "^priming$")
expect_true(all.equal(operated, 100:1))

expect_stdout({operated = sp$operate(data, seq(0, 1, length.out = 100), 100)}, "^$")
expect_true(all.equal(operated, 100:1))

# auto-priming works after clone and changing of 'operation' value
sp = sp$clone(deep = TRUE)
sp$param_set$values$operation = R6::R6Class("anon", inherit = SelectorBest,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new()
sp$param_set$values$operation$param_set$values$shuffle_selection = FALSE
expect_stdout({operated = sp$operate(data, seq(0, 1, length.out = 100), 100)}, "^priming$")
expect_true(all.equal(operated, 100:1))

expect_stdout({operated = sp$operate(data, seq(0, 1, length.out = 100), 100)}, "^$")
expect_true(all.equal(operated, 100:1))

