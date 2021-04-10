source("setup.R", local = TRUE)

fp = FiltorProxy$new()
fp$param_set$values$operation = FiltorNull$new()
expect_filtor(fp, "FiltorProxy")

p = ps(x = p_dbl(0, 1), y = p_dbl(-1, 0))

fp$prime(p)

expect_equal(fp$primed_ps, p)
data = data.table(x = seq(0, 1, length.out = 10), y = seq(-1, 0, length.out = 10))

set.seed(1)
expect_equal(fp$operate(data, data, seq(0, 1, length.out = 10), 5), 1:5)

# auto-priming
fp$param_set$values$operation = R6::R6Class("anon", inherit = FiltorNull,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new()
expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^priming$")

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^$")
expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^$")
expect_equal(ni, 5)


expect_true(all.equal(operated, 1:5))
fp$param_set$values$operation = R6::R6Class("anon2", inherit = FiltorNull,
  public = list(prime = function(...) { cat("priming2\n") ; super$prime(...) }))$new()
expect_stdout({ni = fp$needed_input(5)}, "^priming2$")
expect_equal(ni, 5)

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^$")
expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^$")
expect_equal(ni, 5)



# auto-priming works after clone
fp = fp$clone(deep = TRUE)

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^priming2$")

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^$")
expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^$")
expect_equal(ni, 5)

fp = fp$clone(deep = TRUE)

expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^priming2$")
expect_equal(ni, 5)

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^$")
expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^$")
expect_equal(ni, 5)


# auto-priming works after clone and changing of 'operation' value
fp = fp$clone(deep = TRUE)
fp$param_set$values$operation = R6::R6Class("anon", inherit = FiltorNull,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new()

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^priming$")

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^$")
expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^$")
expect_equal(ni, 5)

fp = fp$clone(deep = TRUE)
fp$param_set$values$operation = R6::R6Class("anon2", inherit = FiltorNull,
  public = list(prime = function(...) { cat("priming2\n") ; super$prime(...) }))$new()

expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^priming2$")
expect_equal(ni, 5)

expect_stdout({operated = fp$operate(data, data, seq(0, 1, length.out = 10), 5)}, "^$")
expect_true(all.equal(operated, 1:5))
expect_stdout({ni = fp$needed_input(5)}, "^$")
expect_equal(ni, 5)


# changing operator actually does something different
fd = FiltorDebug$new(function(v, k, f, n, p) rev(seq_len(n)), function(o) o + 1)

fp$param_set$values$operation = fd

expect_equal(fp$needed_input(5), 6)

expect_equal(fp$operate(data, data, seq(0, 1, length.out = 10), 5), 5:1)


