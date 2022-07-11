
source("setup.R", local = TRUE)



rp = RecombinatorProxy$new()
expect_null(rp$param_set$values$operation)
rp$param_set$values$operation = RecombinatorNull$new(n_indivs_in = 2)
expect_recombinator(rp, "RecombinatorProxy")

rp = RecombinatorProxy$new()
rp$param_set$values$operation = RecombinatorNull$new(n_indivs_in = 1)
expect_recombinator(rp, "RecombinatorProxy")

rp = RecombinatorProxy$new()
rp$param_set$values$operation = RecombinatorCrossoverUniform()
expect_recombinator(rp, "RecombinatorProxy")

rp$param_set$values$operation$param_set$values$p = 1

p = ps(x = p_dbl(0, 1), y = p_dbl(-1, 0), z = p_lgl())
rp$prime(p)
expect_equal(rp$primed_ps, p)
expect_equal(rp$param_set$values$operation$primed_ps, p)

data = data.table(x = c(0, 1, 1, 0), y = c(0, -1, 0, -1), z = c(TRUE, FALSE, TRUE, TRUE))


set.seed(1)
operated = rp$operate(data)
data_recombined = data.table(x = c(1, 0, 0, 1), y = c(-1, 0, -1, 0), z = c(FALSE, TRUE, TRUE, TRUE))
expect_equal(operated, data_recombined)

# change of param_set$values hyperpar propagates
rp$param_set$values$operation$param_set$values$p = 0
expect_equal(rp$operate(data), data)


# auto-priming
rp$param_set$values$operation = R6::R6Class("anon", inherit = RecombinatorDebug,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new(function(n, v, p) if (n == "x") v * 0.1 + 0.5 else v)
expect_stdout({operated = rp$operate(data)}, "priming")
expect_equal(operated, copy(data)[, x := c(.5, .6, .6, .5)])

expect_stdout({operated = rp$operate(data)}, "^$")
expect_equal(operated, copy(data)[, x := c(.5, .6, .6, .5)])

# auto-priming works after clone
rp = rp$clone(deep = TRUE)
expect_stdout({operated = rp$operate(data)}, "priming")
expect_equal(operated, copy(data)[, x := c(.5, .6, .6, .5)])

expect_stdout({operated = rp$operate(data)}, "^$")
expect_equal(operated, copy(data)[, x := c(.5, .6, .6, .5)])

# auto-priming works after clone and changing of 'operation' value
rp = rp$clone(deep = TRUE)
rp$param_set$values$operation = R6::R6Class("anon", inherit = RecombinatorDebug,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new(function(n, v, p) if (n == "x") -v * 0.1 + 0.5 else v)
expect_stdout({operated = rp$operate(data)}, "priming")
expect_equal(operated, copy(data)[, x := c(.5, .4, .4, .5)])

expect_stdout({operated = rp$operate(data)}, "^$")
expect_equal(operated, copy(data)[, x := c(.5, .4, .4, .5)])


# n_indivs_in /  n_indivs_out things
expect_error({rp$param_set$values$operation = RecombinatorNull$new(n_indivs_in = 2, n_indivs_out = 1)},
  "n_indivs_in is a divisor of 2, and where n_indivs_in / n_indivs_out must be 2 / 2")

expect_error({rp$param_set$values$operation = RecombinatorNull$new(n_indivs_in = 4)},
  "n_indivs_in is a divisor of 2, and where n_indivs_in / n_indivs_out must be 2 / 2")

rpminus = RecombinatorProxy$new(n_indivs_in = 2, n_indivs_out = 1)
rpminus$param_set$values$operation = RecombinatorNull$new(n_indivs_in = 2, n_indivs_out = 1)
rpminus$prime(p)

expect_equal(rpminus$operate(data), data[c(1, 3)])

rpminus$param_set$values$operation = RecombinatorCrossoverUniform(keep_complement = FALSE)
rpminus$param_set$values$operation$param_set$values$p = 1

expect_equal(rpminus$operate(data), data[c(2, 4)])
