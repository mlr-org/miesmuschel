
source("setup.R", local = TRUE)

allmutator = MutatorCombination$new(operators = list(numerics = MutatorGauss$new(), discretes = MutatorDiscreteUniform$new()),
  groups = list(numerics = c("ParamInt", "ParamDbl"), discretes = c("ParamLgl", "ParamFct")),
  on_type_not_present = "quiet")

allmutator$param_set$values$discretes.can_mutate_to_same = FALSE

mp = MutatorProxy$new()
expect_equal(mp$param_set$values$operation, MutatorNull$new())
expect_mutator(mp, "MutatorProxy")

mp = MutatorProxy$new()
mp$param_set$values$operation = allmutator
expect_mutator(mp, "MutatorProxy")

p = ps(x = p_dbl(0, 1), y = p_dbl(-1, 0), z = p_lgl())
mp$prime(p)

expect_equal(mp$primed_ps, p)
expect_equal(mp$param_set$values$operation$primed_ps, p)
data = data.table(x = rep(1, 100), y = rep(-1, 100), z = rep(TRUE, 100))

set.seed(1)
operated = mp$operate(data)

expect_equal(operated$z, rep(FALSE, 100))
expect_true(mean(operated$x == 1) > .4)
expect_true(mean(operated$x == 1) < .6)
expect_true(mean(operated$y == -1) > .4)
expect_true(mean(operated$y == -1) < .6)

# change of param_set$values hyperpar propagates
mp$param_set$values$operation$param_set$values$numerics.sdev = 100
mp$param_set$values$operation$param_set$values$discretes.can_mutate_to_same = TRUE
operated = mp$operate(data)
expect_true(mean(operated$z) > .4)
expect_true(mean(operated$z) < .6)
expect_true(mean(operated$x == 0) > .4)
expect_true(mean(operated$x == 0) < .6)
expect_true(mean(operated$y == -1) > .4)
expect_true(mean(operated$y == -1) < .6)


# auto-priming
mp$param_set$values$operation = R6::R6Class("anon", inherit = MutatorDebug,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new(function(n, v, p) if (n == "x") v - 1 else v)
expect_stdout({operated = mp$operate(data)}, "priming")
expect_equal(operated, data.table(x = rep(0, 100), y = rep(-1, 100), z = rep(TRUE, 100)))

expect_stdout({operated = mp$operate(data)}, "^$")
expect_equal(operated, data.table(x = rep(0, 100), y = rep(-1, 100), z = rep(TRUE, 100)))

# auto-priming works after clone
mp = mp$clone(deep = TRUE)
expect_stdout({operated = mp$operate(data)}, "priming")
expect_equal(operated, data.table(x = rep(0, 100), y = rep(-1, 100), z = rep(TRUE, 100)))

# auto-priming works after clone and changing of 'operation' value
mp = mp$clone(deep = TRUE)
mp$param_set$values$operation = R6::R6Class("anon", inherit = MutatorDebug,
  public = list(prime = function(...) { cat("priming\n") ; super$prime(...) }))$new(function(n, v, p) if (n == "x") v - 0.5 else v)
expect_stdout({operated = mp$operate(data)}, "priming")
expect_equal(operated, data.table(x = rep(0.5, 100), y = rep(-1, 100), z = rep(TRUE, 100)))

expect_stdout({operated = mp$operate(data)}, "^$")
