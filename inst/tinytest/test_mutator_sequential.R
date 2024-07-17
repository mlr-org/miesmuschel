source("setup.R", local = TRUE)

## Test MutatorSequential with one mutator
mseq = MutatorSequential$new(list(MutatorDiscreteUniform$new()))
mseq$param_set$values$mutator_1$p = 0.5
expect_mutator(mseq, "MutatorSequential(MutatorDiscreteUniform)")

## Test MutatorSequential with two mutators
mseq = MutatorSequential$new(list(MutatorGauss$new(sdev = 0.1), MutatorDiscreteUniform$new()))
mseq$param_set$values$mutator_1$sdev = 0.1
mseq$param_set$values$mutator_2$p = 0.5
expect_mutator(mseq, "MutatorSequential(MutatorGauss,MutatorDiscreteUniform)")

## Test sequential mutation
mseq = MutatorSequential$new(list(
  MutatorDebug$new(function(n, v, p) v + p$x, "ParamDbl", ps(x = p_dbl())),
  MutatorDebug$new(function(n, v, p) v + p$y, "ParamDbl", ps(y = p_dbl()))
))
mseq$param_set$values$mutator_1$x = 1
mseq$param_set$values$mutator_2$y = 1

p = ps(x = p_dbl(0, 10), y = p_dbl(0, 10))
mseq$prime(p)

operated <- mseq$operate(data.table(x = rep(0, 10), y = rep(0, 10)))
expect_true(all(operated$x == 2) && all(operated$y == 2))

## Test sequential mutation with vector-valued parameters
mseq = MutatorSequential$new(list(
  MutatorDebug$new(function(n, v, p) v + p$x, "ParamDbl", ps(x = p_dbl())),
  MutatorDebug$new(function(n, v, p) v + p$y, "ParamDbl", ps(y = p_dbl()))
))
mseq$param_set$values$mutator_1$x = c(1, 0.5, -1)
mseq$param_set$values$mutator_2$y = c(2, -0.5, 0)

p = ps(x = p_dbl(-10, 10), y = p_dbl(-10, 10), z = p_dbl(-10, 10))
mseq$prime(p)

operated <- mseq$operate(data.table(x = rep(0, 3), y = rep(0, 3), z = rep(0, 3)))
expect_true(all(operated$x == c(3, 0, -1)) && all(operated$y == c(3, 0, -1)) && all(operated$z == c(2, -0.5, 0)))

## Test error message for mismatched parameter lengths
mseq$param_set$values$mutator_1$x = c(1, 0.5, -1)
mseq$param_set$values$mutator_2$y = c(2, -0.5)
expect_error(mseq$operate(data.table(x = rep(0, 3), y = rep(0, 3), z = rep(0, 3))), "x and y must have the same length")
