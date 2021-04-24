source("setup.R", local = TRUE)

op = MiesOperator$new()

expect_read_only(op, c("param_classes", "endomorphism", "is_primed", "param_set", "packages"))
op$prime(ps(a = p_dbl()))
expect_error(op$operate(data.table(a = 1)), "\\.operate needs to be implemented")

mt = Mutator$new()
expect_read_only(mt, c("param_classes", "endomorphism", "is_primed", "param_set", "packages"))
mt$prime(ps(a = p_dbl()))
expect_error(mt$operate(data.table(a = 1)), "\\.mutate needs to be implemented")
mtn = MutatorNumeric$new()
mtn$prime(ps(a = p_dbl()))
expect_error(mtn$operate(data.table(a = 1)), "\\.mutate_numeric needs to be implemented")
mtd = MutatorDiscrete$new()
mtd$prime(ps(a = p_lgl()))
expect_error(mtd$operate(data.table(a = TRUE)), "\\.mutate_discrete needs to be implemented")

sl = Selector$new()
expect_read_only(sl, c("param_classes", "endomorphism", "supported", "is_primed", "param_set", "packages"))
sl$prime(ps(a = p_dbl()))
expect_error(sl$operate(data.table(a = 1), 1, 1), "\\.select needs to be implemented")

rc = Recombinator$new()
expect_read_only(rc, c("param_classes", "endomorphism", "n_indivs_in", "n_indivs_out", "is_primed", "param_set", "packages"))
rc$prime(ps(a = p_dbl()))
expect_error(rc$operate(data.table(a = c(1, 1))), "\\.recombine needs to be implemented")

mttwo = MutatorDebug$new(function(n, v, p) 1:2)
mttwo$prime(ps(a = p_dbl()))
expect_equal(mttwo$operate(data.table(a = c(0, 0))), data.table(a = 1:2))
expect_error(mttwo$operate(data.table(a = c(0, 0, 0))), "Mutation of 3 input rows resulted in 2 output rows")

rctwo = RecombinatorDebug$new(function(n, v, p) 1:2)
rctwo$prime(ps(a = p_dbl()))
expect_equal(rctwo$operate(data.table(a = c(0, 0))), data.table(a = 1:2))
rcthree = RecombinatorDebug$new(function(n, v, p) 1:3)
rcthree$prime(ps(a = p_dbl()))
expect_error(rcthree$operate(data.table(a = c(0, 0))), "Must have exactly 2 rows, but has 3 rows")

ft = Filtor$new()
expect_read_only(ft, c("param_classes", "endomorphism", "is_primed", "param_set", "supported"))
ft$prime(ps(a = p_dbl()))
expect_error(ft$needed_input(1), "\\.needed_input needs to be implemented by inheriting class")
ft2 = R6::R6Class("FiltorDummy", inherit = Filtor, private = list(.needed_input = function(x) x))$new()
ft2$prime(ps(a = p_dbl()))
expect_error(ft2$operate(data.table(a = 1), data.table(a = 1), 1, 1), "\\.filter needs to be implemented by inheriting class")


