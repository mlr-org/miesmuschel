
using("checkmate")
source("helper_mutators.R", local = TRUE)
source("helper_recombinators.R", local = TRUE)

ops = list(ParamDbl = MutatorGauss$new(), ParamFct = MutatorDiscreteUniform$new())

mcom = MutatorCombination$new(ops, on_name_not_present = "quiet", on_type_not_present = "quiet")

expect_equal(mcom$on_name_not_present, "quiet")
expect_equal(mcom$on_type_not_present, "quiet")

expect_mutator(mcom, "MutatorCombination(MutatorGauss, MutatorDiscreteUniform)")


mcom = MutatorCombination$new(ops)

expect_set_equal(mcom$param_classes, c("ParamDbl", "ParamFct"))
expect_equal(mcom$operators, ops)
expect_equal(mcom$groups, list())
expect_equal(mcom$strategies, list())
expect_equal(mcom$endomorphism, TRUE)
expect_equal(mcom$on_name_not_present, "stop")
expect_equal(mcom$on_type_not_present, "warn")
expect_equal(mcom$binary_fct_as_logical, FALSE)

names(mcom)



# TODO what happens when op doesn't support type?
# TODO what happens when using fct op on logical / the other way?
