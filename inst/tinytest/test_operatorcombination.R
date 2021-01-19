
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

expect_set_equal(unlist(lapply(names(ops), function(x) sprintf("%s.%s", x, ops[[x]]$param_set$ids()))), mcom$param_set$ids())

mcom$param_set$values$ParamDbl.sdev = .01
mcom$param_set$values$ParamFct.can_mutate_to_same = FALSE
mcom$param_set$values$ParamFct.p = 1

set.seed(1)
p = ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 10), y1 = p_fct(c("a", "b")))
data = data.table(x1 = c(1, 1), x2 = c(0, 0), y1 = c("a", "b"))
mcom$prime(p)

operated = mcom$operate(data)

expect_equal(operated$y1, c("b", "a"))
expect_true(all(operated$x1 < .999, operated$x2 > .01))
expect_true(all(operated$x1 > .9, operated$x2 < 1))

mcom$param_set$values$ParamDbl.sdev = 1000
mcom$param_set$values$ParamDbl.truncated_normal = FALSE
mcom$param_set$values$ParamFct.p = 0

operated = mcom$operate(data.table(x1 = c(1, 1), x2 = c(0, 0), y1 = c("a", "b")))

expect_equal(operated$y1, c("a", "b"))
expect_true(all(operated$x1 %in% c(0, 1), operated$x2 %in% c(0, 10)))

mcom = MutatorCombination$new(c(ops, list(x1 = MutatorGauss$new())))

mcom$param_set$values$ParamDbl.sdev = .01
mcom$param_set$values$ParamFct.can_mutate_to_same = FALSE
mcom$param_set$values$ParamFct.p = 1
mcom$param_set$values$x1.sdev = 1000
mcom$param_set$values$x1.truncated_normal = FALSE

mcom$prime(p)

operated = mcom$operate(data)
expect_true(all(operated$x1 %in% c(0, 1)))
expect_true(all(operated$x2 > .01))
expect_true(all(operated$x2 < 1))
expect_equal(operated$y1, c("b", "a"))



psnum = ParamDbl$new("x", 0, 1)$rep(4)
pslgl = ParamLgl$new("b")$rep(4)
psfct2 = ParamFct$new("c", c("a", "b"))$rep(4)
psfct3 = ParamFct$new("d", c("a", "b", "c"))$rep(4)

expect_set_equal(MutatorCombination$new(list(g1 = MutatorDiscreteUniform$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$param_classes, c("ParamLgl", "ParamFct"))
expect_set_equal(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorDiscreteUniform$new()),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")))$param_classes, c("ParamDbl", "ParamLgl", "ParamFct"))
expect_set_equal(MutatorCombination$new(list(g1 = MutatorDiscreteUniform$new(), g2 = MutatorDiscreteUniform$new()),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")))$param_classes, c("ParamLgl", "ParamFct"))


expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g1 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2"))), "unique names")
expect_error(MutatorCombination$new(list(ParamDbl = MutatorGauss$new()), groups = list(ParamDbl = c("x_rep_1", "x_rep_2"))), "Special group names")
expect_error(MutatorCombination$new(list(g1 = MutatorDiscreteUniform$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Must be .*ParamFct.* but is .*ParamDbl")
expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorDiscreteUniform$new()),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")))$prime(psnum), "Must be .*ParamFct.* but is .*ParamDbl")

expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3")))$prime(psnum), "No operators for.* x_rep_4 .* ParamDbl")

expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4", "x_rep_4"))), "duplicated values")
expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_2", "x_rep_3", "x_rep_4"))), "more than one group: x_rep_2")

expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new(), x_rep_2 = MutatorGauss$new()),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4"))), "x_rep_2.* own operator must not be present in group")

expect_error(MutatorCombination$new(list()), "Must have length >= 1")
expect_error(MutatorCombination$new(list(x = RecombinatorNull$new())), "May only contain.*Mutator")

# recombinator only: check for different n_in, n_out

expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new()),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")),
  strategies = list(g1 = 1)), "May only contain.*function.*has type.*numeric")

expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new()),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")),
  strategies = list(g3.sdev = identity)), "Strategy for g3.sdev which is not an operator parameter")


expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Named operators g2 have no corresponding dimension")
expect_warning(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_name_not_present = "warn")$prime(psnum), "Named operators g2 have no corresponding dimension")
expect_silent(MutatorCombination$new(list(g1 = MutatorGauss$new(), g2 = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_name_not_present = "quiet")$prime(psnum))


expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), ParamLgl = MutatorDiscreteUniform$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_type_not_present = "stop")$prime(psnum), "Operators for types / special groups ParamLgl have no corresponding dimensions")
expect_error(MutatorCombination$new(list(g1 = MutatorGauss$new(), ParamDbl = MutatorGauss$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_type_not_present = "stop")$prime(psnum), "Operators for types / special groups ParamDbl have no corresponding dimensions")
expect_warning(MutatorCombination$new(list(g1 = MutatorGauss$new(), ParamLgl = MutatorDiscreteUniform$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Operators for types / special groups ParamLgl have no corresponding dimensions")
expect_silent(MutatorCombination$new(list(g1 = MutatorGauss$new(), ParamLgl = MutatorDiscreteUniform$new()), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_type_not_present = "quiet")$prime(psnum))














# TODO what happens when op doesn't support type?
# TODO what happens when using fct op on logical / the other way?
# TODO strategies
