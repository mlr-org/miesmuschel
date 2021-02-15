
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
# expect_equal(mcom$operators, ops)  $operators have different set_id
expect_equal(mcom$groups, list())
expect_equal(mcom$adaptions, list())
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

MutatorClass = R6Class("MutatorNull",
  inherit = Mutator,
  public = list(
    innitialize = function(cls) {
      super$initialize(cls)
    }
  ),
  private = list(
    .mutate = function(values) values
  )
)

mut_int = MutatorClass$new("ParamInt")
mut_dbl = MutatorClass$new("ParamDbl")
mut_fct = MutatorClass$new("ParamFct")
mut_lgl = MutatorClass$new("ParamLgl")
mut_lf = MutatorClass$new(c("ParamLgl", "ParamFct"))

expect_set_equal(MutatorCombination$new(list(g1 = mut_lf), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$param_classes, c("ParamLgl", "ParamFct"))
expect_set_equal(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_lf),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")))$param_classes, c("ParamDbl", "ParamLgl", "ParamFct"))
expect_set_equal(MutatorCombination$new(list(g1 = mut_lf, g2 = mut_lf),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")))$param_classes, c("ParamLgl", "ParamFct"))

expect_error(MutatorCombination$new(list(g1 = mut_dbl), groups = list(g1 = "g2", g2 = "x_rep_1")), "No recursive groups allowed: g2")
expect_error(MutatorCombination$new(list(g1 = mut_dbl), groups = list(g1 = "x_rep_1", g2 = "x_rep_2")), "No operator for group.*g2")

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g1 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2"))), "unique names")
expect_error(MutatorCombination$new(list(ParamDbl = mut_dbl), groups = list(ParamDbl = c("x_rep_1", "x_rep_2"))), "Special group names")
expect_error(MutatorCombination$new(list(g1 = mut_lf), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Must be .*ParamFct.* but is .*ParamDbl")
expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_lf),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")))$prime(psnum), "Must be .*ParamFct.* but is .*ParamDbl")

expect_error(MutatorCombination$new(list(g1 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3")))$prime(psnum), "No operators for.* x_rep_4 .* ParamDbl")

expect_error(MutatorCombination$new(list(g1 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4", "x_rep_4"))), "duplicated values")
expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_2", "x_rep_3", "x_rep_4"))), "more than one group: x_rep_2")

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl, x_rep_2 = mut_dbl),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4"))), "x_rep_2.* own operator must not be present in group")

expect_error(MutatorCombination$new(list()), "Must have length >= 1")
expect_error(MutatorCombination$new(list(x = RecombinatorNull$new())), "May only contain.*Mutator")

# recombinator only: check for different n_in, n_out

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")),
  adaptions = list(g1 = 1)), "May only contain.*function.*has type.*numeric")

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")),
  adaptions = list(g3.sdev = identity)), "Adaption for g3.sdev which is not an operator parameter")

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Named operators g2 have no corresponding dimension")
expect_warning(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_name_not_present = "warn")$prime(psnum), "Named operators g2 have no corresponding dimension")
expect_silent(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_name_not_present = "quiet")$prime(psnum))


expect_error(MutatorCombination$new(list(g1 = mut_dbl, ParamLgl = mut_lf), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_type_not_present = "stop")$prime(psnum), "Operators for types / special groups ParamLgl have no corresponding dimensions")
expect_error(MutatorCombination$new(list(g1 = mut_dbl, ParamDbl = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_type_not_present = "stop")$prime(psnum), "Operators for types / special groups ParamDbl have no corresponding dimensions")
expect_warning(MutatorCombination$new(list(g1 = mut_dbl, ParamLgl = mut_lf), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Operators for types / special groups ParamLgl have no corresponding dimensions")
expect_silent(MutatorCombination$new(list(g1 = mut_dbl, ParamLgl = mut_lf), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_type_not_present = "quiet")$prime(psnum))

expect_silent(MutatorCombination$new(list(ParamFct = mut_lf))$prime(ps(x = p_fct(c("a", "b")))))
expect_silent(MutatorCombination$new(list(ParamLgl = mut_lf), binary_fct_as_logical = TRUE)$prime(ps(x = p_fct(c("a", "b")))))
expect_error(MutatorCombination$new(list(ParamFct = mut_lf),
  binary_fct_as_logical = TRUE, on_type_not_present = "quiet")$prime(ps(x = p_fct(c("a", "b")))), "No operators for dimensions x of types ParamLgl")
expect_warning(MutatorCombination$new(list(g1 = mut_lf, g2 = mut_lf),
  groups = list(g1 = c("ParamAny", "c_rep_1"), g2 = "ParamFct"))$prime(psfct2), "special groups ParamAny have no corresponding dimension")
expect_warning(MutatorCombination$new(list(g1 = mut_lf, g2 = mut_lf),
  groups = list(g1 = c("ParamAny", paste0("c_rep_", 1:4)), g2 = "ParamFct"))$prime(psfct2), "special groups ParamAny, ParamFct have no corresponding dimension")
expect_warning(MutatorCombination$new(list(g1 = mut_lf, g2 = mut_lf),
  groups = list(g1 = paste0("c_rep_", 1:4), g2 = "ParamAny"))$prime(psfct2), "special groups ParamAny have no corresponding dimension")


expect_error(MutatorCombination$new(list(ParamFct = mut_int)), "ParamFct does not support the type")
expect_error(MutatorCombination$new(list(g1 = mut_int), groups = list(g1 = c("x_rep_1", "ParamFct"))), "g1 does not support type.*ParamFct")
expect_warning(MutatorCombination$new(list(g1 = mut_fct), groups = list(g1 = c("x_rep_1", "ParamFct")))$prime(ps(x_rep_1 = p_fct(c("a", "b")))), "ParamFct have no corresponding dimensions")
expect_silent(MutatorCombination$new(list(g1 = mut_fct), groups = list(g1 = c("x_rep_1", "ParamFct")))$prime(ps(x_rep_1 = p_fct(c("a", "b")), x_rep_2 = p_fct(c("a", "b")))))





# It could be argued that the following should work, but let's live without that for now
expect_error(MutatorCombination$new(list(ParamLgl = mut_fct), binary_fct_as_logical = TRUE, on_type_not_present = "quiet"), "ParamLgl does not support the type")

expect_error(MutatorCombination$new(list(ParamLgl = mut_lgl, ParamFct = mut_fct), binary_fct_as_logical = TRUE, on_type_not_present = "quiet")$prime(ps(x = p_fct(c("a", "b")))), "Must be a subset of .*ParamLgl.* but is .*ParamFct")

expect_warning(MutatorCombination$new(list(ParamLgl = mut_lgl, ParamFct = mut_fct), binary_fct_as_logical = FALSE)$prime(ps(x = p_fct(c("a", "b")))), "ParamLgl have no corresponding dimension")

expect_warning(MutatorCombination$new(list(ParamLgl = mut_lf, ParamFct = mut_fct), binary_fct_as_logical = TRUE)$prime(ps(x = p_fct(c("a", "b")))), "ParamFct have no corresponding dimension")


expect_set_equal(MutatorCombination$new(list(g1 = mut_lf), groups = list(g1 = "ParamFct"))$param_classes, "ParamFct")
expect_set_equal(MutatorCombination$new(list(g1 = mut_lf), groups = list(g1 = c("ParamFct", "x_rep_1")))$param_classes, c("ParamFct", "ParamLgl"))

expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, ParamAny = mut_dbl))$param_classes, c("ParamLgl", "ParamDbl"))
expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, g1 = mut_dbl), groups = list(g1 = "ParamAny"))$param_classes, c("ParamLgl", "ParamDbl"))
expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, ParamFct = mut_lf, ParamDbl = mut_dbl, ParamInt = mut_int, ParamAny = mut_dbl))$param_classes, c("ParamLgl", "ParamDbl", "ParamFct", "ParamInt"))
expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, ParamFct = mut_lf, ParamDbl = mut_dbl, ParamInt = mut_int, g1 = mut_dbl), groups = list(g1 = "ParamAny"))$param_classes, c("ParamLgl", "ParamDbl", "ParamFct", "ParamInt"))



mut_null = MutatorNull$new()
mut_gauss_small = MutatorGauss$new()
mut_gauss_small$param_set$values$sdev = .01
mut_gauss_zero_one = MutatorGauss$new()
mut_gauss_zero_one$param_set$values$sdev = 1000
mut_gauss_zero_one$param_set$values$truncated_normal = FALSE
mut_fct_flip = MutatorDiscreteUniform$new()
mut_fct_flip$param_set$values$p = 1
mut_fct_flip$param_set$values$can_mutate_to_same = FALSE

p_r = p_dbl(0, 1)
p_ab = p_fct(c("a", "b"))
p_abc = p_fct(c("a", "b", "c"))
set.seed(1)

transformed = MutatorCombination$new(list(a = mut_null, b = mut_gauss_small, c = mut_fct_flip))$
  prime(ps(a = p_r, b = p_r, c = p_ab))$
  operate(data.table(a = 0, b = 0, c = "a"))
expect_equal(transformed$a, 0)
expect_number(transformed$b, lower = 1e-10, upper = .1)
expect_equal(transformed$c, "b")

transformed = MutatorCombination$new(list(a = mut_null, g1 = mut_fct_flip), groups = list(g1 = c("b", "c")))$
  prime(ps(a = p_r, b = p_ab, c = p_abc))$
  operate(data.table(a = rep(0, 10), b = "a", c = "a"))
expect_equal(transformed$a, rep(0, 10))
expect_equal(transformed$b, rep("b", 10))
expect_set_equal(transformed$c, c("b", "c"))

transformed = MutatorCombination$new(list(ParamDbl = mut_gauss_small, ParamFct = mut_fct_flip))$
  prime(ps(a = p_r, b = p_r, c = p_ab))$
  operate(data.table(a = 0, b = 0, c = "a"))
expect_number(transformed$a, lower = 1e-10, upper = .1)
expect_number(transformed$b, lower = 1e-10, upper = .1)
expect_equal(transformed$c, "b")

transformed = MutatorCombination$new(list(g1 = mut_gauss_small, g2 = mut_fct_flip), groups = list(g1 = "ParamDbl", g2 = "ParamFct"))$
  prime(ps(a = p_r, b = p_r, c = p_ab))$
  operate(data.table(a = 0, b = 0, c = "a"))
expect_number(transformed$a, lower = 1e-10, upper = .1)
expect_number(transformed$b, lower = 1e-10, upper = .1)
expect_equal(transformed$c, "b")

transformed = MutatorCombination$new(list(ParamDbl = mut_gauss_small, ParamFct = mut_fct_flip, ParamLgl = mut_null))$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl()))$
  operate(data.table(a = rep(0, 10), b = 0, c = "a", d = "a", e = TRUE))
expect_numeric(transformed$a, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_numeric(transformed$b, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_equal(transformed$c, rep("b", 10))
expect_set_equal(transformed$d, c("b", "c"))
expect_equal(transformed$e, rep(TRUE, 10))
transformed = MutatorCombination$new(list(ParamDbl = mut_gauss_small, ParamFct = mut_fct_flip, ParamLgl = mut_null), binary_fct_as_logical = TRUE)$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl()))$
  operate(data.table(a = rep(0, 10), b = 0, c = "a", d = "a", e = TRUE))
expect_numeric(transformed$a, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_numeric(transformed$b, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_equal(transformed$c, rep("a", 10))
expect_set_equal(transformed$d, c("b", "c"))
expect_equal(transformed$e, rep(TRUE, 10))

transformed = MutatorCombination$new(list(g1 = mut_gauss_small, g2 = mut_fct_flip, g3 = mut_null),
  groups = list(g1 = "ParamDbl", g2 = "ParamFct", g3 = c("ParamLgl", "f")))$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl(), f = p_r))$
  operate(data.table(a = rep(0, 10), b = 0, c = "a", d = "a", e = TRUE, f = 0))
expect_numeric(transformed$a, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_numeric(transformed$b, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_equal(transformed$c, rep("b", 10))
expect_set_equal(transformed$d, c("b", "c"))
expect_equal(transformed$e, rep(TRUE, 10))
expect_equal(transformed$f, rep(0, 10))

transformed = MutatorCombination$new(list(g1 = mut_gauss_small, g2 = mut_fct_flip, g3 = mut_null),
  groups = list(g1 = "ParamDbl", g2 = "ParamFct", g3 = c("ParamLgl", "f")), binary_fct_as_logical = TRUE)$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl(), f = p_r))$
  operate(data.table(a = rep(0, 10), b = 0, c = "a", d = "a", e = TRUE, f = 0))
expect_numeric(transformed$a, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_numeric(transformed$b, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_equal(transformed$c, rep("a", 10))
expect_set_equal(transformed$d, c("b", "c"))
expect_equal(transformed$e, rep(TRUE, 10))
expect_equal(transformed$f, rep(0, 10))

transformed = MutatorCombination$new(list(ParamFct = mut_fct_flip, b = mut_null))$
  prime(ps(a = p_ab, b = p_ab))$
  operate(data.table(a = "a", b = "a"))
expect_equal(transformed, data.table(a = "b", b = "a"))

transformed = MutatorCombination$new(list(g1 = mut_fct_flip, g2 = mut_null),
  groups = list(g1 = "ParamFct", g2 = "b"))$
  prime(ps(a = p_ab, b = p_ab))$
  operate(data.table(a = "a", b = "a"))
expect_equal(transformed, data.table(a = "b", b = "a"))


transformed = MutatorCombination$new(list(g1 = mut_fct_flip, g2 = mut_null),
  groups = list(g1 = "ParamAny", g2 = "b"))$
  prime(ps(a = p_ab, b = p_ab))$
  operate(data.table(a = "a", b = "a"))
expect_equal(transformed, data.table(a = "b", b = "a"))

transformed = MutatorCombination$new(list(g1 = mut_fct_flip, g2 = mut_null),
  groups = list(g1 = c("ParamAny", "b"), g2 = "ParamDbl"))$
  prime(ps(a = p_ab, b = p_ab, c = p_r))$
  operate(data.table(a = "a", b = "a", c = 0))
expect_equal(transformed, data.table(a = "b", b = "b", c = 0))

mut_nested = MutatorCombination$new(list(g1 = MutatorCombination$new(list(a = mut_gauss_small, b = mut_gauss_zero_one, c = mut_fct_flip)),
  d = mut_gauss_small, e = mut_null),
  groups = list(g1 = c("a", "b", "c")))
expected_pv = list(g1.a.sdev = .01, g1.a.truncated_normal = TRUE, g1.b.sdev = 1000, g1.b.truncated_normal = FALSE,
  g1.c.p = 1, g1.c.can_mutate_to_same = FALSE, d.sdev = .01, d.truncated_normal = TRUE)
expect_equal(mut_nested$param_set$values[names(expected_pv)], expected_pv)
transformed = mut_nested$prime(ps(a = p_r, b = p_r, c = p_ab, d = p_r, e = p_r))$operate(data.table(a = rep(0, 10), b = 0, c = "a", d = 0, e = 0))
expect_numeric(transformed$a, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_set_equal(transformed$b, c(0, 1))
expect_equal(transformed$c, rep("b", 10))
expect_numeric(transformed$d, lower = 1e-10, upper = .1, any.missing = FALSE, len = 10)
expect_equal(transformed$e, rep(0, 10))


mut_maybe_flip = MutatorCombination$new(list(a = MutatorMaybe$new(mut_fct_flip)))
mut_maybe_flip$param_set$values$a.maybe.p = 0.5
expect_set_equal(mut_maybe_flip$param_classes, c("ParamLgl", "ParamFct"))
transformed = mut_maybe_flip$prime(ps(a = p_ab))$operate(data.table(a = rep("a", 10)))
expect_set_equal(transformed$a, c("a", "b"))

# adaptions

mut_adapt = MutatorCombination$new(list(ParamDbl = mut_gauss_small), adaptions = list(ParamDbl.sdev = function(x) if (x$a < 0.5) .01 else 0))

expected_params = paste0("ParamDbl.", setdiff(mut_gauss_small$param_set$ids(), "sdev"))
expect_equal(mut_adapt$param_set$values[expected_params], sapply(expected_params, function(x) mut_gauss_small$param_set$values[[substr(x, 10, 1000)]], simplify = FALSE))
mut_adapt$prime(ps(a = p_r, b = p_r))
transformed = mut_adapt$operate(data.table(a = c(rep(0, 10), rep(0.25, 10), rep(0.75, 10), rep(1, 10)), b = c(rep(1, 20), rep(0, 20))))
expect_numeric(transformed[1:10, a], len = 10, lower = 1e-10, upper = .1)
expect_numeric(transformed[1:10, b], len = 10, lower = .9, upper = 1 - 1e-10)
expect_numeric(transformed[11:20, a], len = 10, lower = .15, upper = .35)
expect_numeric(transformed[11:20, b], len = 10, lower = .9, upper = 1 - 1e-10)
expect_equal(transformed[21:30, a], rep(.75, 10))
expect_equal(transformed[21:30, b], rep(0, 10))
expect_equal(transformed[31:40, a], rep(1, 10))
expect_equal(transformed[31:40, b], rep(0, 10))
