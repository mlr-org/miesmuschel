
source("setup.R", local = TRUE)


###################
# Basics -- Mutator
ops = list(ParamDbl = MutatorGauss$new(), ParamFct = MutatorDiscreteUniform$new())
mcom = MutatorCombination$new(ops, on_name_not_present = "quiet", on_type_not_present = "quiet")
expect_read_only(mcom, c("operators", "groups", "adaptions", "binary_fct_as_logical"))
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

###################
# Basics -- Recombinator
recops = list(ParamDbl = RecombinatorCrossoverUniform$new(), ParamFct = RecombinatorNull$new())
rcom = RecombinatorCombination$new(recops, on_name_not_present = "quiet", on_type_not_present = "quiet")
expect_read_only(rcom, c("operators", "groups", "adaptions", "binary_fct_as_logical", "n_indivs_in", "n_indivs_out"))
expect_equal(rcom$on_name_not_present, "quiet")
expect_equal(rcom$on_type_not_present, "quiet")
expect_recombinator(rcom, "RecombinatorCombination(RecombinatorCrossoverUniform, RecombinatorNull)")

rcom = RecombinatorCombination$new(recops)
expect_set_equal(rcom$param_classes, c("ParamDbl", "ParamFct"))
expect_equal(rcom$groups, list())
expect_equal(rcom$adaptions, list())
expect_equal(rcom$endomorphism, TRUE)
expect_equal(rcom$on_name_not_present, "stop")
expect_equal(rcom$on_type_not_present, "warn")
expect_equal(rcom$binary_fct_as_logical, FALSE)
expect_equal(rcom$n_indivs_in, 2)
expect_equal(rcom$n_indivs_out, 2)

###################
# Mutator: real-world test with actual operators (gauss and discrete)
expect_set_equal(unlist(lapply(names(ops), function(x) sprintf("%s.%s", x, ops[[x]]$param_set$ids()))), mcom$param_set$ids())
mcom$param_set$values$ParamDbl.sdev = .01
mcom$param_set$values$ParamDbl.sdev_is_relative = TRUE
mcom$param_set$values$ParamDbl.truncated_normal = TRUE
mcom$param_set$values$ParamFct.can_mutate_to_same = FALSE

set.seed(1)

p = ps(x1 = p_dbl(0, 1), x2 = p_dbl(0, 10), y1 = p_fct(c("a", "b")))
data = data.table(x1 = c(1, 1), x2 = c(0, 0), y1 = c("a", "b"))
mcom$prime(p)

operated = mcom$operate(data)

expect_equal(operated$y1, c("b", "a"))
expect_true(all(operated$x1 < .999, operated$x2 > .01))
expect_true(all(operated$x1 > .9, operated$x2 < 1))

###################
# Recombinator: real-world test with actual operators (crossover and null)
expect_set_equal(unlist(lapply(names(recops), function(x) sprintf("%s.%s", x, recops[[x]]$param_set$ids()))), rcom$param_set$ids())
rcom$param_set$values$ParamDbl.p = 1
rcom$prime(p)

rdata = data.table(x1 = c(1, 0, 0, 1), x2 = c(0, 0, 1, 0), y1 = c("a", "b", "a", "b"))

expect_equal(rcom$operate(rdata), data.table(x1 = c(0, 1, 1, 0), x2 = c(0, 0, 0, 1), y1 = c("a", "b", "a", "b")))

###################
# Mutator: using debug operators
p = ps(x1 = p_dbl(0, 3), x2 = p_dbl(0, 10), y1 = p_fct(c("a", "b")))

# operate by type pseudogroup
madder = MutatorDebug$new(function(n, v, p) v + p$x, "ParamDbl", ps(x = p_dbl()))
msetter = MutatorDebug$new(function(n, v, p) rep(p$x, length(v)), "ParamFct", ps(x = p_uty()))
ops = list(ParamDbl = madder, ParamFct = msetter)
mcom = MutatorCombination$new(ops)
mcom$param_set$values$ParamDbl.x = 2
mcom$param_set$values$ParamFct.x = "b"
mcom$prime(p)
operated = mcom$operate(data)
expect_true(all(operated$x1 == 3, operated$x2 == 2, operated$y1 == "b"))

# type pseudogroup, overriden with name
mcom = MutatorCombination$new(c(ops, list(x1 = madder)))
mcom$param_set$values$ParamDbl.x = 2
mcom$param_set$values$ParamFct.x = "a"
mcom$param_set$values$x1.x = -1
mcom$prime(p)
operated = mcom$operate(data)
expect_true(all(operated$x1 == 0, operated$x2 == 2, operated$y1 == "a"))

###################
# Recombinator: using debug operators
raddergen = function(nin, nout) {
  RecombinatorDebug$new(function(n, v, p) head(v, nout) + p$x + seq_len(nout),
    c("ParamDbl", "ParamInt"), ps(x = p_int()),
    n_indivs_in = nin, n_indivs_out = nout)
}
rsettergen = function(nin, nout) {
  RecombinatorDebug$new(function(n, v, p) rep(p$x, nout),
    c("ParamDbl", "ParamFct"), ps(x = p_uty()),
    n_indivs_in = nin, n_indivs_out = nout)
}
rops = list(ParamDbl = raddergen(2, 2), ParamFct = rsettergen(2, 2))
rcom = RecombinatorCombination$new(rops)
rcom$param_set$values$ParamDbl.x = -1
rcom$param_set$values$ParamFct.x = "b"
rcom$prime(p)
operated = rcom$operate(rdata)
expect_equal(operated, data.table(x1 = c(1, 1, 0, 2), x2 = c(0, 1, 1, 1), y1 = "b"))

# type pseudogroup, overriden with name
rcom = RecombinatorCombination$new(c(rops, list(x1 = raddergen(2, 2))))
rcom$param_set$values$ParamDbl.x = 2
rcom$param_set$values$ParamFct.x = "a"
rcom$param_set$values$x1.x = -1
rcom$prime(p)
operated = rcom$operate(rdata)
expect_equal(operated, data.table(x1 = c(1, 1, 0, 2), x2 = c(3, 4, 4, 4), y1 = "a"))

###################
# Recombinator only: check for different n_in, n_out
expect_error(RecombinatorCombination$new(list(ParamDbl = raddergen(2, 2), ParamFct = rsettergen(2, 1))), "disagreeing n_indivs_in / n_indivs_out")
rcom = RecombinatorCombination$new(list(ParamDbl = raddergen(2, 1), ParamFct = rsettergen(2, 1)))
expect_equal(rcom$n_indivs_in, 2)
expect_equal(rcom$n_indivs_out, 1)
rcom$param_set$values$ParamDbl.x = 1
rcom$param_set$values$ParamFct.x = "a"
rcom$prime(p)
operated = rcom$operate(rdata)
expect_equal(operated, data.table(x1 = c(3, 2), x2 = c(2, 3), y1 = "a"))

rcom = RecombinatorCombination$new(list(ParamDbl = raddergen(2, 2), ParamFct = rsettergen(1, 1)))
expect_equal(rcom$n_indivs_in, 2)
expect_equal(rcom$n_indivs_out, 2)
rcom$param_set$values$ParamDbl.x = -1
rcom$param_set$values$ParamFct.x = "b"
rcom$prime(p)
operated = rcom$operate(rdata)
expect_equal(operated, data.table(x1 = c(1, 1, 0, 2), x2 = c(0, 1, 1, 1), y1 = "b"))

expect_error(RecombinatorCombination$new(list(ParamDbl = raddergen(2, 1), ParamFct = rsettergen(1, 1))), "disagreeing n_indivs_in / n_indivs_out")

###################
# Mutator: various error conditions
psnum = ParamDbl$new("x", 0, 1)$rep(4)
pslgl = ParamLgl$new("b")$rep(4)
psfct2 = ParamFct$new("c", c("a", "b"))$rep(4)
psfct3 = ParamFct$new("d", c("a", "b", "c"))$rep(4)

MutatorClass = R6::R6Class("MutatorNull",
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


expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")),
  adaptions = list(g1 = 1)), "May only contain.*function.*has type.*numeric")

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl),
  groups = list(g1 = c("x_rep_1", "x_rep_2"), g2 = c("x_rep_3", "x_rep_4")),
  adaptions = list(g3.sdev = identity)), "Adaption for g3.sdev which is not an operator parameter")

expect_error(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))$prime(psnum), "Named operators g2 have no corresponding dimension")
expect_warning(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_name_not_present = "warn")$prime(psnum), "Named operators g2 have no corresponding dimension")

mt = MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))
expect_equal(mt$on_name_not_present, "stop")
mt$on_name_not_present = "warn"
expect_warning(mt$prime(psnum), "Named operators g2 have no corresponding dimension")

expect_silent(MutatorCombination$new(list(g1 = mut_dbl, g2 = mut_dbl), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")), on_name_not_present = "quiet")$prime(psnum))


mt = MutatorCombination$new(list(g1 = mut_dbl, ParamLgl = mut_lf), groups = list(g1 = c("x_rep_1", "x_rep_2", "x_rep_3", "x_rep_4")))
expect_equal(mt$on_type_not_present, "warn")
mt$on_type_not_present = "stop"
expect_error(mt$prime(psnum), "Operators for types / special groups ParamLgl have no corresponding dimensions")


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


# cannot prime when Param names clash with special groups

anynull = MutatorCombination$new(list(ParamAny = MutatorNull$new()))
for (badgroup in c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl", "ParamAny")) {
  expect_error(anynull$prime(ParamSet$new(list(ParamInt$new(badgroup)))), sprintf("components must not have names.*%s", badgroup))
}


expect_set_equal(MutatorCombination$new(list(g1 = mut_lf), groups = list(g1 = "ParamFct"))$param_classes, "ParamFct")
expect_set_equal(MutatorCombination$new(list(g1 = mut_lf), groups = list(g1 = c("ParamFct", "x_rep_1")))$param_classes, c("ParamFct", "ParamLgl"))

expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, ParamAny = mut_dbl))$param_classes, c("ParamLgl", "ParamDbl"))
expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, g1 = mut_dbl), groups = list(g1 = "ParamAny"))$param_classes, c("ParamLgl", "ParamDbl"))
expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, ParamFct = mut_lf, ParamDbl = mut_dbl, ParamInt = mut_int, ParamAny = mut_dbl))$param_classes, c("ParamLgl", "ParamDbl", "ParamFct", "ParamInt"))
expect_set_equal(MutatorCombination$new(list(ParamLgl = mut_lf, ParamFct = mut_lf, ParamDbl = mut_dbl, ParamInt = mut_int, g1 = mut_dbl), groups = list(g1 = "ParamAny"))$param_classes, c("ParamLgl", "ParamDbl", "ParamFct", "ParamInt"))


# various operation assignments
mut_null = MutatorNull$new()
mut_add_one = madder$clone(deep = TRUE)
mut_add_one$param_set$values$x = 1
mut_add_two = madder$clone(deep = TRUE)
mut_add_two$param_set$values$x = 2
mut_fct_flip = MutatorDebug$new(function(n, v, p) ifelse(v %in% p$set, sapply(v, setdiff, x = p$set), v), "ParamFct", ps(set = p_uty()))
mut_fct_flip$param_set$values$set = c("a", "b")

p_r = p_dbl(0, 3)
p_ab = p_fct(c("a", "b"))
p_abc = p_fct(c("a", "b", "c"))
set.seed(1)

transformed = MutatorCombination$new(list(a = mut_null, b = mut_add_one, c = mut_fct_flip))$
  prime(ps(a = p_r, b = p_r, c = p_ab))$
  operate(data.table(a = 0, b = 0, c = "a"))
expect_equal(transformed$a, 0)
expect_equal(transformed$b, 1)
expect_equal(transformed$c, "b")

mt = MutatorCombination$new(list(a = mut_add_one, g1 = mut_fct_flip), groups = list(g1 = c("b", "c")))$
  prime(ps(a = p_r, b = p_ab, c = p_abc))
transformed = mt$operate(data.table(a = rep(c(0, 1), 5), b = "a", c = "c"))
expect_equal(transformed$a, rep(c(1, 2), 5))
expect_equal(transformed$b, rep("b", 10))
expect_equal(transformed$c, rep("c", 10))
expect_equal(mt$groups, list(g1 = c("b", "c")))

transformed = MutatorCombination$new(list(ParamDbl = mut_add_one, ParamFct = mut_fct_flip))$
  prime(ps(a = p_r, b = p_r, c = p_ab))$
  operate(data.table(a = 0, b = 1, c = "a"))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "b")

transformed = MutatorCombination$new(list(g1 = mut_add_one, g2 = mut_fct_flip), groups = list(g1 = "ParamDbl", g2 = "ParamFct"))$
  prime(ps(a = p_r, b = p_r, c = p_ab))$
  operate(data.table(a = 0, b = 1, c = "a"))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "b")

transformed = MutatorCombination$new(list(ParamDbl = mut_add_one, ParamFct = mut_fct_flip, ParamLgl = mut_null))$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl()))$
  operate(data.table(a = 0, b = 1, c = "a", d = "b", e = TRUE))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "b")
expect_equal(transformed$d, "a")
expect_equal(transformed$e, TRUE)

mt = MutatorCombination$new(list(ParamDbl = mut_add_one, ParamFct = mut_fct_flip, ParamLgl = mut_null), binary_fct_as_logical = TRUE)$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl()))
transformed = mt$operate(data.table(a = 0, b = 1, c = "a", d = "a", e = TRUE))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "a")
expect_equal(transformed$d, "b")
expect_equal(transformed$e, TRUE)
expect_equal(mt$binary_fct_as_logical, TRUE)


transformed = MutatorCombination$new(list(g1 = mut_add_one, g2 = mut_fct_flip, g3 = mut_null),
  groups = list(g1 = "ParamDbl", g2 = "ParamFct", g3 = c("ParamLgl", "f")))$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl(), f = p_r))$
  operate(data.table(a = 0, b = 1, c = "a", d = "a", e = TRUE, f = 0))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "b")
expect_set_equal(transformed$d, "b")
expect_equal(transformed$e, TRUE)
expect_equal(transformed$f, 0)

transformed = MutatorCombination$new(list(g1 = mut_add_one, g2 = mut_fct_flip, g3 = mut_null),
  groups = list(g1 = "ParamDbl", g2 = "ParamFct", g3 = c("ParamLgl", "f")), binary_fct_as_logical = TRUE)$
  prime(ps(a = p_r, b = p_r, c = p_ab, d = p_abc, e = p_lgl(), f = p_r))$
  operate(data.table(a = 0, b = 1, c = "a", d = "a", e = TRUE, f = 0))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "a")
expect_set_equal(transformed$d, "b")
expect_equal(transformed$e, TRUE)
expect_equal(transformed$f, 0)

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

mut_nested = MutatorCombination$new(list(g1 = MutatorCombination$new(list(a = mut_add_one, b = mut_add_two, c = mut_fct_flip)),
  d = mut_add_one, e = mut_null),
  groups = list(g1 = c("a", "b", "c")))
expected_pv = list(g1.a.x = 1, g1.b.x = 2, g1.c.set = c("a", "b"), d.x = 1)
expect_set_equal(names(mut_nested$param_set$values), names(expected_pv))
expect_equal(mut_nested$param_set$values[names(expected_pv)], expected_pv)

transformed = mut_nested$prime(ps(a = p_r, b = p_r, c = p_ab, d = p_r, e = p_r))$operate(data.table(a = 0, b = 0, c = "a", d = 0, e = 0))
expect_equal(transformed$a, 1)
expect_equal(transformed$b, 2)
expect_equal(transformed$c, "b")
expect_equal(transformed$d, 1)
expect_equal(transformed$e, 0)


mut_maybe_flip = MutatorCombination$new(list(a = MutatorMaybe$new(mut_fct_flip)))
mut_maybe_flip$param_set$values$a.p = 0.5
expect_equal(mut_maybe_flip$param_classes, "ParamFct")
transformed = mut_maybe_flip$prime(ps(a = p_ab))$operate(data.table(a = rep("a", 10)))
expect_set_equal(transformed$a, c("a", "b"))






###################
# Mutator: adaptions
madder2 = MutatorDebug$new(function(n, v, p) v + p$x, "ParamDbl", ps(x = p_dbl(), y = p_lgl()))
madder2$param_set$values = list(x = 1, y = TRUE)

mut_adapt = MutatorCombination$new(list(ParamDbl = madder2), adaptions = list(ParamDbl.x = function(x) if (x$a < 1) 1 else 2))

expect_equal(mut_adapt$param_set$values, list(ParamDbl.y = TRUE))
mut_adapt$prime(ps(a = p_r, b = p_r))
transformed = mut_adapt$operate(data.table(a = c(0, 1, 0, 1), b = c(0, 0, 1, 1)))
expect_equal(transformed, data.table(a = c(1, 3, 1, 3), b = c(1, 2, 2, 3)))

madder2c = madder2$clone(deep = TRUE)
madder2c$param_set$set_id = "ParamDbl"
madder2c$param_set$values$x = 2
madder2c$prime(ps(a = p_r, b = p_r))

expect_equal(mut_adapt$operators, list(ParamDbl = madder2c))
expect_equal(mut_adapt$adaptions, list(ParamDbl.x = function(x) if (x$a < 1) 1 else 2))

###################
# Recombinator: adaptions
rec_adapt = RecombinatorCombination$new(list(ParamDbl = raddergen(2, 2), x2 = rsettergen(1, 1), ParamFct = rsettergen(2, 2)),
  adaptions = list(ParamDbl.x = function(x) x$x2[[1]], x2.x = function(x) x$x1[[1]]))
expect_equal(rec_adapt$param_set$ids(), "ParamFct.x")
rec_adapt$param_set$values$ParamFct.x = "a"
rec_adapt$prime(ps(x1 = p_dbl(0, 10), x2 = p_dbl(0, 10), y1 = p_fct(c("a", "b"))))

expect_equal(rec_adapt$operate(rdata), data.table(x1 = c(2, 2, 2, 4), x2 = c(1, 1, 0, 0), y1 = c("a", "a", "a", "a")))

###################
# Cloning doesn't mess up ParamSetShadow construction
rec_adapt_c = rec_adapt$clone(deep = TRUE)
expect_equal(rec_adapt_c$param_set$ids(), "ParamFct.x")
rec_adapt_c$param_set$values$ParamFct.x = "b"

expect_equal(rec_adapt$param_set$values$ParamFct.x, "a")

expect_equal(rec_adapt_c$operate(rdata), data.table(x1 = c(2, 2, 2, 4), x2 = c(1, 1, 0, 0), y1 = c("b", "b", "b", "b")))





