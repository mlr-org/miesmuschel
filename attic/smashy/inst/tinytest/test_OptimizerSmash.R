
source("setup.R", local = TRUE)


# normal optimization
oi = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))



opt = OptimizerSmash$new()

opt$param_set$values$mu = 4
opt$param_set$values$survival_fraction = 0.5
opt$param_set$values$fidelity_steps = 0
opt$param_set$values$sampling = generate_design_random_increasing

# accessing components
expect_equal_without_id(opt$filtor, FiltorProxy$new())
expect_read_only(opt, c("filtor", "param_set"))

expect_error(opt$optimize(oi), "Need exactly one budget parameter for multifidelity method, but found 0")

oi$search_space$params$bud$tags = "budget"


# auto-recognition of generations
oi$terminator = trm("evals", n_evals = 12)
expect_error(opt$optimize(oi), "When fidelity_steps is 0")
oi$terminator = trm("combo", list(trm("evals", n_evals = 100), trm("gens", generations = 3)), any = FALSE)
expect_error(opt$optimize(oi), "When fidelity_steps is 0")

oi$terminator = trm("combo", list(trm("evals", n_evals = 100), trm("gens", generations = 0)), any = TRUE)

expect_error(opt$optimize(oi), "At least one generation")

oi$terminator = trm("combo", list(trm("evals", n_evals = 100), trm("gens", generations = 3)), any = TRUE)
opt$param_set$values$survival_fraction = 0.9
expect_error(opt$optimize(oi), "Number of survivors equals the total")

opt$param_set$values$survival_fraction = 0.5

set.seed(1)
opt$optimize(oi)


expect_data_table(oi$archive$data, nrows = 12)

rowids = sapply(mlr3misc::transpose_list(oi$archive$data[, setdiff(oi$search_space$ids(), c("p1", "bud")), with = FALSE]), mlr3misc::str_collapse)

# 1st gen: 1 and 2 survive. 2nd gen: two 1s survive (and two entries are sampled anew, get 1:2)
expect_equal(oi$archive$data$p1, c(1:4, 1:2, 1:2, 1:2, 1, 1))

expect_equal(rowids[c(1:6, 1:2, 9:10, 5, 1)], rowids)

expect_equal(oi$archive$data$dob, rep(1:3, each = 4))
expect_equal(oi$archive$data$eol, c(2, 2, 1, 1, 3, 2, 3, 2, NA, NA, NA, NA))

expect_equal(oi$archive$data$bud, rep(c(-10, 0, 10), each = 4))


# optimization continuation

opt$param_set$values$fidelity_steps = 3
oi$terminator = trm("gens", generations = 6)

opt$optimize(oi)

expect_data_table(oi$archive$data, nrows = 24)

rowids = sapply(mlr3misc::transpose_list(oi$archive$data[, setdiff(oi$search_space$ids(), c("p1", "bud")), with = FALSE]), mlr3misc::str_collapse)

# 1st gen: 1 and 2 survive. 2nd gen: two 1s survive (and two entries are sampled anew, get 1:2)
expect_equal(oi$archive$data$p1, rep(c(1:4, 1:2, 1:2, 1:2, 1, 1), 2))

expect_equal(rowids[c(c(1:6, 1:2, 9:10, 5, 1), 12 + c(1:6, 1:2, 9:10, 5, 1))], rowids)

expect_equal(oi$archive$data$dob, rep(1:6, each = 4))
expect_equal(oi$archive$data$eol, c(2, 2, 1, 1, 3, 2, 3, 2, 3, 3, 3, 3, 5, 5, 4, 4, 6, 5, 6, 5, NA, NA, NA, NA))
expect_equal(oi$archive$data$bud, rep(rep(c(-10, 0, 10), 2), each = 4))

# interruption in between

oi2 = oi = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))
oi2$search_space$params$bud$tags = "budget"
oi2$terminator = trm("evals", n_evals = 6)

opt$optimize(oi2)

expect_equal(oi2$archive$data$p1, c(1:4, 1:2, 1:2))
rowids = sapply(mlr3misc::transpose_list(oi2$archive$data[, setdiff(oi2$search_space$ids(), c("p1", "bud")), with = FALSE]), mlr3misc::str_collapse)
expect_equal(rowids[c(1:6, 1:2)], rowids)
expect_equal(oi2$archive$data$dob, rep(1:2, each = 4))
expect_equal(oi2$archive$data$eol, c(2, 2, 1, 1, NA, NA, NA, NA))
expect_equal(oi$archive$data$bud, rep(c(-10, 0), each = 4))


# .. and continue ...
oi2$terminator = trm("gens", generations = 6)
opt$optimize(oi2)



rowids = sapply(mlr3misc::transpose_list(oi2$archive$data[, setdiff(oi2$search_space$ids(), c("p1", "bud")), with = FALSE]), mlr3misc::str_collapse)
expect_equal(oi2$archive$data$p1, rep(c(1:4, 1:2, 1:2, 1:2, 1, 1), 2))
expect_equal(rowids[c(c(1:6, 1:2, 9:10, 5, 1), 12 + c(1:6, 1:2, 9:10, 5, 1))], rowids)
expect_equal(oi2$archive$data$dob, rep(1:6, each = 4))
expect_equal(oi2$archive$data$eol, c(2, 2, 1, 1, 3, 2, 3, 2, 3, 3, 3, 3, 5, 5, 4, 4, 6, 5, 6, 5, NA, NA, NA, NA))


# filtering

account = new.env()
account$archive = list()

ftraccount = FiltorDebug$new(handler = function(v, k, f, n, p) {
  account$archive[[length(account$archive) + 1]] = v$bud
  seq_len(n) * 2
}, ni = function(o, p) o * 2)

opt = OptimizerSmash$new(ftraccount)

opt$param_set$values$mu = 6
opt$param_set$values$survival_fraction = 0.5
opt$param_set$values$fidelity_steps = 4
opt$param_set$values$sampling = generate_design_random_increasing

oi2$clear()
oi2$terminator = trm("gens", generations = 8)
opt$optimize(oi2)

rowids = sapply(mlr3misc::transpose_list(oi2$archive$data[, setdiff(oi2$search_space$ids(), c("p1", "bud")), with = FALSE]), mlr3misc::str_collapse)
expect_equal(oi2$archive$data$p1, c(1:6, (1:3) * 2, 1:3, (1:3) * 2, 2, 1, 2, (1:3) * 2, 2, 2, 1, c(1:5, 1) * 2, (1:3) * 2, 2, 4, 2, (1:3) * 2, 2, 2, 2, (1:3) * 2, 2, 2, 2))

expect_equal(rowids[c(1:9, 1:3, 13:15, 7, 1:2, 19:21, 13, 7, 1, 25:33, 25, 26, 30, 37:39, 31, 25, 30, 43:45, 37, 31, 25)], rowids)

expect_equal(oi2$archive$data$dob, rep(1:8, each = 6))

expect_equal(oi2$archive$data$eol,
  c(2, 2, 2, 1, 1, 1,
    3, 2, 2, 3, 3, 2,
    4, 3, 3, 4, 4, 3,
    4, 4, 4, 4, 4, 4,

    6, 6, 5, 5, 5, 6,
    7, 6, 6, 7, 6, 7,
    8, 7, 7, 8, 8, 7,
    NA, NA, NA, NA, NA, NA
))

# correct budget given to filtor
expected_archive = lapply(rep(seq(-10, 10, length.out = 4), 2), rep, each = 6)[1:7]
expected_archive[[4]] = rep(-10, 12)
expect_equal(account$archive, expected_archive)

# again, with  filter_with_max_budget

opt$param_set$values$filter_with_max_budget = TRUE

oi2$clear()
account$archive = list()
opt$optimize(oi2)

rowids = sapply(mlr3misc::transpose_list(oi2$archive$data[, setdiff(oi2$search_space$ids(), c("p1", "bud")), with = FALSE]), mlr3misc::str_collapse)
expect_equal(oi2$archive$data$p1, c(1:6, (1:3) * 2, 1:3, (1:3) * 2, 2, 1, 2, (1:3) * 2, 2, 2, 1, c(1:5, 1) * 2, (1:3) * 2, 2, 4, 2, (1:3) * 2, 2, 2, 2, (1:3) * 2, 2, 2, 2))

expect_equal(rowids[c(1:9, 1:3, 13:15, 7, 1:2, 19:21, 13, 7, 1, 25:33, 25, 26, 30, 37:39, 31, 25, 30, 43:45, 37, 31, 25)], rowids)

expect_equal(oi2$archive$data$dob, rep(1:8, each = 6))

expect_equal(oi2$archive$data$eol,
  c(2, 2, 2, 1, 1, 1,
    3, 2, 2, 3, 3, 2,
    4, 3, 3, 4, 4, 3,
    4, 4, 4, 4, 4, 4,

    6, 6, 5, 5, 5, 6,
    7, 6, 6, 7, 6, 7,
    8, 7, 7, 8, 8, 7,
    NA, NA, NA, NA, NA, NA
))

expected_archive = lapply(c(seq(-10, 10, length.out = 4), rep(10, 3)), rep, each = 6)
expected_archive[[4]] = rep(10, 12)

expect_equal(account$archive, expected_archive)



# cloning, paramsets handled properly

opt = OptimizerSmash$new(filtor = ftr("surprog", mlr3::lrn("regr.rpart")))

opt$param_set$set_id = "test"

opt$param_set$values$filtor.cp = 0.7

expect_equal(opt$filtor$param_set$values$cp, 0.7)
opt$param_set$values$filtor.cp = 1
expect_equal(opt$filtor$param_set$values$cp, 1)

opt2 = opt$clone(deep = TRUE)

expect_equal(opt2$param_set$values$filtor.cp, 1)
opt$param_set$values$filtor.cp = 0.3
expect_equal(opt$filtor$param_set$values$cp, 0.3)
expect_equal(opt2$param_set$values$filtor.cp, 1)

opt2$param_set$values$filtor.cp = 0.01
expect_equal(opt$filtor$param_set$values$cp, 0.3)
expect_equal(opt2$param_set$values$filtor.cp, 0.01)

expect_equal(opt2$param_set$set_id, "test")
expect_equal(opt$param_set$set_id, "test")

# ParamClasses and properties

fdblint = FiltorDebug$new(handler = function(v, k, f, n, p) seq_len(n), ni = function(o, p) o, param_classes = c("ParamDbl", "ParamInt"), param_set = ps(plus = p_dbl()))

opt2 = OptimizerSmash$new(filtor = fdblint)

expect_equal(opt2$param_classes, c("ParamDbl", "ParamInt"))
expect_equal(opt2$properties, c("dependencies", "single-crit"))

