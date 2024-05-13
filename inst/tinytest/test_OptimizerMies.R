
source("setup.R", local = TRUE)

# normal optimization
mplus = MutatorDebug$new(handler = function(n, v, p) if (n == "p1" ) v + p$plus * seq_along(v) else v, param_set = ps(plus = p_dbl()))
rminus = RecombinatorDebug$new(handler = function(n, v, p) if (n == "p1" ) v - p$minus * seq_along(v) else v, param_set = ps(minus = p_dbl()), n_indivs_in = 3)

seltop = SelectorDebug$new(handler = function(v, f, n, p) head(order(abs(f[, 1] - p$top)), n), param_set = ps(top = p_dbl()))

oi = as_oi(get_objective_passthrough("minimize", FALSE))
oi$terminator = trm("evals", n_evals = 28)

opt = OptimizerMies$new(mutator = mplus, recombinator = rminus, parent_selector = seltop, survival_selector = seltop)


# accessing components
expect_equal_without_id(opt$mutator, mplus)
expect_equal_without_id(opt$recombinator, rminus)
expect_equal_without_id(opt$parent_selector, seltop)
expect_equal_without_id(opt$survival_selector, seltop)
expect_equal(opt$elite_selector, NULL)

expect_read_only(opt, c("mutator", "recombinator", "parent_selector", "survival_selector", "elite_selector", "param_set"))

opt$param_set$values = list(
  lambda = 6, mu = 10, mutator.plus = -0.1, recombinator.minus = -0.001, parent_selector.top = -4, survival_selector.top = 0,
  survival_strategy = "plus", initializer = generate_design_random_increasing
)

set.seed(1)
opt$optimize(oi)

expect_data_table(oi$archive$data, nrows = 28)

rowids = sapply(mlr3misc::transpose_list(oi$archive$data[, setdiff(oi$search_space$ids(), "p1"), with = FALSE]), mlr3misc::str_collapse)

gen1 = oi$archive$data[1:10, p1]
expect_equal(gen1, 1:10)

survivors = gen1
survivor_rowids = rowids[1:10]
genrange = 11:16
GENERATION = 2

expected_selected = survivor_rowids[order(abs(survivors - 4))[1:6]]
expect_set_equal(expected_selected, rowids[genrange])

gen_input = survivors[order(abs(survivors - 4))[1:6]]
gen_output = oi$archive$data[(genrange)[match(expected_selected, rowids[genrange])], p1]
expect_equal(round(sort(gen_input - gen_output), 2), (1:6) / 10)
expect_equal(sort((-gen_input + gen_output) %% 0.01), rep(c(.001, .002, .003), each = 2))

newsurv = oi$archive$data[dob <= GENERATION & (is.na(eol) | eol > GENERATION), p1]
survivor_rowids = rowids[oi$archive$data[, dob <= GENERATION & (is.na(eol) | eol > GENERATION)]]
expect_equal(sort(newsurv), c(survivors, gen_output)[order(c(survivors, gen_output))[1:10]])

# optimization with proxy operators
oi$clear()

opt = OptimizerMies$new(recombinator = rminus)

mplus$param_set$values$plus = 0.1
seltop$param_set$values$top = -4
seltop0 = seltop$clone(deep = TRUE)
seltop0$param_set$values$top = 0

opt$param_set$values = list(
  lambda = 6, mu = 10, mutator.operation = mplus, recombinator.minus = 0.001, parent_selector.operation = seltop, survival_selector.operation = seltop0,
  survival_strategy = "plus", initializer = generate_design_random_increasing
)
set.seed(1)
opt$optimize(oi)

expect_data_table(oi$archive$data, nrows = 28)

gen1 = oi$archive$data[1:10, p1]
expect_equal(gen1, 1:10)
gen2 = oi$archive$data[11:16, p1]
gen2surv = oi$archive$data[dob <= 2 & (is.na(eol) | eol > 2), p1]
gen3 = oi$archive$data[17:22, p1]
gen3surv = oi$archive$data[dob <= 3 & (is.na(eol) | eol > 3), p1]
gen4 = oi$archive$data[23:28, p1]

expect_equal(round(sort(sort(gen2) - sort(gen1[order(abs(gen1 - 4))[1:6]])), 2), 1:6 / 10)
expect_equal(sort((-sort(gen2) + sort(gen1[order(abs(gen1 - 4))[1:6]])) %% 0.01), rep(c(.001, .002, .003), each = 2))
expect_equal(sort(gen2surv), c(gen1, gen2)[order(c(gen1, gen2))[1:10]])

# optimization with elite selection

opt = OptimizerMies$new(mutator = mplus, recombinator = rminus, parent_selector = seltop, survival_selector = seltop, elite_selector = seltop)
expect_equal_without_id(opt$elite_selector, seltop)

opt$param_set$values = list(
  lambda = 6, mu = 10, mutator.plus = -0.1, recombinator.minus = -0.001, parent_selector.top = -4, survival_selector.top = 0, elite_selector.top = -9,
  survival_strategy = "comma", initializer = generate_design_random_increasing, n_elite = 3
)


set.seed(1)
oi$clear()
opt$optimize(oi)

expect_data_table(oi$archive$data, nrows = 28)

oi$archive$data

gen1 = oi$archive$data[1:10, p1]
expect_equal(gen1, 1:10)
gen2 = oi$archive$data[11:16, p1]
gen2surv = oi$archive$data[dob <= 2 & (is.na(eol) | eol > 2), p1]
gen3 = oi$archive$data[17:22, p1]
gen3surv = oi$archive$data[dob <= 3 & (is.na(eol) | eol > 3), p1]
gen4 = oi$archive$data[23:28, p1]

expect_equal(round(sort(sort(gen2) - sort(gen1[order(abs(gen1 - 4))[1:6]])), 2), -(6:1) / 10)
expect_equal(sort((sort(gen2) - sort(gen1[order(abs(gen1 - 4))[1:6]])) %% 0.01), rep(c(.001, .002, .003), each = 2))
expect_equal(sort(gen2surv), sort(c(gen2[order(gen2)[1:6]], gen1[order(abs(gen1 - 9))][1:3])))

# optimization with additional components

rminusaddit = RecombinatorDebug$new(handler = function(n, v, p) if (n == "px1") v - p$minus * seq_along(v) else v, param_set = ps(minus = p_dbl()), n_indivs_in = 3)

opt = OptimizerMies$new(mutator = mplus, recombinator = rminusaddit, parent_selector = seltop, survival_selector = seltop)

opt$param_set$values = list(
  lambda = 6, mu = 10, mutator.plus = -0.1, recombinator.minus = -0.001, parent_selector.top = -4, survival_selector.top = 0,
  survival_strategy = "plus", initializer = generate_design_random_increasing, additional_component_sampler = generate_increasing_sampler(ps(px1 = p_dbl(), px2 = p_dbl()))
)

oi$clear()
opt$optimize(oi)

expect_names(colnames(oi$archive$data), permutation.of = c(oi$search_space$ids(), "dob", "eol", oi$objective$codomain$ids(),
  "x_domain", "timestamp", "batch_nr", "px1", "px2", "x_id"))

# supported types are propagated
set.seed(1)
oi$clear()
opt$optimize(oi)

expect_data_table(oi$archive$data, nrows = 28)

gen1 = oi$archive$data[1:10, p1]
gen1px = oi$archive$data[1:10, px1]
expect_equal(oi$archive$data$px1[1:10], 1:10)
expect_equal(oi$archive$data$px2[1:10], 1:10)
expect_equal(gen1, 1:10)
gen2 = oi$archive$data[11:16, p1]
gen2px = oi$archive$data[11:16, px1]
gen2surv = oi$archive$data[dob <= 2 & (is.na(eol) | eol > 2), p1]


gen3 = oi$archive$data[17:22, p1]
gen3surv = oi$archive$data[dob <= 3 & (is.na(eol) | eol > 3), p1]
gen4 = oi$archive$data[23:28, p1]

expect_equal(sort(sort(gen2) - sort(gen1[order(abs(gen1 - 4))[1:6]])), -(6:1) / 10)
expect_equal(sort(gen2surv), c(gen1, gen2)[order(c(gen1, gen2))[1:10]])
expect_equal(sort(sort(gen2px) - sort(gen1px[order(abs(gen1 - 4))[1:6]])), rep(c(.001, .002, .003), each = 2))


# Multifidelity

oib = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))
if (miesmuschel:::paradox_s3) {
  oib$search_space$tags$bud = "budget"
} else {
  oib$search_space$params$bud$tags = "budget"
}
oib$terminator = trm("evals", n_evals = 10)

opt = OptimizerMies$new(mutator = MutatorNull$new(), recombinator = RecombinatorCrossoverUniform(),
  parent_selector = SelectorBest$new(), survival_selector = SelectorBest$new(), multi_fidelity = TRUE)

records = new.env()

opt$param_set$values$fidelity = function(inst, budget_id, last_fidelity, last_fidelity_offspring) {
  records$fidelity_gens = c(records$fidelity_gens, mies_generation(inst))
  records$fidelity_args = rbind(records$fidelity_args,
    data.table(budget_id = list(budget_id), last_fidelity = list(last_fidelity), last_fidelity_offspring = list(last_fidelity_offspring)))
  c(1, 4, 6)[[max(1, min(mies_generation(inst), 3))]]
}
opt$param_set$values$fidelity_offspring = function(inst, budget_id, last_fidelity, last_fidelity_offspring) {
  records$fidelity_offspring_gens = c(records$fidelity_offspring_gens, mies_generation(inst))
  records$fidelity_offspring_args = rbind(records$fidelity_offspring_args,
    data.table(budget_id = list(budget_id), last_fidelity = list(last_fidelity), last_fidelity_offspring = list(last_fidelity_offspring)))
  c(1, 2, 3)[[max(1, min(mies_generation(inst), 3))]]
}
opt$param_set$values$mu = 2
opt$param_set$values$lambda = 2

set.seed(1)
expect_error(opt$optimize(oi), "Need exactly one budget parameter.* found 0")

opt$optimize(oib)

# budgets: initial sample: 1, first offspring: 1, re-eval of survivors: 4, second offspring: 2, re-eval: 6
expect_equal(oib$archive$data$bud, rep(c(1, 1, 4, 2, 6), each = 2))
# generations: initial, first offspring, re-eval, second offspring, re-eval
expect_equal(oib$archive$data$dob, rep(c(1, 2, 2, 3, 3), each = 2))

# at what generations and with what arguments the fidelity-functions are called
expect_equal(records$fidelity_gens, c(0, 2, 3))

expect_equal(records$fidelity_args, data.table(budget_id = list("bud"), last_fidelity = list(NULL, 1, 4), last_fidelity_offspring = list(NULL, 1, 2)))

expect_equal(records$fidelity_offspring_gens, c(1, 2, 3))

expect_equal(records$fidelity_offspring_args, data.table(budget_id = list("bud"), last_fidelity = list(1, 4, 6), last_fidelity_offspring = list(NULL, 1, 2)))


expect_names(colnames(oib$archive$data), permutation.of = c(oib$search_space$ids(), "dob", "eol", oib$objective$codomain$ids(), "x_domain", "timestamp", "batch_nr", "x_id"))

# cloning, paramsets handled properly

opt = OptimizerMies$new(mutator = mplus, recombinator = rminus, parent_selector = seltop, survival_selector = seltop)
if (!miesmuschel:::paradox_s3) opt$param_set$set_id = "test"

opt$param_set$values$mutator.plus = 0
expect_equal(opt$mutator$param_set$values$plus, 0)
opt$param_set$values$mutator.plus = 1
expect_equal(opt$mutator$param_set$values$plus, 1)

opt2 = opt$clone(deep = TRUE)

expect_equal(opt2$param_set$values$mutator.plus, 1)
opt$param_set$values$mutator.plus = 0
expect_equal(opt$mutator$param_set$values$plus, 0)
expect_equal(opt2$param_set$values$mutator.plus, 1)

opt2$param_set$values$mutator.plus = -1
expect_equal(opt$mutator$param_set$values$plus, 0)
expect_equal(opt2$param_set$values$mutator.plus, -1)

if (!miesmuschel:::paradox_s3) expect_equal(opt2$param_set$set_id, "test")
if (!miesmuschel:::paradox_s3) expect_equal(opt$param_set$set_id, "test")

# ParamClasses

mdblint = MutatorDebug$new(handler = function(n, v, p) if (n == "p1" ) v + p$plus * seq_along(v) else v, c("ParamDbl", "ParamInt"), param_set = ps(plus = p_dbl()))
rdblfct = RecombinatorDebug$new(handler = function(n, v, p) if (n == "p1" ) v - p$minus * seq_along(v) else v, c("ParamDbl", "ParamFct"),
  param_set = ps(minus = p_dbl()), n_indivs_in = 3)

seltopsc = SelectorDebug$new(handler = function(v, f, n, p) head(order(abs(f[, 1] - p$top)), n), param_set = ps(top = p_dbl()), supported = "single-crit")

# oi = as_oi(get_objective_passthrough("minimize", FALSE))
# oi$terminator = trm("evals", n_evals = 28)

opt2 = OptimizerMies$new(mutator = mdblint, recombinator = rdblfct, parent_selector = seltop, survival_selector = seltop)

expect_equal(opt2$param_classes, "ParamDbl")
expect_true(all(c("single-crit", "multi-crit") %in% opt2$properties))

opt3 = OptimizerMies$new(mutator = mdblint, recombinator = rdblfct, parent_selector = seltop, survival_selector = seltopsc)
expect_equal(intersect(opt3$properties, c("single-crit", "multi-crit")), "single-crit")
