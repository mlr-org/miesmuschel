
source("setup.R", local = TRUE)

tb = TerminatorBudget$new()

expect_error(tb$param_set$get_values(), "Missing required parameter.*budget")

oibig = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))

expect_error(tb$is_terminated(oibig$archive), "Missing required parameter.*budget")

tb$param_set$values$budget = Inf

expect_list(tb$param_set$values, names = "unique")
expect_names(names(tb$param_set$values), permutation.of = c("aggregate", "budget"))
expect_equal(tb$param_set$values[c("aggregate", "budget")], list(aggregate = sum, budget = Inf))



expect_error(tb$is_terminated(oibig), "Must inherit from class 'Archive'")

expect_error(tb$is_terminated(oibig$archive), "Need exactly one budget parameter")
expect_error(tb$status(oibig$archive), "Need exactly one budget parameter")

if (miesmuschel:::paradox_s3) {
  oibig$search_space$tags[["bud"]] = "budget"
} else {
  oibig$search_space$params$bud$tags = "budget"
}

expect_false(tb$is_terminated(oibig$archive))

expect_equal(tb$status(oibig$archive), c(max_steps = 100, current_steps = 0))

tb$param_set$values$budget = 0

expect_true(tb$is_terminated(oibig$archive))
expect_equal(tb$status(oibig$archive), c(max_steps = 0, current_steps = 0))

design = cbind(generate_design_random(oibig$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)
oibig$eval_batch(design)

tb$param_set$values$budget = 36
expect_false(tb$is_terminated(oibig$archive))
expect_equal(tb$status(oibig$archive), c(max_steps = 100, current_steps = floor(35 / 36 * 100)))

oibig$eval_batch(design[1])

expect_true(tb$is_terminated(oibig$archive))
expect_equal(tb$status(oibig$archive), c(max_steps = 100, current_steps = 100))

oibig$eval_batch(design[1])

expect_true(tb$is_terminated(oibig$archive))
expect_equal(tb$status(oibig$archive), c(max_steps = 100, current_steps = floor(37 / 36 * 100)))

expect_error(suppressWarnings({tb$param_set$values$aggregate = max}), "must be a function.* finite numeric value")

tb$param_set$values$aggregate = function(x) max(x, 0)
tb$param_set$values$budget = 10

expect_false(tb$is_terminated(oibig$archive))
expect_equal(tb$status(oibig$archive), c(max_steps = 100, current_steps = floor(9 / 10 * 100)))

oibig$eval_batch(design[1][, bud := 10])

expect_true(tb$is_terminated(oibig$archive))
expect_equal(tb$status(oibig$archive), c(max_steps = 100, current_steps = 100))


# multi objective
oim = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))
if (miesmuschel:::paradox_s3) {
  oim$search_space$tags[["bud"]] = "budget"
} else {
  oim$search_space$params$bud$tags = "budget"
}
expect_false(tb$is_terminated(oim$archive))
oibig$eval_batch(design[, p2 := 1:9][9, bud := 10])
expect_true(tb$is_terminated(oibig$archive))


# Actual Optimizer Test
oib = as_oi(objective_budget)
tb$param_set$values = list(budget = 30, aggregate = sum)
oib$terminator = tb

opt = OptimizerMies$new(mutator = mut("gauss", sdev = 0.1), recombinator = RecombinatorCrossoverUniform(),
  parent_selector = SelectorBest$new(), survival_selector = SelectorBest$new(), multi_fidelity = TRUE)
opt$param_set$values$fidelity = function(inst, budget_id, last_fidelity, last_fidelity_offspring) c(1, 4, 6)[[max(1, min(mies_generation(inst), 3))]]
opt$param_set$values$fidelity_offspring = function(inst, budget_id, last_fidelity, last_fidelity_offspring) c(1, 2, 3)[[max(1, min(mies_generation(inst), 3))]]

opt$param_set$values$mu = 2
opt$param_set$values$lambda = 2

set.seed(1)
for (i in 1:3) {
  oib$clear()
  opt$optimize(oib)
  expect_true(sum(oib$archive$data$b) >= 30)
  expect_true(sum(oib$archive$data[batch_nr < max(batch_nr), b]) < 30)
}
