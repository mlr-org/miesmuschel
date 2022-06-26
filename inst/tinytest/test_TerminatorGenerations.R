
source("setup.R", local = TRUE)

tg = TerminatorGenerations$new()
expect_error(tg$param_set$get_values(), "Missing required parameter.*generations")

oibig = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))

expect_error(tg$is_terminated(oibig$archive), "Missing required parameter.*generations")


tg$param_set$values$generations = Inf

expect_equal(tg$param_set$values, list(generations = Inf))



expect_error(tg$is_terminated(oibig), "Must inherit from class 'Archive'")

expect_false(tg$is_terminated(oibig$archive))
expect_equal(tg$status(oibig$archive), c(max_steps = Inf, current_steps = 0))

tg$param_set$values$generations = 0

expect_true(tg$is_terminated(oibig$archive))

expect_equal(tg$status(oibig$archive), c(max_steps = 0, current_steps = 0))

mies_init_population(oibig, 3)

expect_equal(tg$status(oibig$archive), c(max_steps = 0, current_steps = 1))

design = cbind(generate_design_random(oibig$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)
oibig$eval_batch(design)

expect_equal(tg$status(oibig$archive), c(max_steps = 0, current_steps = 3))

tg$param_set$values$generations = 4

expect_false(tg$is_terminated(oibig$archive))
expect_equal(tg$status(oibig$archive), c(max_steps = 4, current_steps = 3))

tg$param_set$values$generations = 3

expect_true(tg$is_terminated(oibig$archive))
expect_equal(tg$status(oibig$archive), c(max_steps = 3, current_steps = 3))

# multi objective
oim = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))
expect_false(tg$is_terminated(oim$archive))
oibig$eval_batch(design[, p2 := 1:9][9, bud := 10])
expect_true(tg$is_terminated(oibig$archive))

# Actual Optimizer Test
oib = as_oi(objective_budget)
tg$param_set$values$generations = 3
oib$terminator = tg



opt = OptimizerMies$new(mutator = MutatorGauss$new(), recombinator = RecombinatorCrossoverUniform$new(),
  parent_selector = SelectorBest$new(), survival_selector = SelectorBest$new(), multi_fidelity = TRUE)
opt$param_set$values$fidelity = function(inst, budget_id, last_fidelity, last_fidelity_offspring) c(1, 4, 6)[[max(1, min(mies_generation(inst), 3))]]
opt$param_set$values$fidelity_offspring = function(inst, budget_id, last_fidelity, last_fidelity_offspring) c(1, 2, 3)[[max(1, min(mies_generation(inst), 3))]]
opt$param_set$values$mu = 2
opt$param_set$values$lambda = 2

set.seed(1)
opt$optimize(oib)

# budgets: initial sample: 1, first offspring: 1, re-eval of survivors: 4, second offspring: 2, re-eval: 6
expect_equal(oib$archive$data$b, rep(c(1, 1, 4, 2, 6), each = 2))
# generations: initial, first offspring, re-eval, second offspring, re-eval
expect_equal(oib$archive$data$dob, rep(c(1, 2, 2, 3, 3), each = 2))

# terminator_get_generations

tg = TerminatorGenerations$new()
tg$param_set$values$generations = 10
expect_equal(terminator_get_generations(tg), 10)
tc = TerminatorCombo$new(list(tg, TerminatorEvals$new()))
expect_equal(terminator_get_generations(tc), 10)
tc$param_set$values$any = FALSE
expect_equal(terminator_get_generations(tc), Inf)

expect_error(terminator_get_generations(10), "Invalid terminator given")
