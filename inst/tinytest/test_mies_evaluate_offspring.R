

source("setup.R", local = TRUE)

oismall = as_oi(get_objective_passthrough("minimize", TRUE))

# eval from empty archive
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1)), data.table(pout1 = 1))
expected_archive = data.table(p1 = 1, dob = 1, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 1)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# eval from nonempty archive
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1:3)), data.table(pout1 = 1:3))
expected_archive = rbind(expected_archive,
  data.table(p1 = 1:3, dob = 2, eol = NA_real_, pout1 = 1:3, x_domain = list(list(p1 = 1), list(p1 = 2), list(p1 = 3)), batch_nr = 2))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# eval with additional components
oismall = as_oi(get_objective_passthrough("minimize", TRUE))
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1, x1 = 10)), data.table(pout1 = 1))
expected_archive = data.table(p1 = 1, x1 = 10, dob = 1, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 1)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# fidelity schedule
fidelity_schedule = data.frame(
  generation = c(1, 3, 5),
  budget_new = c(1, 3, 5),
  budget_survivors = c(2, 5, 4)
)

# fid sched missing
oismall = as_oi(get_objective_passthrough("minimize", TRUE, "bud"))
expect_error(mies_evaluate_offspring(oismall, data.frame(p1 = 1), budget_id = "bud"),
  "fidelity_schedule.*must be a data.frame")

# budget_id missing
expect_error(mies_evaluate_offspring(oismall, data.frame(p1 = 1), fidelity_schedule = fidelity_schedule),
  "budget_id.*not 'NULL'")

# budget_new, generation 1
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1), budget_id = "bud", fidelity_schedule = fidelity_schedule), data.table(pout1 = 1))
expected_archive = data.table(p1 = 1, bud = 1, dob = 1, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1, bud = 1)), batch_nr = 1)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# budget_survivors, generation 2
mies_evaluate_offspring(oismall, data.frame(p1 = 1:3), budget_id = "bud", fidelity_schedule = fidelity_schedule, survivor_budget = TRUE)
expected_archive = rbind(expected_archive,
  data.table(p1 = 1:3, bud = 2, dob = 2, eol = NA_real_, pout1 = 1:3, x_domain = lapply(1:3, function(x) (list(p1 = x, bud = 2))), batch_nr = 2))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# budget_new, generation 3
mies_evaluate_offspring(oismall, data.frame(p1 = 3:1), budget_id = "bud", fidelity_schedule = fidelity_schedule, survivor_budget = FALSE)
expected_archive = rbind(expected_archive,
  data.table(p1 = 3:1, bud = 3, dob = 3, eol = NA_real_, pout1 = 3:1, x_domain = lapply(3:1, function(x) (list(p1 = x, bud = 3))), batch_nr = 3))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# budget_survivors, generation 4
mies_evaluate_offspring(oismall, data.frame(p1 = c(5, 7)), budget_id = "bud", fidelity_schedule = fidelity_schedule, survivor_budget = TRUE)
expected_archive = rbind(expected_archive,
  data.table(p1 = c(5, 7), bud = 5, dob = 4, eol = NA_real_, pout1 = c(5, 7), x_domain = lapply(c(5, 7), function(x) (list(p1 = x, bud = 5))), batch_nr = 4))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# zero-row evaluations
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = numeric(0)), budget_id = "bud", fidelity_schedule = fidelity_schedule, survivor_budget = TRUE),
  data.table(pout1 = numeric(0)))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = numeric(0)), budget_id = "bud", fidelity_schedule = fidelity_schedule, survivor_budget = FALSE),
  data.table(pout1 = numeric(0)))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = numeric(0), bud = numeric(0))),
  data.table(pout1 = numeric(0)))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# larger searchspace
oilarge = as_oi(get_objective_passthrough("minimize", FALSE))
expect_equal(mies_evaluate_offspring(oilarge, generate_design_random(oilarge$search_space, 2)$data[, p1 := 1:2]), data.table(pout1 = 1:2))

# multi-objective
oilarge_mo = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE))
expect_equal(mies_evaluate_offspring(oilarge_mo, generate_design_random(oilarge$search_space, 2)$data[, p1 := 1:2][, p2 := 2:1]), data.table(pout1 = 1:2, pout2 = 2:1))
oismall_mo = as_oi(get_objective_passthrough(c("minimize", "maximize"), TRUE))
expect_equal(mies_evaluate_offspring(oismall_mo, generate_design_random(oismall_mo$search_space, 2)$data[, p1 := 1:2][, p2 := 2:1]), data.table(pout1 = 1:2, pout2 = 2:1))
expected_archive_mo = data.table(p1 = 1:2, p2 = 2:1, dob = 1, eol = NA_real_, pout1 = 1:2, pout2 = 2:1, x_domain = list(list(p1 = 1, p2 = 2), list(p1 = 2, p2 = 1)), batch_nr = 1)
expect_equal(copy(oismall_mo$archive$data)[, timestamp := NULL], expected_archive_mo, ignore.col.order = TRUE)


# evaluating and fidelity stepping
# step_fidelity FALSE

expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1), budget_id = "bud", fidelity_schedule = fidelity_schedule), data.table(pout1 = 1))

expected_archive = rbind(expected_archive,
  data.table(p1 = 1, bud = 5, dob = 5, eol = NA_real_, pout1 = 1, x_domain = lapply(1, function(x) (list(p1 = x, bud = 5))), batch_nr = 5))

expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


# step_fidelity TRUE, monotonic = TRUE (re-evaluates only budget < 4
res = mies_evaluate_offspring(oismall, data.frame(p1 = 1), budget_id = "bud", fidelity_schedule = fidelity_schedule, step_fidelity = TRUE, monotonic = TRUE)

# evaluates offspring (pout1 = 1), and re-evaluates all entries with budget < 4
expect_equal(res, data.table(pout1 = c(1, expected_archive[bud < 4, pout1])))

expected_archive2 = rbind(expected_archive,
  data.table(p1 = 1, bud = 5, dob = 6, eol = NA_real_, pout1 = 1, x_domain = lapply(1, function(x) (list(p1 = x, bud = 5))), batch_nr = 6),
  expected_archive[bud < 4][, `:=`(bud = 4, dob = 6, x_domain = lapply(p1, function(x) (list(p1 = x, bud = 4))), batch_nr = 6)]
)
expected_archive2[bud < 4, eol := 6]

expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive2, ignore.col.order = TRUE)

# step_fidelity TRUE, monotonic = FALSE (re-evaluates budget != 4)
res = mies_evaluate_offspring(oismall, data.frame(p1 = 1), budget_id = "bud", fidelity_schedule = fidelity_schedule, step_fidelity = TRUE, monotonic = FALSE)

# evaluates offspring (pout1 = 1), and re-evaluates all entries with budget != 4
expect_equal(res, data.table(pout1 = c(1, expected_archive2[is.na(eol) & bud != 4, pout1])))

expected_archive3 = rbind(expected_archive2,
  data.table(p1 = 1, bud = 5, dob = 7, eol = NA_real_, pout1 = 1, x_domain = lapply(1, function(x) (list(p1 = x, bud = 5))), batch_nr = 7),
  expected_archive2[is.na(eol) & bud != 4][, `:=`(bud = 4, dob = 7, x_domain = lapply(p1, function(x) (list(p1 = x, bud = 4))), batch_nr = 7)]
)
expected_archive3[bud != 4 & is.na(eol) & dob < 7, eol := 7]

expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive3, ignore.col.order = TRUE)
