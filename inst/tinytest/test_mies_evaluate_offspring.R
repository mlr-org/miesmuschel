

source("setup.R", local = TRUE)

oismall = as_oi(get_objective_passthrough("minimize", TRUE))

# eval from empty archive
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1)), data.table(pout1 = 1))
expected_archive = data.table(p1 = 1, dob = 1, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 1, x_id = 1)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# eval from nonempty archive
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1:3)), data.table(pout1 = 1:3))
expected_archive = rbind(expected_archive,
  data.table(p1 = 1:3, dob = 2, eol = NA_real_, pout1 = 1:3, x_domain = list(list(p1 = 1), list(p1 = 2), list(p1 = 3)), batch_nr = 2, x_id = 2:4))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# eval with additional components
oismall = as_oi(get_objective_passthrough("minimize", TRUE))
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1, x1 = 10)), data.table(pout1 = 1))
expected_archive = data.table(p1 = 1, x1 = 10, dob = 1, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 1, x_id = 1)
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
  "fidelity.*ust be.*scalar")

# budget_id missing
expect_error(mies_evaluate_offspring(oismall, data.frame(p1 = 1), fidelity = 1),
  "budget_id.*not 'NULL'")

# reevaluate_fidelity not null
expect_error(mies_evaluate_offspring(oismall, data.frame(p1 = 1), reevaluate_fidelity = 1),
  "reevaluate_fidelity must be NULL")


# fidelity == 1, generation 1
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = 1), budget_id = "bud", fidelity = 1), data.table(pout1 = 1))
expected_archive = data.table(p1 = 1, bud = 1, dob = 1, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1, bud = 1)), batch_nr = 1, x_id = 1)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# fidelity == 2, generation 2
mies_evaluate_offspring(oismall, data.frame(p1 = 1:3), budget_id = "bud", fidelity = 2)
expected_archive = rbind(expected_archive,
  data.table(p1 = 1:3, bud = 2, dob = 2, eol = NA_real_, pout1 = 1:3, x_domain = lapply(1:3, function(x) (list(p1 = x, bud = 2))), batch_nr = 2, x_id = 2:4))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# fidelity == 4, generation 3
mies_evaluate_offspring(oismall, data.frame(p1 = 3:1), budget_id = "bud", fidelity = 4)
expected_archive = rbind(expected_archive,
  data.table(p1 = 3:1, bud = 4, dob = 3, eol = NA_real_, pout1 = 3:1, x_domain = lapply(3:1, function(x) (list(p1 = x, bud = 4))), batch_nr = 3, x_id = 5:7))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


oismall$archive$data[4, eol := 3]
expected_archive[1:3, eol := 4]
expected_archive[4, eol := 3]


# fidelity == 1 + reevaluate_fidelity == 3, fidelity_monotonic == TRUE, generation 4
mies_evaluate_offspring(oismall, data.frame(p1 = c(5, 7)), budget_id = "bud", fidelity = 1, reevaluate_fidelity = 3)
# new indivs
expected_archive = rbind(expected_archive,
  data.table(p1 = c(5, 7), bud = 1, dob = 4, eol = NA_real_, pout1 = c(5, 7), x_domain = lapply(c(5, 7), function(x) (list(p1 = x, bud = 1))), batch_nr = 4, x_id = 8:9))
# reevals
expected_archive = rbind(expected_archive,
  data.table(p1 = c(1, 1, 2), bud = 3, dob = 4, eol = NA_real_, pout1 = c(1, 1, 2), x_domain = lapply(c(1, 1, 2), function(x) (list(p1 = x, bud = 3))), batch_nr = 4, x_id = 1:3))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# fidelity == 3 + reevaluate_fidelity == 1, fidelity_monotonic == FALSE, generation 5
mies_evaluate_offspring(oismall, data.frame(p1 = c(5, 7)), budget_id = "bud", fidelity = 3, reevaluate_fidelity = 1, fidelity_monotonic = FALSE)
expected_archive[is.na(eol) & bud != 1, eol := 5]
expected_archive = rbind(expected_archive,
  data.table(p1 = c(5, 7), bud = 3, dob = 5, eol = NA_real_, pout1 = c(5, 7), x_domain = lapply(c(5, 7), function(x) (list(p1 = x, bud = 3))), batch_nr = 5, x_id = 10:11))
expected_archive = rbind(expected_archive,
  expected_archive[eol == 5][, `:=`(bud = 1, dob = 5, eol = NA, batch_nr = 5, x_domain = lapply(x_domain, function(y) { y$bud = 1 ; y }))])
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# fidelity, reevaluate_fidelity == 1, fidelity_monotonic = TRUE, generation 6 -> no reevals
mies_evaluate_offspring(oismall, data.frame(p1 = c(5, 7)), budget_id = "bud", fidelity = 1, reevaluate_fidelity = 1, fidelity_monotonic = TRUE)
expected_archive = rbind(expected_archive,
  data.table(p1 = c(5, 7), bud = 1, dob = 6, eol = NA_real_, pout1 = c(5, 7), x_domain = lapply(c(5, 7), function(x) (list(p1 = x, bud = 1))), batch_nr = 6, x_id = 12:13))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


# zero-row evaluations
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = numeric(0)), budget_id = "bud", fidelity = 1),
  data.table(pout1 = numeric(0)))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)
expect_equal(mies_evaluate_offspring(oismall, data.frame(p1 = numeric(0), bud = numeric(0))), data.table(pout1 = numeric(0)))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# larger searchspace
oilarge = as_oi(get_objective_passthrough("minimize", FALSE))
expect_equal(mies_evaluate_offspring(oilarge, generate_design_random(oilarge$search_space, 2)$data[, p1 := 1:2]), data.table(pout1 = 1:2))

# multi-objective
oilarge_mo = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE))
expect_equal(mies_evaluate_offspring(oilarge_mo, generate_design_random(oilarge$search_space, 2)$data[, p1 := 1:2][, p2 := 2:1]), data.table(pout1 = 1:2, pout2 = 2:1))
oismall_mo = as_oi(get_objective_passthrough(c("minimize", "maximize"), TRUE))
expect_equal(mies_evaluate_offspring(oismall_mo, generate_design_random(oismall_mo$search_space, 2)$data[, p1 := 1:2][, p2 := 2:1]), data.table(pout1 = 1:2, pout2 = 2:1))
expected_archive_mo = data.table(p1 = 1:2, p2 = 2:1, dob = 1, eol = NA_real_, pout1 = 1:2, pout2 = 2:1, x_domain = list(list(p1 = 1, p2 = 2), list(p1 = 2, p2 = 1)), batch_nr = 1, x_id = 1:2)
expect_equal(copy(oismall_mo$archive$data)[, timestamp := NULL], expected_archive_mo, ignore.col.order = TRUE)

