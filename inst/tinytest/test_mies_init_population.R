
source("setup.R", local = TRUE)

oismall = as_oi(get_objective_passthrough("minimize", TRUE))

# dob and eol are not accepted in either search space or additional components
expect_error(mies_init_population(as_oi(get_objective_passthrough("minimize", TRUE, "dob")), mu = 2), "search_space.*disjunct from.*dob")
expect_error(mies_init_population(as_oi(get_objective_passthrough("minimize", TRUE, "eol")), mu = 2), "search_space.*disjunct from.*eol")
expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("dob", 0, 1), rfun = function(n) rep(0, n))), "dob.*may not be additional component")
expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("eol", 0, 1), rfun = function(n) rep(0, n))), "eol.*may not be additional component")

expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("p1", 0, 1), rfun = function(n) rep(0, n))), "Search space and additional copmonents name clash: p1")
expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("pout1", 0, 1), rfun = function(n) rep(0, n))), "codomain and additional components name clash: pout1")

oilarge = as_oi(get_objective_passthrough("minimize", FALSE))
mies_init_population(oilarge, mu = 3)
expect_data_table(oilarge$archive$data, nrow = 3)
expect_true(oilarge$search_space$check_dt(oilarge$archive$data[, oilarge$search_space$ids(), with = FALSE]))

oilarge$archive$data[, dob := NULL]
expect_error(mies_init_population(oilarge, mu = 3), "'eol' but not 'dob' column found")

# init empty archive with 3 samples
mies_init_population(oismall, mu = 3, initializer = generate_design_increasing)
expected_archive = data.table(p1 = 1:3, dob = 1, eol = NA_real_, pout1 = 1:3, x_domain = list(list(p1 = 1), list(p1 = 2), list(p1 = 3)), batch_nr = 1)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# init initialized archive with 2 more samples
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive = rbind(expected_archive, data.table(p1 = 1:2, dob = 2, eol = NA_real_, pout1 = 1:2, x_domain = list(list(p1 = 1), list(p1 = 2)), batch_nr = 2))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# recognize need for one more row when one sample is declared dead
oismall$archive$data[2, eol := 2]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive[2, eol := 2]
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 3, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 3))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# when no eol column is found, the trailing rows are taken
oismall$archive$data[, eol := NULL]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive[, eol := NULL][, eol := c(1, rep(NA, 5))]
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# when no dob / eol is found, both are inited to 0
oismall$archive$data[, dob := NULL][, eol := NULL]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive[, eol := NULL][, dob := NULL][, dob := 0][, eol := c(0, rep(NA, 5))]
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# no dob / eol, but new generations are sampled:
oismall$archive$data = oismall$archive$data[, dob := NULL][, eol := NULL][1:3]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive = expected_archive[, eol := NULL][, dob := NULL][1:5][, dob := rep(0:1, c(3, 2))][, eol := NA_real_]
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# additional components when other components already exist
additional_component_sampler = generate_increasing_sampler(ps(x1 = p_dbl(0, 100), x2 = p_dbl(0, 100)))
mies_init_population(oismall, mu = 6, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 2, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 3))
expected_archive[, c("x1", "x2") := .(1:6, 1:6)]
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# additional components with partial NA generate error
oismall$archive$data[2, x1 := NA]
expect_error(mies_init_population(oismall, mu = 6, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler),
  "where some, but not all additional component values are NA")
expect_error(mies_init_population(oismall, mu = 7, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler),
  "where some, but not all additional component values are NA")
oismall$archive$data[, x1 := NULL]
expect_error(mies_init_population(oismall, mu = 6, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler),
  "Some, but not all, additional components already in archive: x2")
expect_error(mies_init_population(oismall, mu = 7, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler),
  "Some, but not all, additional components already in archive: x2")
oismall$archive$data[, x1 := 1:6]

# additional components with full NA row
oismall$archive$data[2, x1 := NA]
oismall$archive$data[2, x2 := NA]
mies_init_population(oismall, mu = 6, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive[2, c("x1", "x2") := .(1, 1)]
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# additoinal components with full NA row, plus new row to be sampled
oismall$archive$data[2, x1 := NA]
oismall$archive$data[2, x2 := NA]
mies_init_population(oismall, mu = 7, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive[2, c("x1", "x2") := .(1, 1)]
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 3, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 4, x1 = 2, x2 = 2))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# additional components on empty archive
oismall2 = as_oi(get_objective_passthrough("minimize", TRUE))
mies_init_population(oismall2, mu = 3, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive[2, c("x1", "x2") := .(2, 2)][1:3, dob := 1]
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3], ignore.col.order = TRUE)

# additional components on nonempty archive where additional components already exist and number of alives is reduced
mies_init_population(oismall2, mu = 2, initializer = generate_design_increasing,
  additional_component_sampler = SamplerUnif$new(additional_component_sampler$param_set))  # This is needed because of https://github.com/mlr-org/paradox/issues/338: sampler needs to generate 0 rows but that doesn't work currently.
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3][1, eol := 1][], ignore.col.order = TRUE)

# additional components on nonempty archive where additional components already exist and number of alives stays the same
oismall3 = as_oi(get_objective_passthrough("minimize", TRUE))
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing,
  additional_component_sampler = SamplerUnif$new(additional_component_sampler$param_set))  # This is needed because of https://github.com/mlr-org/paradox/issues/338: sampler needs to generate 0 rows but that doesn't work currently.
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive[1:3], ignore.col.order = TRUE)

# additional components on nonempty archive where additional components already exist and number of alives stays the same
mies_init_population(oismall3, mu = 5, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive = expected_archive[1:5][4:5, dob := 2][4:5, c("x1", "x2") := .(1:2, 1:2)]
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# number of alives is reduced, additional components are added
oismall3$archive$data[, x1 := NULL][, x2 := NULL]
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive$eol = rep(c(1, NA), c(2, 3))
expected_archive$x1 = expected_archive$x2 = c(NA, NA, 1:3)
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# Number of alives is reduced, additoinal components are partially added
oismall3$archive$data$x1 = oismall3$archive$data$x2 = c(NA, NA, NA, 2, NA)
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive$eol = rep(c(1, NA), c(3, 2))
expected_archive$x1 = expected_archive$x2 = c(NA, NA, NA, 2:1)
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# initializing without additional components, when components are present
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing)
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 3, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 3, x1 = NA, x2 = NA))
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# multi-objective
oimo = as_oi(get_objective_passthrough(c("minimize", "maximize"), TRUE))
mies_init_population(oimo, mu = 3, initializer = generate_design_increasing)
expected_archive = data.table(p1 = 1:3, p2 = 1:3, dob = 1, eol = NA_real_, pout1 = 1:3, pout2 = 1:3, x_domain = list(list(p1 = 1, p2 = 1), list(p1 = 2, p2 = 2), list(p1 = 3, p2 = 3)), batch_nr = 1)
expect_equal(copy(oimo$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# budget
oibu = as_oi(get_objective_passthrough("minimize", TRUE, "bud"))
fidelity_schedule = data.frame(
  generation = c(1, 3),
  budget_new = c(1, 2),
  budget_survivors = c(2, 3)
)

expect_error(mies_init_population(inst = oibu, mu = 3, initializer = generate_design_increasing, fidelity_schedule = fidelity_schedule),
  "budget_id.*not 'NULL'")

expect_error(mies_init_population(inst = oibu, mu = 3, initializer = generate_design_increasing, budget_id = "bud"),
  "fidelity_schedule.*must be a data.frame")

mies_init_population(inst = oibu, mu = 3, initializer = generate_design_increasing, fidelity_schedule = fidelity_schedule, budget_id = "bud")
expected_archive = data.table(p1 = 1:3, bud = 2, dob = 1, eol = NA_real_, pout1 = 1:3, x_domain = lapply(1:3, function(x) list(p1 = x, bud = 2)), batch_nr = 1)
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# same budget in gen 2
mies_init_population(inst = oibu, mu = 5, initializer = generate_design_increasing, fidelity_schedule = fidelity_schedule, budget_id = "bud")
expected_archive = rbind(expected_archive, data.table(p1 = 1:2, bud = 2, dob = 2, eol = NA_real_, pout1 = 1:2, x_domain = lapply(1:2, function(x) list(p1 = x, bud = 2)), batch_nr = 2))
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# different budget in gen 3
mies_init_population(inst = oibu, mu = 8, initializer = generate_design_increasing, fidelity_schedule = fidelity_schedule, budget_id = "bud")
expected_archive = rbind(expected_archive, data.table(p1 = 1:3, bud = 3, dob = 3, eol = NA_real_, pout1 = 1:3, x_domain = lapply(1:3, function(x) list(p1 = x, bud = 3)), batch_nr = 3))
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


# coverage
