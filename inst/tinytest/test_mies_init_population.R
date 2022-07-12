
source("setup.R", local = TRUE)

set.seed(1)
oismall = as_oi(get_objective_passthrough("minimize", TRUE))

# dob and eol are not accepted in either search space or additional components
expect_error(mies_init_population(as_oi(get_objective_passthrough("minimize", TRUE, "dob")), mu = 2), "search_space.*disjunct from.*dob")
expect_error(mies_init_population(as_oi(get_objective_passthrough("minimize", TRUE, "eol")), mu = 2), "search_space.*disjunct from.*eol")
expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("dob", 0, 1), rfun = function(n) rep(0, n))), "dob.*may not be additional component")
expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("eol", 0, 1), rfun = function(n) rep(0, n))), "eol.*may not be additional component")

expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("p1", 0, 1), rfun = function(n) rep(0, n))), "Search space and additional components name clash: p1")

expect_error(mies_init_population(oismall, mu = 2,
  additional_component_sampler = Sampler1DRfun$new(param = ParamDbl$new("pout1", 0, 1), rfun = function(n) rep(0, n))), "codomain and additional components name clash: pout1")

oilarge = as_oi(get_objective_passthrough("minimize", FALSE))
mies_init_population(oilarge, mu = 3)
expect_data_table(oilarge$archive$data, nrows = 3)
expect_true(oilarge$search_space$check_dt(oilarge$archive$data[, oilarge$search_space$ids(), with = FALSE]))

oilarge$archive$data[, dob := NULL]
expect_error(mies_init_population(oilarge, mu = 3), "'eol' but not 'dob' column found")

# init empty archive with 3 samples
mies_init_population(oismall, mu = 3, initializer = generate_design_increasing)
expected_archive = data.table(p1 = 1:3, dob = 1, eol = NA_real_, pout1 = 1:3, x_domain = list(list(p1 = 1), list(p1 = 2), list(p1 = 3)), batch_nr = 1, x_id = 1:3)
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# init initialized archive with 2 more samples
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive = rbind(expected_archive, data.table(p1 = 1:2, dob = 2, eol = NA_real_, pout1 = 1:2, x_domain = list(list(p1 = 1), list(p1 = 2)), batch_nr = 2, x_id = 4:5))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# recognize need for one more row when one sample is declared dead
oismall$archive$data[2, eol := 2]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive[2, eol := 2]
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 3, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 3, x_id = 6))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# when no eol column is found, the trailing rows are taken
oismall$archive$data[, eol := NULL]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
# we are in generation 3, so the end of life is set to 3
expected_archive[, eol := NULL][, eol := NA_real_][(p1 == 3), eol := 3]  # the () are used to avoid creation of an index here
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# when no dob / eol is found, both are inited to 0
oismall$archive$data[, dob := NULL][, eol := NULL]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
# missing dob means we are generation 0, so the end of life is set to 0
expected_archive[, eol := NULL][, eol := NA_real_][, dob := 0][(p1 == 3), eol := 0]  # the () are used to avoid creation of an index here
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# no dob / eol, but new generations are sampled:
oismall$archive$data = oismall$archive$data[, dob := NULL][, eol := NULL][1:3]
mies_init_population(oismall, mu = 5, initializer = generate_design_increasing)
expected_archive = expected_archive[, eol := NULL][, dob := NULL][1:5][, dob := rep(0:1, c(3, 2))][, eol := NA_real_]
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# additional components when other components already exist
additional_component_sampler = generate_increasing_sampler(ps(x1 = p_dbl(0, 100), x2 = p_dbl(0, 100)))
mies_init_population(oismall, mu = 6, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 2, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 3, x_id = 6))
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
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 3, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 4, x1 = 2, x2 = 2, x_id = 7))
expect_equal(copy(oismall$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# additional components on empty archive
oismall2 = as_oi(get_objective_passthrough("minimize", TRUE))
halfss_selector = SelectorBest$new()$prime(oismall2$search_space)
fullss_selector = SelectorBest$new()

mies_prime_operators(oismall2$search_space, selectors = list(fullss_selector), additional_components = additional_component_sampler$param_set)
cached_expected = copy(expected_archive)

mies_init_population(oismall2, mu = 3, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive[2, c("x1", "x2") := .(2, 2)][1:3, dob := 1]
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3], ignore.col.order = TRUE)
# additional components on nonempty archive where additional components already exist and number of alives is reduced
mies_init_population(oismall2, mu = 2, initializer = generate_design_increasing,
  additional_component_sampler = SamplerUnif$new(additional_component_sampler$param_set))  # This is needed because of https://github.com/mlr-org/paradox/issues/338: sampler needs to generate 0 rows but that doesn't work currently.
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3][3, eol := 1][], ignore.col.order = TRUE)

## same, but with fullss_selector
oismall2$clear()
expected_archive = copy(cached_expected)
mies_init_population(oismall2, mu = 3, initializer = generate_design_increasing, survival_selector = fullss_selector, additional_component_sampler = additional_component_sampler)
expected_archive[2, c("x1", "x2") := .(2, 2)][1:3, dob := 1]
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3], ignore.col.order = TRUE)
mies_init_population(oismall2, mu = 2, initializer = generate_design_increasing,
  additional_component_sampler = SamplerUnif$new(additional_component_sampler$param_set))
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3][3, eol := 1][], ignore.col.order = TRUE)

## same, but with halfss_selector
oismall2$clear()
expected_archive = copy(cached_expected)
mies_init_population(oismall2, mu = 3, initializer = generate_design_increasing, survival_selector = halfss_selector, additional_component_sampler = additional_component_sampler)
expected_archive[2, c("x1", "x2") := .(2, 2)][1:3, dob := 1]
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3], ignore.col.order = TRUE)
mies_init_population(oismall2, mu = 2, initializer = generate_design_increasing,
  additional_component_sampler = SamplerUnif$new(additional_component_sampler$param_set))
expect_equal(copy(oismall2$archive$data)[, timestamp := NULL], expected_archive[1:3][3, eol := 1][], ignore.col.order = TRUE)

# additional components on nonempty archive where additional components already exist and number of alives stays the same
oismall3 = as_oi(get_objective_passthrough("minimize", TRUE))
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing,
  additional_component_sampler = SamplerUnif$new(additional_component_sampler$param_set))  # This is needed because of https://github.com/mlr-org/paradox/issues/338: sampler needs to generate 0 rows but that doesn't work currently.
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive[1:3], ignore.col.order = TRUE)

# additional components on nonempty archive where additional components already exist and number of alives increases
mies_init_population(oismall3, mu = 5, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive = expected_archive[1:5][4:5, dob := 2][4:5, c("x1", "x2") := .(1:2, 1:2)]
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# number of alives is reduced, additional components are added
cached_expected = copy(expected_archive)
cached_oismall3 = oismall3$clone(deep = TRUE)

oismall3$archive$data[, x1 := NULL][, x2 := NULL]
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
deleting = c(2, 3, 5)
expected_archive$eol = NA_real_
expected_archive$eol[deleting] = 2
expected_archive$x1[deleting] = expected_archive$x2[deleting] = NA_real_
expected_archive$x1[-deleting] = expected_archive$x2[-deleting] = c(1, 2)
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# same, but with fullss_selector
expected_archive = copy(cached_expected)
oismall3 = cached_oismall3$clone(deep = TRUE)
oismall3$archive$data[, x1 := NULL][, x2 := NULL]
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, survival_selector = fullss_selector, additional_component_sampler = additional_component_sampler)
deleting = c(2, 3, 5)
expected_archive$eol = NA_real_
expected_archive$eol[deleting] = 2
expected_archive$x1 = expected_archive$x2 = as.numeric(1:5)
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# same, but with halfss_selector
expected_archive = copy(cached_expected)
oismall3 = cached_oismall3$clone(deep = TRUE)
oismall3$archive$data[, x1 := NULL][, x2 := NULL]
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, survival_selector = halfss_selector, additional_component_sampler = additional_component_sampler)
deleting = c(2, 3, 5)
expected_archive$eol = NA_real_
expected_archive$eol[deleting] = 2
expected_archive$x1[deleting] = expected_archive$x2[deleting] = NA_real_
expected_archive$x1[-deleting] = expected_archive$x2[-deleting] = c(1, 2)
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


# Number of alives is reduced, additoinal components are partially added
cached_expected = copy(expected_archive)
cached_oismall3 = oismall3$clone(deep = TRUE)

oismall3$archive$data$x1 = oismall3$archive$data$x2 = c(NA, NA, NA, 2, NA)
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, additional_component_sampler = additional_component_sampler)
expected_archive$eol = NA_real_
expected_archive[(pout1 != 1), eol := 2][, `:=`(x1 = NA_real_, x2 = NA_real_)]
expected_archive[(pout1 == 1), x1 := c(1, 2)]
expected_archive[(pout1 == 1), x2 := c(1, 2)]
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# same, but with fullss_selector
expected_archive = copy(cached_expected)
oismall3 = cached_oismall3$clone(deep = TRUE)
oismall3$archive$data$x1 = oismall3$archive$data$x2 = c(NA, NA, NA, 2, NA)
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, survival_selector = fullss_selector, additional_component_sampler = additional_component_sampler)
expected_archive$eol = NA_real_
expected_archive[(pout1 != 1), eol := 2][, `:=`(x1 = NA_real_, x2 = NA_real_)]
expected_archive[c(1, 4), x1 := c(1, 2)]
expected_archive[c(1, 4), x2 := c(1, 2)]
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# same, but with halfss_selector
expected_archive = copy(cached_expected)
oismall3 = cached_oismall3$clone(deep = TRUE)
oismall3$archive$data$x1 = oismall3$archive$data$x2 = c(NA, NA, NA, 2, NA)
mies_init_population(oismall3, mu = 2, initializer = generate_design_increasing, survival_selector = halfss_selector, additional_component_sampler = additional_component_sampler)
expected_archive$eol = NA_real_
expected_archive[(pout1 != 1), eol := 2][, `:=`(x1 = NA_real_, x2 = NA_real_)]
expected_archive[(pout1 == 1), x1 := c(1, 2)]
expected_archive[(pout1 == 1), x2 := c(1, 2)]
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# initializing without additional components, when components are present
mies_init_population(oismall3, mu = 3, initializer = generate_design_increasing)
expected_archive = rbind(expected_archive, data.table(p1 = 1, dob = 3, eol = NA_real_, pout1 = 1, x_domain = list(list(p1 = 1)), batch_nr = 3, x1 = NA, x2 = NA, x_id = 6))
expect_equal(copy(oismall3$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# multi-objective
oimo = as_oi(get_objective_passthrough(c("minimize", "maximize"), TRUE))
mies_init_population(oimo, mu = 3, initializer = generate_design_increasing)
expected_archive = data.table(p1 = 1:3, p2 = 1:3, dob = 1, eol = NA_real_, pout1 = 1:3, pout2 = 1:3, x_domain = list(list(p1 = 1, p2 = 1), list(p1 = 2, p2 = 2), list(p1 = 3, p2 = 3)), batch_nr = 1, x_id = 1:3)
expect_equal(copy(oimo$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# budget
oibu = as_oi(get_objective_passthrough("minimize", TRUE, "bud"))

expect_error(mies_init_population(inst = oibu, mu = 3, initializer = generate_design_increasing, fidelity = 1),
  "budget_id.*not 'NULL'")

expect_error(mies_init_population(inst = oibu, mu = 3, initializer = generate_design_increasing, budget_id = "bud"),
  "fidelity.*atomic scalar")

mies_init_population(inst = oibu, mu = 3, initializer = generate_design_increasing, fidelity = 1, budget_id = "bud")
expected_archive = data.table(p1 = 1:3, bud = 1, dob = 1, eol = NA_real_, pout1 = 1:3, x_domain = lapply(1:3, function(x) list(p1 = x, bud = 1)), batch_nr = 1, x_id = 1:3)
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# different budget, no reeval
mies_init_population(inst = oibu, mu = 5, initializer = generate_design_increasing, fidelity = 3, budget_id = "bud", fidelity_new_individuals_only = TRUE)
expected_archive = rbind(expected_archive, data.table(p1 = 1:2, bud = 3, dob = 2, eol = NA_real_, pout1 = 1:2, x_domain = lapply(1:2, function(x) list(p1 = x, bud = 3)), batch_nr = 2, x_id = 4:5))
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


# different budget, with reeval, monotonic = TRUE
cached_oibu = oibu$clone(deep = TRUE)
mies_init_population(inst = oibu, mu = 7, initializer = generate_design_increasing, fidelity = 2, budget_id = "bud")
expected_archive[(bud == 1), eol := 3]

expected_archive = rbind(expected_archive, data.table(p1 = c(1:2, 1:3), bud = 2, dob = 3, eol = NA_real_, pout1 = c(1:2, 1:3), x_domain = lapply(c(1:2, 1:3), function(x) list(p1 = x, bud = 2)), batch_nr = 3, x_id = c(6:7, 1:3)))
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

## fidelity decreases, !monotonic
oibu = cached_oibu$clone(deep = TRUE)
mies_init_population(inst = oibu, mu = 9, initializer = generate_design_increasing, fidelity = 2, budget_id = "bud", fidelity_monotonic = FALSE)
expected_archive[(bud %in% c(1, 3)), eol := 3]
expected_archive = rbind(expected_archive[1:5], data.table(p1 = c(1:4, 1:3, 1:2), bud = 2, dob = 3, eol = NA_real_, pout1 = c(1:4, 1:3, 1:2), x_domain = lapply(c(1:4, 1:3, 1:2), function(x) list(p1 = x, bud = 2)), batch_nr = 3, x_id = c(6:9, 1:5)))
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


cached_oibu = oibu$clone(deep = TRUE)
cached_expected = copy(expected_archive)

mies_prime_operators(oibu$search_space, selectors = list(halfss_selector), budget_id = "bud")
mies_prime_operators(oibu$search_space, selectors = list(fullss_selector), budget_id = "bud", additional_components = additional_component_sampler$param_set)

## indivs die, fidelity increases, additional components
mies_init_population(oibu, mu = 3, initializer = generate_design_increasing, fidelity = 4, budget_id = "bud", additional_component_sampler = additional_component_sampler)
expected_archive[(pout1 > 1), eol := 3]
expected_archive[(is.na(eol)), `:=`(x1 = c(1, 2, 3), x2 = c(1, 2, 3))]
expected_archive = rbind(copy(expected_archive)[(is.na(eol)), eol := 4],
  expected_archive[(is.na(eol))][, `:=`(dob = 4, bud = 4, batch_nr = 4)][, x_domain := lapply(x_domain, function(xd) { xd$bud = 4 ; xd})])
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)

# same with fullss_selector
oibu = cached_oibu$clone(deep = TRUE)
expected_archive = copy(cached_expected)
mies_init_population(oibu, mu = 3, initializer = generate_design_increasing, fidelity = 4, budget_id = "bud", survival_selector = fullss_selector, additional_component_sampler = additional_component_sampler)
expected_archive[(is.na(eol)), `:=`(x1 = as.numeric(seq_len(.N)), x2 = as.numeric(seq_len(.N)))]
expected_archive[(pout1 > 1), eol := 3]

expected_archive = rbind(copy(expected_archive)[(is.na(eol)), eol := 4],
  expected_archive[(is.na(eol))][, `:=`(dob = 4, bud = 4, batch_nr = 4)][, x_domain := lapply(x_domain, function(xd) { xd$bud = 4 ; xd})])
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)


# same with halfss_selector
oibu = cached_oibu$clone(deep = TRUE)
expected_archive = copy(cached_expected)
mies_init_population(oibu, mu = 3, initializer = generate_design_increasing, fidelity = 4, budget_id = "bud", survival_selector = halfss_selector, additional_component_sampler = additional_component_sampler)
expected_archive[(pout1 > 1), eol := 3]
expected_archive[(is.na(eol)), `:=`(x1 = c(1, 2, 3), x2 = c(1, 2, 3))]
expected_archive = rbind(copy(expected_archive)[(is.na(eol)), eol := 4],
  expected_archive[(is.na(eol))][, `:=`(dob = 4, bud = 4, batch_nr = 4)][, x_domain := lapply(x_domain, function(xd) { xd$bud = 4 ; xd})])
expect_equal(copy(oibu$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)









# coverage
