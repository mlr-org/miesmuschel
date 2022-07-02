

source("setup.R", local = TRUE)

oibig = as_oi(get_objective_passthrough("minimize", FALSE, "bud"))
oibigmulti = as_oi(get_objective_passthrough(c("minimize", "maximize"), FALSE, "bud"))

design = cbind(generate_design_random(oibig$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

designmultiobj = cbind(generate_design_random(oibigmulti$search_space, 9)$data[, bud := c(1, 1, 1, 3, 3, 7, 5, 5, 9)],
  data.table(additional = 1:9, dob = rep(1:3, each = 3), eol = rep(c(3, NA, NA), 3))
)

ac = ps(additional = p_int(1, 9))

expect_reevald = function(rows, budget, oi, additional = TRUE, multiobj = FALSE) {
  if (multiobj) design = designmultiobj
  expected_archive = rbind(
    cbind(copy(design), batch_nr = 1, pout1 = design$p1)[rows, eol := 3],
    cbind(copy(design), batch_nr = 2, pout1 = design$p1)[, dob := 3][, bud := budget][rows]
  )
  if (multiobj) expected_archive[, pout2 := p2]
  expected_archive$x_domain = transpose_list(expected_archive[, oi$search_space$ids(), with = FALSE])
  if (!additional) expected_archive[, additional := NULL]
  expect_equal(copy(oi$archive$data)[, timestamp := NULL], expected_archive, ignore.col.order = TRUE)
#  print(copy(oi$archive$data)[, timestamp := NULL][])
#  print(expected_archive)
}

oibig$clear()
oibig$eval_batch(design)
expect_reevald(integer(0), 1, oibig)


# no reeval: budget is 1, the smallest, and reeval is monotonic
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 1, additional_components = ac)
expect_reevald(integer(0), 1, oibig)

# reeval of rows 2:3 (alive with budget 1)
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 2, additional_components = ac)
expect_reevald(2:3, 2, oibig)


# reeval 2:3, 5, 8 (budget 6)
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 6, additional_components = ac)
expect_reevald(c(2:3, 5, 8), 6, oibig)

# no reeval
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 1, additional_components = ac)
expect_reevald(integer(0), 6, oibig)


# reeval 8 (budget 6): current gen only
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 6, additional_components = ac, current_gen_only = TRUE)
expect_reevald(8, 6, oibig)

# no reeval: current gen only
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 5, additional_components = ac, current_gen_only = TRUE)
expect_reevald(integer(0), 5, oibig)

# reeval 9: current gen only, nonmonotonic
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 5, additional_components = ac, current_gen_only = TRUE, fidelity_monotonic = FALSE)
expect_reevald(9, 5, oibig)

# reeval 2:3, 5:6, 9: nonmonotonic
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 5, additional_components = ac, fidelity_monotonic = FALSE)
expect_reevald(c(2:3, 5:6, 9), 5, oibig)

# reeval 2:3, 5:6, 9: nonmonotonic; no additional component
oibig$clear()
oibig$eval_batch(copy(design)[, additional := NULL])
mies_step_fidelity(oibig, "bud", 5, fidelity_monotonic = FALSE)
expect_reevald(c(2:3, 5:6, 9), 5, oibig, additional = FALSE)

# no reeval: current gen only; no additional component
oibig$clear()
oibig$eval_batch(copy(design)[, additional := NULL])
mies_step_fidelity(oibig, "bud", 5, current_gen_only = TRUE)
expect_reevald(integer(0), 5, oibig, additional = FALSE)


# reeval 2:3, 5:6, 9: nonmonotonic; multiobjective
oibigmulti$clear()
oibigmulti$eval_batch(designmultiobj)
mies_step_fidelity(oibigmulti, "bud", 5, fidelity_monotonic = FALSE, additional_components = ac)
expect_reevald(c(2:3, 5:6, 9), 5, oibigmulti, multiobj = TRUE)

# no reeval: current gen only; multiobjective
oibigmulti$clear()
oibigmulti$eval_batch(designmultiobj)
mies_step_fidelity(oibigmulti, "bud", 5, current_gen_only = TRUE, additional_components = ac)
expect_reevald(integer(0), 5, oibigmulti, multiobj = TRUE)


oibig$clear()
oibig$eval_batch(design)
# termination during fidelity step does not change archive
archive_expected = copy(oibig$archive$data)
oibig$terminator = bbotk::TerminatorEvals$new()
oibig$terminator$param_set$values$n_evals = 9
expect_error(mies_step_fidelity(oibig, "bud", 5, additional_components = ac), class = "terminated_error", "TerminatorEvals")
expect_equal(oibig$archive$data, archive_expected)

# generation terminator doesn't trigger in fidelity step
# reeval of rows 2:3 (alive with budget 1)
oibig$terminator = TerminatorGenerations$new()
oibig$terminator$param_set$values$generations = 3
oibig$clear()
oibig$eval_batch(design)
mies_step_fidelity(oibig, "bud", 2, additional_components = ac)  # no termination triggered
expect_reevald(2:3, 2, oibig)
expect_error(oibig$eval_batch(design), class = "terminated_error", "TerminatorGenerations")  # .. but not because the terminator doesn't do anything
oibig$terminator = bbotk::TerminatorNone$new()


# fill in coverage
oibig$clear()
oibig$eval_batch(copy(design)[, eol := NULL])
expect_error(mies_step_fidelity(oibig, "bud", 1), "No alive individuals. Need to run mies_init_population")
