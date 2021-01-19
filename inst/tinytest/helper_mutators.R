
# mut: mutator to check
# mutator_name: name to print in info
# is_primed: whether the mutator is already primed. if so, the expectation that unprimed mutator gives an error will not be checked.
expect_mutator = function(mut, mutator_name, is_primed = FALSE) {

  expect_r6(mut, "Mutator", info = mutator_name)

  # names may not be param classes directly, so add a period here
  p = ps(ParamLgl. = p_lgl(), ParamDbl. = p_dbl(0, 1), ParamInt. = p_int(0, 1), ParamFct. = p_fct(c("a", "b", "c")))

  if (!is_primed) {
    expect_error(mut$operate(pvals), "must be primed first", info = mutator_name)
  }

  p_forbidden = p$clone(deep = TRUE)$subset(ids = setdiff(p$ids(), paste0(mut$param_classes, ".")))
  if (length(p_forbidden$ids())) {
    expect_error(mut$prime(p_forbidden), "Must be a subset of", info = mutator_name)
    # pvals_forbidden = generate_design_random(p_forbidden, 1)$data
  }

  p_allowed = p$clone(deep = TRUE)$subset(ids = paste0(mut$param_classes, "."))
  pvals_allowed = generate_design_random(p_allowed, 3)$data

  mut$prime(p_allowed)

  test_alloweds = function(data, pp) {
    mut$prime(pp)
    mutated = mut$operate(data)
    expect_true(pp$test_dt(mutated), info = mutator_name)
    expect_data_table(mutated, nrows = nrow(data), ncols = ncol(data), any.missing = FALSE, info = mutator_name)
  }

  test_alloweds(pvals_allowed, p_allowed)
  test_alloweds(pvals_allowed[1], p_allowed)

  p_allowed_one = p_allowed$clone(deep = TRUE)$subset(p_allowed$ids()[[1]])

  test_alloweds(pvals_allowed[, 1, with = FALSE], p_allowed_one)
  test_alloweds(pvals_allowed[1, 1, with = FALSE], p_allowed_one)

  p_allowed_multicol = ParamSet$new(lapply(letters[1:3], function(x) {
    par = p_allowed$params[[1]]$clone(deep = TRUE)
    par$id = x
    par
  }))
  pvals_allowed_multicol = generate_design_random(p_allowed_multicol, 3)$data

  expect_error(mut$operate(pvals_allowed_multicol), "Parameter 'a' not available", info = mutator_name)

  test_alloweds(pvals_allowed_multicol, p_allowed_multicol)
  test_alloweds(pvals_allowed_multicol[1], p_allowed_multicol)

  expect_true(mut$endomorphism)
}

