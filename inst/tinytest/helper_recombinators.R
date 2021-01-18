
# rec: recombinator to check
# recombinator_name: name to print in info
# is_primed: whether the recombinator is already primed. If so, the expectation that unprimed recombinator gives an error will not be checked.
expect_recombinator = function(rec, recombinator_name, is_primed = FALSE) {

  expect_r6(rec, "Recombinator", info = recombinator_name)

  p = ps(ParamLgl = p_lgl(), ParamDbl = p_dbl(0, 1), ParamInt = p_int(0, 1), ParamFct = p_fct(c("a", "b", "c")))

  if (!is_primed) {
    expect_error(rec$operate(pvals), "must be primed first", info = recombinator_name)
  }

  p_forbidden = p$clone(deep = TRUE)$subset(ids = setdiff(p$ids(), rec$param_classes))
  if (length(p_forbidden$ids())) {
    expect_error(rec$prime(p_forbidden), "Must be a subset of", info = recombinator_name)
  }

  p_allowed = p$clone(deep = TRUE)$subset(ids = rec$param_classes)
  pvals_allowed = generate_design_random(p_allowed, 3 * rec$n_indivs_in)$data

  rec$prime(p_allowed)

  test_alloweds = function(data, pp) {
    rec$prime(pp)
    recombined = rec$operate(data)
    expect_true(pp$test_dt(recombined), info = recombinator_name)
    expect_data_table(recombined, nrows = nrow(data) / rec$n_indivs_in * rec$n_indivs_out,
      ncols = ncol(data), any.missing = FALSE, info = recombinator_name)
  }

  test_alloweds(pvals_allowed, p_allowed)
  test_alloweds(pvals_allowed[seq_len(rec$n_indivs_in)], p_allowed)

  p_allowed_one = p_allowed$clone(deep = TRUE)$subset(p_allowed$ids()[[1]])

  test_alloweds(pvals_allowed[, 1, with = FALSE], p_allowed_one)
  test_alloweds(pvals_allowed[seq_len(rec$n_indivs_in), 1, with = FALSE], p_allowed_one)

  p_allowed_multicol = ParamSet$new(lapply(letters[1:3], function(x) {
    par = p_allowed$params[[1]]$clone(deep = TRUE)
    par$id = x
    par
  }))
  pvals_allowed_multicol = generate_design_random(p_allowed_multicol, 3 * rec$n_indivs_in)$data

  expect_error(rec$operate(pvals_allowed_multicol), "Parameter 'a' not available", info = recombinator_name)

  test_alloweds(pvals_allowed_multicol, p_allowed_multicol)
  test_alloweds(pvals_allowed_multicol[seq_len(rec$n_indivs_in)], p_allowed_multicol)

  expect_true(rec$endomorphism)
}
