
# sel: selector to check
# selector_name: name to print in info
# can_oversample: whether the selector can select more than values are present
# is_primed: whether the selector is already primed. If so, the expectation that unprimed selector gives an error will not be checked.
expect_selector = function(sel, selector_name, can_oversample = TRUE, is_primed = FALSE) {
  expect_r6(sel, "Selector", info = selector_name)

  p = ps(ParamLgl = p_lgl(), ParamDbl = p_dbl(0, 1), ParamInt = p_int(0, 1), ParamFct = p_fct(c("a", "b", "c")))

  if (!is_primed) {
    expect_error(sel$operate(pvals), "must be primed first", info = selector_name)
  }

  p_forbidden = p$clone(deep = TRUE)$subset(ids = setdiff(p$ids(), sel$param_classes))
  if (length(p_forbidden$ids())) {
    expect_error(sel$prime(p_forbidden), "Must be a subset of", info = selector_name)
  }

  p_allowed = p$clone(deep = TRUE)$subset(ids = sel$param_classes)
  pvals_allowed = generate_design_random(p_allowed, 3)$data

  sel$prime(p_allowed)

  test_alloweds = function(data, pp) {
    sel$prime(pp)
    if ("single-crit" %in% sel$supported) {
      for (i in 1:4) {
        if (!can_oversample && i > nrow(data)) break
        expect_integerish(sel$operate(data, seq_len(nrow(data)), i),
          lower = 1, upper = nrow(data), any.missing = FALSE, tol = 1e-100, len = i)
      }
    }
    if ("multi-crit" %in% sel$supported) {
      for (i in 1:4) {
        if (!can_oversample && i > nrow(data)) break
        expect_integerish(sel$operate(data, matrix(seq_len(nrow(data) * 3), ncol = 3), i),
          lower = 1, upper = nrow(data), any.missing = FALSE, tol = 1e-100, len = i)
      }
    }
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

  expect_error(sel$operate(pvals_allowed_multicol), "Parameter 'a' not available", info = selector_name)

  test_alloweds(pvals_allowed_multicol, p_allowed_multicol)
  test_alloweds(pvals_allowed_multicol[1], p_allowed_multicol)


  expect_false(sel$endomorphism)
}
