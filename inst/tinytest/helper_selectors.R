
# sel: selector to check
# selector_name: name to print in info
# can_oversample: whether the selector can select more than values are present
# is_primed: whether the selector is already primed. If so, the expectation that unprimed selector gives an error will not be checked.
expect_selector = function(sel, selector_name, can_oversample = TRUE, is_primed = FALSE) {
  expect_r6(sel, "Selector", info = selector_name)

  p = ps(ParamLgl. = p_lgl(), ParamDbl. = p_dbl(0, 1), ParamInt. = p_int(0, 1), ParamFct. = p_fct(c("a", "b", "c")))
  pbig = ps(ParamLgl. = p_lgl(), ParamDbl. = p_dbl(0, 1), ParamInt. = p_int(0, 1), ParamFct. = p_fct(c("a", "b", "c")),
    ParamLgl.1 = p_lgl(), ParamDbl.1 = p_dbl(0, 1), ParamInt.1 = p_int(0, 1), ParamFct.1 = p_fct(c("a", "b", "c")))

  p_forbidden = p$clone(deep = TRUE)$subset(ids = setdiff(p$ids(), paste0(sel$param_classes, ".")))
  if (length(p_forbidden$ids())) {
    expect_error(sel$prime(p_forbidden), "Must be a subset of", info = selector_name)
  }

  p_allowed = p$clone(deep = TRUE)$subset(ids = paste0(sel$param_classes, "."))
  pvals_allowed = generate_design_random(p_allowed, 3)$data

  if (!is_primed) {
    expect_error(sel$operate(pvals_allowed, seq_len(nrow(pvals_allowed)), 1), "must be primed first", info = selector_name)
  }

  pbig_allowed = pbig$clone(deep = TRUE)$subset(ids = c(paste0(sel$param_classes, "."), paste0(sel$param_classes, ".1")))
  pbigvals_allowed = generate_design_random(pbig_allowed, 3)$data

  sel$prime(pbig_allowed)
  expect_equal(sel$primed_ps, pbig_allowed)
  expect_error(sel$operate(pvals_allowed, seq_len(nrow(pvals_allowed)), 1), "[mM]ust be a (permutation of set|set equal to)")

  sel$prime(p_allowed)
  expect_error(sel$operate(pbigvals_allowed, seq_len(nrow(pbigvals_allowed)), 1), "Parameter .*\\.1.*not available")

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
  test_alloweds(as.data.frame(pvals_allowed), p_allowed)
  test_alloweds(pvals_allowed[1], p_allowed)

  p_allowed_one = p_allowed$clone(deep = TRUE)$subset(p_allowed$ids()[[1]])

  test_alloweds(pvals_allowed[, 1, with = FALSE], p_allowed_one)
  test_alloweds(pvals_allowed[1, 1, with = FALSE], p_allowed_one)

  if (miesmuschel:::paradox_s3) {
    p_allowed_multicol = ParamSet$new(sapply(letters[1:3], function(x) {
      p_allowed$get_domain(p_allowed$ids()[[1]])
    }, simplify = FALSE))

  } else {
    p_allowed_multicol = ParamSet$new(lapply(letters[1:3], function(x) {
      par = p_allowed$params[[1]]$clone(deep = TRUE)
      par$id = x
      par
    }))
  }
  pvals_allowed_multicol = generate_design_random(p_allowed_multicol, 3)$data

  expect_error(sel$operate(pvals_allowed_multicol, seq_len(nrow(pvals_allowed_multicol)), 1), "Parameter 'a' not available", info = selector_name)

  test_alloweds(pvals_allowed_multicol, p_allowed_multicol)
  test_alloweds(pvals_allowed_multicol[1], p_allowed_multicol)


  expect_false(sel$endomorphism)
}

SelectorDebug = R6::R6Class("SelectorDebug",
  inherit = Selector,
  public = list(
    handler = NULL,
    initialize = function(handler, param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit")) {
      self$handler = assert_function(handler, args = c("v", "f", "n", "p"), ordered = TRUE)
      super$initialize(param_classes = param_classes, param_set = param_set, supported = supported)
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select, context) {
      self$handler(v = values, f = fitnesses, n = n_select, p = self$param_set$values)
    }
  )
)
