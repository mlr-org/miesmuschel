
# ftr: filtor to check
# filtor_name: name to print in info
# is_primed: whether the filtor is already primed. If so, the expectation that unprimed filtor gives an error will not be checked.
expect_filtor = function(ftr, filtor_name, is_primed = FALSE) {
  expect_r6(ftr, "Filtor", info = filtor_name)

  p = ps(ParamLgl. = p_lgl(), ParamDbl. = p_dbl(0, 1), ParamInt. = p_int(0, 1), ParamFct. = p_fct(c("a", "b", "c")))
  pbig = p$clone(deep = TRUE)
  lapply(p$params, function(x) { x = x$clone() ; x$id = paste0(x$id, "1") ; pbig$add(x) })

  p_forbidden = p$clone(deep = TRUE)$subset(ids = setdiff(p$ids(), paste0(ftr$param_classes, ".")))
  if (length(p_forbidden$ids())) {
    expect_error(ftr$prime(p_forbidden), "Must be a subset of", info = filtor_name)
  }



  p_allowed = p$clone(deep = TRUE)$subset(ids = paste0(ftr$param_classes, "."))
  pvals_allowed = generate_design_random(p_allowed, 3)$data

  if (!is_primed) {
    expect_error(ftr$operate(pvals_allowed, pvals_allowed, seq_len(nrow(pvals_allowed)), 1), "must be primed first", info = filtor_name)
  }

  pbig_allowed = pbig$clone(deep = TRUE)$subset(ids = c(paste0(ftr$param_classes, "."), paste0(ftr$param_classes, ".1")))

  ftr$prime(pbig_allowed)

  expect_int({datasize = ftr$needed_input(4)}, lower = 1, tol = 1e-100)

  pvals_allowed = generate_design_random(p_allowed, datasize)$data
  pbigvals_allowed = generate_design_random(pbig_allowed, datasize)$data


  expect_error(ftr$operate(pvals_allowed, pvals_allowed, seq_len(nrow(pvals_allowed)), 1), "Must be a permutation of set")

  ftr$prime(p_allowed)
  expect_error(ftr$operate(pvals_allowed, pbigvals_allowed, seq_len(nrow(pbigvals_allowed)), 1), "Parameter .*\\.1.*not available")
  expect_error(ftr$operate(pbigvals_allowed, pvals_allowed, seq_len(nrow(pvals_allowed)), 1), "Parameter .*\\.1.*not available")

  expect_error(ftr$operate(pvals_allowed, pvals_allowed, seq_len(nrow(pvals_allowed) + 1), 1), "fitnesses.*Must have length .*but has length ")
  expect_error(ftr$operate(pvals_allowed, pvals_allowed, seq_len(nrow(pvals_allowed)), nrow(pvals_allowed) + 1), "n_filter.*Element 1 is not <= ")

  expect_error(ftr$operate(first(pvals_allowed, datasize - 1), pvals_allowed, seq_len(nrow(pvals_allowed)), 4), "Needs at least .* individuals to select .* individuals, but got .*")

  test_alloweds = function(data, pp) {
    ftr$prime(pp)
    neededs = sapply(1:4, function(i) ftr$needed_input(i))
    if ("single-crit" %in% ftr$supported) {
      for (i in 1:4) {
        if (nrow(data) < neededs[[i]]) break
        expect_integerish(ftr$operate(data, data, seq_len(nrow(data)), i),
          lower = 1, upper = nrow(data), any.missing = FALSE, tol = 1e-100, len = i)
      }
    }
    if ("multi-crit" %in% ftr$supported) {
      for (i in 1:4) {
        if (nrow(data) < neededs[[i]]) break
        expect_integerish(ftr$operate(data, data, matrix(seq_len(nrow(data) * 3), ncol = 3), i),
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

  p_allowed_multicol = ParamSet$new(lapply(letters[1:3], function(x) {
    par = p_allowed$params[[1]]$clone(deep = TRUE)
    par$id = x
    par
  }))
  pvals_allowed_multicol = generate_design_random(p_allowed_multicol, datasize)$data

  ftr$prime(p_allowed)
  expect_error(ftr$operate(pvals_allowed, pvals_allowed_multicol, nrow(pvals_allowed_multicol), 1), "Parameter 'a' not available", info = filtor_name)
  expect_error(ftr$operate(pvals_allowed_multicol, pvals_allowed, nrow(pvals_allowed), 1), "Parameter 'a' not available", info = filtor_name)

  test_alloweds(pvals_allowed_multicol, p_allowed_multicol)
  test_alloweds(pvals_allowed_multicol[1], p_allowed_multicol)


  expect_false(ftr$endomorphism)
}

FiltorDebug = R6::R6Class("FiltorDebug",
  inherit = Filtor,
  public = list(
    handler = NULL,
    ni = NULL,
    initialize = function(handler, ni, param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit")) {
      self$handler = assert_function(handler, args = c("v", "k", "f", "n", "p"), ordered = TRUE)
      self$ni = assert_function(ni, args = c("o"), ordered = TRUE)
      super$initialize(param_classes = param_classes, param_set = param_set, supported = supported)
    }
  ),
  private = list(
    .filter = function(values, known_values, fitnesses, n_select) {
      self$handler(v = values, k = known_values, f = fitnesses, n = n_select, p = self$param_set$values)
    },
    .needed_input = function(output_size) self$ni(output_size)
  )
)
