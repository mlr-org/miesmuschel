
# rec: recombinator to check
# recombinator_name: name to print in info
# is_primed: whether the recombinator is already primed. If so, the expectation that unprimed recombinator gives an error will not be checked.
expect_recombinator = function(rec, recombinator_name, is_primed = FALSE) {

  expect_r6(rec, "Recombinator", info = recombinator_name)

  p = ps(ParamLgl. = p_lgl(), ParamDbl. = p_dbl(0, 1), ParamInt. = p_int(0, 1), ParamFct. = p_fct(c("a", "b", "c")))
  pbig = p$clone(deep = TRUE)
  lapply(p$params, function(x) { x = x$clone() ; x$id = paste0(x$id, "1") ; pbig$add(x) })

  if (!is_primed) {
    expect_error(rec$operate(pvals), "must be primed first", info = recombinator_name)
  }

  p_forbidden = p$clone(deep = TRUE)$subset(ids = setdiff(p$ids(), paste0(rec$param_classes, ".")))
  if (length(p_forbidden$ids())) {
    expect_error(rec$prime(p_forbidden), "Must be a subset of", info = recombinator_name)
  }

  p_allowed = p$clone(deep = TRUE)$subset(ids = paste0(rec$param_classes, "."))
  pvals_allowed = generate_design_random(p_allowed, 3 * rec$n_indivs_in)$data

  pbig_allowed = pbig$clone(deep = TRUE)$subset(ids = c(paste0(rec$param_classes, "."), paste0(rec$param_classes, ".1")))
  pbigvals_allowed = generate_design_random(pbig_allowed, 3 * rec$n_indivs_in)$data

  rec$prime(pbig_allowed)
  expect_error(rec$operate(pvals_allowed), "Must be a permutation of set")

  rec$prime(p_allowed)
  expect_error(rec$operate(pbigvals_allowed), "Parameter .*\\.1.*not available")

  recombined = rec$operate(as.data.frame(pvals_allowed))
  expect_data_frame(recombined, nrows = nrow(pvals_allowed) / rec$n_indivs_in * rec$n_indivs_out,
    ncols = ncol(pvals_allowed), any.missing = FALSE, info = recombinator_name)
  expect_true(identical(class(recombined), "data.frame"))  # No data.table output when input is data.frame


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

RecombinatorDebug = R6::R6Class("RecombinatorDebug",
  inherit = Recombinator,
  public = list(
    handler = NULL,
    initialize = function(handler, param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      self$handler = assert_function(handler, args = c("n", "v", "p"), ordered = TRUE)
      super$initialize(param_classes = param_classes, param_set = param_set, n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out)
    }
  ),
  private = list(
    .recombine = function(values) {
      as.data.table(sapply(names(values), function(n) self$handler(n, values[[n]], self$param_set$values), simplify = FALSE))
    }
  )
)
