
#' @title Selector
#' @include MiesOperator.R
#' @include dictionaries.R
#' @export
Selector = R6Class("Selector",
  inherit = MiesOperator,
  public = list(
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit")) {
      assert_subset(supported, c("single-crit", "multi-crit"))
      assert_character(supported, any.missing = FALSE, unique = TRUE, min.len = 1)
      private$.supported = supported
      super$initialize(param_classes, param_set, endomorphism = FALSE)
    }
  ),
  active = list(
    supported = function(val) {
      if (!missing(val)) stop("supported is read-only.")
      private$.supported
    }
  ),
  private = list(
    .supported = NULL,
    .operate = function(values, fitnesses, n_select) {
      assert_data_table(values, min.rows = 1)
      if ("single-crit" %in% self$supported && test_numeric(fitnesses, any.missing = FALSE, len = nrow(values))) {
        fitnesses = matrix(fitnesses, ncol = 1)
      }
      assert_matrix(fitnesses, nrows = nrow(values),
        min.cols = 1, max.cols = if ("multi-crit" %nin% self$supported) 1,
        mode = "numeric", any.missing = FALSE
      )

      assert_int(n_select, lower = 0, tol = 1e-100)
      selected = private$.select(values, fitnesses, n_select)
      assert_integerish(selected, tol = 1e-100, lower = 1, upper = nrow(values), any.missing = FALSE, len = n_select)
    },
    .select = function(values, fitnesses, n_select) stop(".select needs to be implemented by inheriting class.")
  )
)

#' @export
SelectorRandom = R6Class("SelectorRandom",
  inherit = Selector,
  public = list(
    initialize = function() {
      param_set = ps(replace = p_lgl(tags = "required"))
      param_set$values = list(replace = FALSE)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set)
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      params = self$param_set$get_values()
      sample(nrow(values), n_select, replace = params$replace)
    }
  )
)
mlr_selectors$add("random", SelectorRandom)

#' @export
SelectorBest = R6Class("SelectorBest",
  inherit = Selector,
  public = list(
    initialize = function() {
      super$initialize(supported = "single-crit")
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      order(fitnesses, decreasing = TRUE)[(seq_len(n_select) - 1) %% nrow(values) + 1]
    }
  )
)
mlr_selectors$add("best", SelectorBest)
