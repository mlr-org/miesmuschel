

# TODO: dictionary & quick access function


#' @title Selector
#' @include MiesOperator
Selector = R6Class("Selector",
  inherit = MiesOperator,
  public = list(
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit")) {  # TODO: how to do this?
      assert_subset(supported, c("single-crit", "multi-crit"))
      assert_character(supported, any.missing = FALSE, unique = TRUE, min.len = 1)
      private$.supported = supported
      super$initialize(param_classes, param_set)
    },
    select = function(values, param_set, fitnesses, n_select) {
      assert_subset(param_set$class, self$param_classes)
      assert_list(values, min.len = 1)
      assert_numeric(fitnesses, length = length(values))
      assert_int(n_select, lower = 0, tol = 1e-100)
      map(values, param_set$assert)  # TODO: this may be overkill, should only happen for debugging
      selected = private$.select(values, param_set, fitnesses, n_select)
      assert_integerish(selected, tol = 1e-100, lower = 1, upper = length(values), any.missing = FALSE, len = n_select)
      selected
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
    .select = function(values, param_set, fitnesses, n_select) stop(".select needs to be implemented by inheriting class.")
  )
)

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
    .select = function(values, param_set, fitnesses, n_select) {
      params = self$param_set$get_values()
      sample(length(values), n_select, replace = params$replace)
    }
  )
)

SelectorBest = R6Class("SelectorBest",
  inherit = Selector,
  public = list(
    initialize = function() {
      super$initialize(supported = "single-crit")
    }
  ),
  private = list(
    .select = function(values, param_set, fitnesses, n_select) {
      order(fitnesses, decreasing = TRUE)[(seq_len(n_select) - 1) %% length(values) + 1]
    }
  )
)
