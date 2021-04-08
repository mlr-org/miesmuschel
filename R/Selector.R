#' @title Selector Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing selection operations, inheriting from [`MiesOperator`].
#'
#' A [`Selector`] gets a table of individuals as input, along with information on the individuals' performance values and the
#' number of individuals to select, and returns a vector of integers indicating which individuals were selected.
#'
#' Selection operations are performed in ES algorithms to facilitate concentration towards individuals that perform well with regard to the
#' fitness measure.
#'
#' Fitness values are always *maximized*, both in single- and multi-criterion optimization.
#'
#' Unlike most other operator types inheriting from [`MiesOperator`], the `$operate()` function has three arguments, which are passed on to `$.select()`
#' * `values` :: `data.frame`\cr
#'   Individuals to operate on. Must pass the check of the [`Param`][paradox::ParamSet] given in the last `$prime()` call
#'   and may not have any missing components.
#' * `fitnesses` :: `numeric` | `matrix`\cr
#'   Fitnesses for each individual given in `values`. If this is a `numeric`, then its length must be equal to the number of rows in `values`. If
#'   this is a `matrix`, if number of rows must be equal to the number of rows in `values`, and it must have one column when doing single-crit optimization
#'   and one column each for each  "criterion" when doing multi-crit optimization.
#' * `n_select` :: `integer(1)`\cr
#'   Number of individuals to select. Some `Selector`s select individuals with replacement, for which this value may be greater than the number of
#'   rows in `values`.
#'
#' The return value for an operation will be a numeric vector of integer values of ength `n_select` indexing the individuals that were selected. Some `Selector`s
#' select individuals with replacement, for which the return value may contain indices more than once.
#'
#' @section Inheriting:
#' `Selector` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.select()`
#' function. The user of the object calls `$operate()`, and the arguments are passed on to private `$.select()` after checking that
#' the operator is primed, that the `values` argument conforms to the primed domain and that other values match. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family selectors
#' @export
Selector = R6Class("Selector",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Selector`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @param supported (`character`)\cr
    #'   Subset of `"single-crit"` and `"multi-crit"`, indicating wether single and / or multi-criterion optimization is supported.
    #'   Default both of them.\cr
    #'   The `$supported` field will reflect this value.
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit")) {
      assert_subset(supported, c("single-crit", "multi-crit"))
      assert_character(supported, any.missing = FALSE, unique = TRUE, min.len = 1)
      private$.supported = supported
      super$initialize(param_classes, param_set, endomorphism = FALSE)
    }
  ),
  active = list(
    #' @field supported (`character`)\cr
    #' Optimization supported by this `Selector`, can be `"single-crit"`, `"multi-crit"`, or both.
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
