#' @title Scalor Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing ranking operations, inheriting from [`MiesOperator`].
#'
#' A [`Scalor`] gets a table of individuals as input, along with information on the individuals' performance values
#' and returns a vector of a possible scalarization of individuals' fitness (or other qualities).
#'
#' [`Scalor`]s can be used by [`Selector`]s as a basis to select individuals by. This way it is possible to have tournament
#' selection ([`SelectorTournament`]) or elite selection ([`SelectorBest`]) based on different, configurable qualities of
#' individuals.
#'
#' Unlike most other operator types inheriting from [`MiesOperator`], the `$operate()` function has two arguments, which are passed on to `$.scale()`
#' * `values` :: `data.frame`\cr
#'   Individuals to operate on. Must pass the check of the [`Param`][paradox::ParamSet] given in the last `$prime()` call
#'   and may not have any missing components.
#' * `fitnesses` :: `numeric` | `matrix`\cr
#'   Fitnesses for each individual given in `values`. If this is a `numeric`, then its length must be equal to the number of rows in `values`. If
#'   this is a `matrix`, if number of rows must be equal to the number of rows in `values`, and it must have one column when doing single-crit optimization
#'   and one column each for each "criterion" when doing multi-crit optimization.\cr
#'   Note that fitness values are always *maximized*, both in single- and multi-criterion optimization, so objective output is multiplied with `-1` if it
#'   is tagged as `"minimize"`.
#'
#' The return value of an operation should be a numeric vector with one finite value for each entry of `values`, assigning high values to individuals in
#' some way more "desirable" than others with low values.
#'
#' @section Inheriting:
#' `Scalor` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.scale()`
#' function. The user of the object calls `$operate()`, and the arguments are passed on to private `$.scale()` after checking that
#' the operator is primed, that the `values` argument conforms to the primed domain and that other values match. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family scalors
#' @export
Scalor = R6Class("Scalor",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Mutator`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @param supported (`character`)\cr
    #'   Subset of `"single-crit"` and `"multi-crit"`, indicating wether single and / or multi-criterion optimization is supported.
    #'   Default both of them.\cr
    #'   The `$supported` field will reflect this value.
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), supported = c("single-crit", "multi-crit"), packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {
      assert_subset(supported, c("single-crit", "multi-crit"))
      assert_character(supported, any.missing = FALSE, unique = TRUE, min.len = 1)
      private$.supported = supported
      super$initialize(param_classes, param_set, endomorphism = FALSE, packages = packages, dict_entry = dict_entry, dict_shortaccess = "scl", own_param_set = own_param_set)
    }
  ),
  active = list(
    #' @field supported (`character`)\cr
    #' Optimization supported by this `Scalor`, can be `"single-crit"`, `"multi-crit"`, or both.
    supported = function(val) {
      if (!missing(val)) stop("supported is read-only.")
      private$.supported
    }
  ),
  private = list(
    .supported = NULL,
    .operate = function(values, fitnesses, context) {
      assert_data_table(values, min.rows = 1)
      if ("single-crit" %in% self$supported && test_numeric(fitnesses, any.missing = FALSE, len = nrow(values))) {
        fitnesses = matrix(fitnesses, ncol = 1)
      }
      assert_matrix(fitnesses, nrows = nrow(values),
        min.cols = 1, max.cols = if ("multi-crit" %nin% self$supported) 1,
        mode = "numeric", any.missing = FALSE
      )
      scaled = private$.scale(values, fitnesses, context)
      assert_numeric(scaled, finite = TRUE, any.missing = FALSE, len = nrow(values))
    },
    .scale = function(values, fitnesses, context) stop(".scale needs to be implemented by inheriting class.")
  )
)
