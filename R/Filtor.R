#' @title Filtor Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing filter operations, inheriting from [`MiesOperator`].
#'
#' A [`Filtor`] gets (1) a table of individuals that are to be filtered, as well as a table of individuals that were already evaluated,
#' along with information on the latter individuals' performance values. Furthermore, the
#' number of individuals to return is given. The `Filtor` returns a vector of unique integers indicating which individuals were selected.
#'
#' Filter operations are performed in ES algorithms to facilitate concentration towards individuals that perform well with regard to the
#' fitness measure, without evaluating the fitness measure, for example through a surrogate model.
#'
#' Fitness values are always *maximized*, both in single- and multi-criterion optimization.
#'
#' Unlike most other operator types inheriting from [`MiesOperator`], the `$operate()` function has four arguments, which are passed on to `$.filter()`
#' * `values` :: `data.frame`\cr
#'   Individuals to filter. Must pass the check of the [`Param`][paradox::ParamSet] given in the last `$prime()` call
#'   and may not have any missing components.
#' * `known_values` :: `data.frame`\cr
#'   Individuals to use for filtering. Must pass the check of the [`Param`][paradox::ParamSet] given in the last `$prime()` call
#'   and may not have any missing components. Note that `known_values` may be empty.
#' * `fitnesses` :: `numeric` | `matrix`\cr
#'   Fitnesses for each individual given in `old_values`. If this is a `numeric`, then its length must be equal to the number of rows in `values`. If
#'   this is a `matrix`, if number of rows must be equal to the number of rows in `values`, and it must have one column when doing single-crit optimization
#'   and one column each for each  "criterion" when doing multi-crit optimization.
#' * `n_filter` :: `integer(1)`\cr
#'   Number of individuals to select. Some `Filtor`s select individuals with replacement, for which this value may be greater than the number of
#'   rows in `values`.
#'
#' The return value for an operation will be a numeric vector of integer values of ength `n_filter` indexing the individuals that were selected. `Filtor`
#' must always return unique integers, i.e. select every individual at most once.
#'
#' @section Inheriting:
#' `Filtor` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.filter()`
#' function. The user of the object calls `$operate()`, and the arguments are passed on to private `$.filter()` after checking that
#' the operator is primed, that the `values` and `known_values` arguments conforms to the primed domain and that other values match.
#'
#' The `private$.needed_input()` function should also be overloaded. It is a function that gets a single input, `output_size`, a positive integer indicating
#' the number of individuals that the caller desires. The function should calculate the number of `values` that are required to
#' filter down to `output_size`, given the current configuraiton parameter settings. The needed input should always be at least `output_size`.
#'
#' Typically, the `$initialize()` function should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family filtors
#' @export
Filtor = R6Class("Filtor",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Filtor`.
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
    },
    needed_input = function(output_size) {
      assert_int(output_size, tol = 1e-100, lower = 1)
      assert_int(private$.needed_input(output_size), tol = 1e-100, lower = output_size)
    }
  ),
  active = list(
    #' @field supported (`character`)\cr
    #' Optimization supported by this `Filtor`, can be `"single-crit"`, `"multi-crit"`, or both.
    supported = function(val) {
      if (!missing(val)) stop("supported is read-only.")
      private$.supported
    }
  ),
  private = list(
    .supported = NULL,
    .operate = function(values, known_values, fitnesses, n_filter) {
      assert_data_table(values, min.rows = 1)

      private$.primed_ps$assert_dt(known_values)
      assert_names(colnames(known_values), permutation.of = private$.primed_ps$ids())
      if (!is.data.table(known_values)) {
        # don't change input by reference
        known_values = as.data.table(known_values)
      }

      if ("single-crit" %in% self$supported && test_numeric(fitnesses, any.missing = FALSE, len = nrow(known_values))) {
        fitnesses = matrix(fitnesses, ncol = 1)
      }

      assert_matrix(fitnesses, nrows = nrow(known_values),
        min.cols = 1, max.cols = if ("multi-crit" %nin% self$supported) 1,
        mode = "numeric", any.missing = FALSE
      )

      assert_int(n_filter, lower = 0, upper = nrow(known_values), tol = 1e-100)

      needed_input = self$needed_input(n_filter)
      if (nrow(values) < needed_input) stopf("Needs at least %s individuals to select %s individuals, but got %s.", needed_input, n_filter, nrow(values))

      selected = private$.filter(values, known_values, fitnesses, n_filter)

      assert_integerish(selected, tol = 1e-100, lower = 1, upper = nrow(values), any.missing = FALSE, len = n_filter, unique = TRUE)
    },
    .filter = function(values, known_values, fitnesses, n_filter) stop(".filter needs to be implemented by inheriting class."),
    .needed_input = function(output_size) stop(".needed_input needs to be implemented by inheriting class.")
  )
)
