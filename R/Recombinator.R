#' @title Recombinator Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing recombination operations, inheriting from [`MiesOperator`].
#'
#' Recombinators get a table of individuals as input and return a table of modified individuals as output. Individuals are acted on by
#' groups: every `$n_indivs_out` lines of output corresponds to a group of `$n_indivs_in` lines of input, and presence or absence
#' of other input groups does not affect the result.
#'
#' Recombination operations are performed in ES algorithms to facilitate exploration of the search space that combine partial
#' solutions.
#'
#' @section Inheriting:
#' `Recombinator` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.recombine()`
#' function. The user of the object calls `$operate()`, which calls `$.recombine()` for each `$n_indivs_in` sized group of individuals after checking that
#' the operator is primed, that the `values` argument conforms to the primed domain. `$.recombine()` should then return a table of
#' `$n_indivs_out` individuals for each call. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family recombinators
#' @export
Recombinator = R6Class("Recombinator",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Recombinator`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @param n_indivs_in (`integer(1)`)\cr
    #'   Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    #'   Default 2.\cr
    #'   The `$n_indivs_in` field will reflect this value.
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. The number of results from the recombinator will be
    #'   `nrow(values) / n_indivs_in * n_indivs_out`. Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_in, lower = 1, tol = 1e-100)
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      private$.n_indivs_in = n_indivs_in
      private$.n_indivs_out = n_indivs_out
      super$initialize(param_classes, param_set)
    }
  ),
  active = list(
    #' @field n_indivs_in (`integer(1)`)\cr
    #' Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    n_indivs_in = function(val) {
      if (!missing(val)) stop("n_indivs_in is read-only.")
      private$.n_indivs_in
    },
    #' @field n_indivs_out (`integer(1)`)\cr
    #' Number of individuals produced for each group of `$n_indivs_in` individuals.
    n_indivs_out = function(val) {
      if (!missing(val)) stop("n_indivs_out is read-only.")
      private$.n_indivs_out
    }
  ),
  private = list(
    .n_indivs_in = NULL,
    .n_indivs_out = NULL,
    .recombine = function(values) stop(".recombine needs to be implemented by inheriting class."),
    .operate = function(values) {
      assert_true(nrow(values) %% self$n_indivs_in == 0)
      rbindlist(
        lapply(split(values, rep(seq_len(nrow(values) / self$n_indivs_in), each = self$n_indivs_in)), function(vs) {
          vs = private$.recombine(vs)
          assert_data_table(vs, nrows = self$n_indivs_out)
        }), use.names = TRUE)
    }
  )
)
