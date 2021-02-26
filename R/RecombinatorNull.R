#' @title Null-Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_null
#'
#' @description
#' Null-recombinator that does not perform any operation on its input. Useful in particular with operator-wrappers such as [`RecombinatorMaybe`] or
#' [`RecombinatorCombination`].
#'
#' `n_indivs_in` and `n_indivs_out` can be set during construction, where `n_indivs_out` must be less or equal `n_indivs_in`. If it is strictly less,
#' then the operation returns only the first `n_indivs_out` individuals out of each `n_indivs_in` sized group.
#'
#' @section Hyperparameters:
#' This operator has no hyperparameters.
#'
#' @templateVar id null
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @export
RecombinatorNull = R6Class("RecombinatorNull",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize base class components of the `Recombinator`.
    #' @param n_indivs_in (`integer(1)`)\cr
    #'   Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    #'   Setting this number to a number unequal 1 is mostly useful when incorporating this operator in wrappers such as [`RecombinatorMaybe`] or
    #'   [`RecombinatorCombination`].
    #'   Default 1.\cr
    #'   The `$n_indivs_in` field will reflect this value.
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. Must be at most `n_indivs_in`. If this is less than `n_indivs_in`,
    #'   then only the first `n_indivs_out` individuals out of each `n_indivs_in` sized group are returned by an operation.
    #'   Setting this number to a number unequal 1 is mostly useful when incorporating this operator in wrappers such as [`RecombinatorMaybe`] or
    #'   [`RecombinatorCombination`].
    #'   Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    initialize = function(n_indivs_in = 1, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      assert_int(n_indivs_in, lower = n_indivs_out, tol = 1e-100)
      super$initialize(n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out)
    }
  ),
  private = list(
    .recombine = function(values) first(values, self$n_indivs_out)
  )
)
dict_recombinators$add("null", RecombinatorNull)
