#' @title Null-Mutator
#'
#' @include Mutator.R
#'
#' @name dict_mutators_null
#'
#' @description
#' Null-mutator that does not perform any operation on its input. Useful in particular with operator-wrappers such as [`MutatorMaybe`] or [`MutatorCombination`].
#'
#' @section Hyperparameters:
#' This operator has no hyperparameters.
#'
#' @templateVar id null
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @export
MutatorNull = R6Class("MutatorNull",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorNull` object.
    initialize = function() {
      # call initialization with standard options: allow everything etc.
      super$initialize()
    }
  ),
  private = list(
    .mutate = function(values) values
  )
)
dict_mutators$add("null", MutatorNull)
