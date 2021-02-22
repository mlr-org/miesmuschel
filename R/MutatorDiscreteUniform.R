#' @title Uniform Discrete Mutator
#'
#' @include Mutator.R
#'
#' @name dict_mutators_unif
#'
#' @description
#' Discrete components are mutated by sampling from a uniform distribution, either from all possible
#' values of each component, or from all values except the original value.
#'
#' Since the information loss is very high, this should in most cases be combined with [`MutatorCmpMaybe`].
#'
#' @section Hyperparameters:
#' * `can_mutate_to_same` :: `logical(1)`\cr
#'   Whether to sample from entire range of each parameter (`TRUE`) or from all values except the
#'   current value (`FALSE`). Initialized to `TRUE`.
#'
#' @templateVar id unif
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @export
MutatorDiscreteUniform = R6Class("MutatorDiscreteUniform",
  inherit = MutatorDiscrete,
  public = list(
    #' @description
    #' Initialize the `MutatorDiscreteUniform` object.
    initialize = function() {
      param_set = ps(can_mutate_to_same = p_lgl(tags = "required"))
      param_set$values = list(can_mutate_to_same = TRUE)
      super$initialize(c("ParamLgl", "ParamFct"), param_set)
    }
  ),
  private = list(
    .mutate_discrete = function(values, levels) {
      params = self$param_set$get_values()
      unlist(pmap(list(values, levels), function(v, l) {
        if (!params$can_mutate_to_same) l = setdiff(l, v)
        sample(l, 1)
      }))
    }
  )
)
dict_mutators$add("unif", MutatorDiscreteUniform)
