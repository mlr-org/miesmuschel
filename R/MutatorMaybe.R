#' @title Mutator Choosing Action Probabilistically
#'
#' @include Mutator.R
#'
#' @name dict_mutators_maybe
#'
#' @description
#' [`Mutator`] that chooses which operation to perform probabilistically. The [`Mutator`] wraps two other [`Mutator`]s given during construction,
#' and for each individuum, the operation to perform is sampled: with probability `p` (hyperparameter), the [`Mutator`] given to the `mutator`
#' construction argument is applied, and with probability `p - 1` the one given to `mutator_not` is applied.
#'
#' @section Hyperparameters:
#' This operator has the hyperparameters of the [`Mutator`]s that it wraps: The hyperparameters of the operator given to the `mutator` construction argument
#' are prefixed with `"maybe."`, the hyperparameters of the operator given to the `mutator_not` construction argument are prefixed with `"maybe_not."`.
#'
#' Additional hyperparameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per individual with which to apply the operator given to the `mutator` construction argument.
#'
#' @templateVar id maybe
#' @templateVar additional , <mutator> \[, <mutator_not>\]
#' @template autoinfo_prepare_mut
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are the set intersection of supported classes of `mutator` and `mutator_not`.
#'
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @export
MutatorMaybe = R6Class("MutatorMaybe",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorMaybe` object.
    #' @param mutator ([`Mutator`])\cr
    #'   [`Mutator`] to wrap. This operator gets run with probability `p` (Hyperparameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #' @param mutator_not ([`Mutator`])\cr
    #'   Another [`Mutator`] to wrap. This operator runs when `mutator` is not chosen. By
    #'   default, this is [`MutatorNull`], i.e. no operation. With this default, the
    #'   `MutatorMaybe` object applies the `mutator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    initialize = function(mutator, mutator_not = MutatorNull$new()) {
      private$.wrapped = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(mutator_not, "Mutator")$clone(deep = TRUE)

      private$.wrapped$param_set$set_id = "maybe"
      private$.wrapped_not$param_set$set_id = "maybe_not"
      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"))
      private$.maybe_param_set$values = list(p = 1)
      super$initialize(intersect(mutator$param_classes, mutator_not$param_classes),
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set))
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `mutator` and `mutator_not` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      private$.wrapped$prime(param_set)
      private$.wrapped_not$prime(param_set)
      super$prime(param_set)
      invisible(self)
    }
  ),
  private = list(
    .mutate = function(values) {
      mutating = runif(nrow(values)) < self$param_set$get_values()$p
      if (any(mutating)) {
        mutated = private$.wrapped$operate(values[mutating])
      } else {
        mutated = values[mutating]
      }
      if (any(!mutating)) {
        mutated_not = private$.wrapped_not$operate(values[!mutating])
      } else {
        mutated_not = values[!mutating]
      }
      rownumbers = seq_len(nrow(values))
      rowoder = order(c(rownumbers[mutating], rownumbers[!mutating]))
      rbind(mutated, mutated_not)[rowoder]
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_mutators$add("maybe", MutatorMaybe)
