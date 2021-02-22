#' @title Mutator Choosing Action Component-Wise Independently
#'
#' @include Mutator.R
#'
#' @name dict_mutators_cmpmaybe
#'
#' @description
#' [`Mutator`] that chooses which operation to perform probabilistically. The [`Mutator`] wraps two other [`Mutator`]s given during construction,
#' and both of these operators are run. The ultimate result is sampled from the results of these operations independently for each
#' individuum and component: with probability `p` (hyperparameter), the result from the [`Mutator`] given to the `mutator`
#' construction argument is used, and with probability `p - 1` the one given to `mutator_not` is used.
#'
#' @section Hyperparameters:
#' This operator has the hyperparameters of the [`Mutator`]s that it wraps: The hyperparameters of the operator given to the `mutator` construction argument
#' are prefixed with `"cmpmaybe."`, the hyperparameters of the operator given to the `mutator_not` construction argument are prefixed with `"cmpmaybe_not."`.
#'
#' Additional hyperparameters:
#' * `p` :: `numeric(1)` \cr
#'   Probability per component with which to apply the operator given to the `mutator` construction argument.
#'
#' @templateVar id cmpmaybe
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
MutatorCmpMaybe = R6Class("MutatorCmpMaybe",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorCmpMaybe` object.
    #' @param mutator ([`Mutator`])\cr
    #'   [`Mutator`] to wrap. This operator gets run with probability `p` (Hyperparameter).\cr
    #'   The constructed object gets a *clone* of this argument.
    #' @param mutator_not ([`Mutator`])\cr
    #'   Another [`Mutator`] to wrap. This operator runs when `mutator` is not chosen. By
    #'   default, this is [`MutatorNull`], i.e. no operation. With this default, the
    #'   `MutatorCmpMaybe` object applies the `mutator` operation with probability `p`, and
    #'   no operation at all otherwise.\cr
    #'   The constructed object gets a *clone* of this argument.
    initialize = function(mutator, mutator_not = MutatorNull$new()) {
      private$.wrapped = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(mutator_not, "Mutator")$clone(deep = TRUE)

      private$.wrapped$param_set$set_id = "cmpmaybe"
      private$.wrapped_not$param_set$set_id = "cmpmaybe_not"
      private$.maybe_param_set = ps(p = p_uty(custom_check = crate(function(x) check_numeric(x,
        lower = tol_bound(0, "lower"), upper = tol_bound(1, "upper"), any.missing = FALSE, min.len = 1), .parent = topenv()), tags = "required"))
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
      mutated = private$.wrapped$operate(values)
      mutated_not = private$.wrapped_not$operate(values)
      p = self$param_set$get_values()$p
      p = pmin(pmax(p, 0), 1)
      if (!length(p) %in% c(1, ncol(values))) stop("p must have either length 1, or length of input.")
      mutating = matrix(runif(nrow(values) * ncol(values)) < p, nrow = nrow(values))
      as.data.table(mapply(function(mutnot, mut, takemut) {
        mutnot[takemut] <- mut[takemut]
        mutnot
      }, mutated_not, mutated, as.data.frame(mutating), SIMPLIFY = FALSE))
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
dict_mutators$add("cmpmaybe", MutatorCmpMaybe)
