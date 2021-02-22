#' @title Proxy-Mutator that Mutates According to its Hyperparameter
#'
#' @include Mutator.R
#'
#' @description
#' Mutator that performs the operation in its `operation` hyperparameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s mutation operation fully parametrizable.
#'
#' Changes in the `operation` hyperparameter are only realized whenever `$prime()` is called, so `$prime()`
#' must be called every time when `operation` is changed, *even if* the new hyperparameter value is already primed.
#'
#' @section Hyperparameters:
#' * `operation` :: [`Mutator`]\cr
#'   Operation to perform. Initialized to [`MutatorNull`].
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @family mutator wrappers
#' @export
MutatorProxy = R6Class("MutatorProxy",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorProxy` object.
    initialize = function() {
      param_set = ps(operation = p_uty(custom_check = function(x) check_r6(x, "Mutator"), tags = "required"))
      param_set$values = list(operation = MutatorNull$new())
      # call initialization with standard options: allow everything etc.
      super$initialize(param_set = param_set)
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` hyperparameter.
    #' This must be called whenever the `operation` hyperparameter changes, *even if* the hyperparameter is already primed.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      primed_with = self$param_set$get_values()$operation
      operation = primed_with$clone(deep = TRUE)
      operation$prime(param_set)
      super$prime(param_set)
      private$.operation = operation  # only change operation once everything else succeeded
      private$.primed_with = primed_with  # keep uncloned copy of hyperparameter value for check in `.mutate()`
      invisible(self)
    }
  ),
  private = list(
    .mutate = function(values) {
      if (!is.null(private$.primed_with) && !identical(self$param_set$get_values()$operation, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we ignore and hope the user is doing the right thing. After all, this message
        # is only to be informative.
        stop("MutatorProxy$prime() must be called again when the 'operation' hyperparameter changes.")
      }
      private$.operation$operate(values)
    },
    .operation = NULL,
    .primed_with = NULL,
    deep_clone = function(name, value) {
      if (name == ".primed_with") return(NULL)
      super$deep_clone(name, value)
    }
  )
)
dict_mutators$add("proxy", MutatorProxy)
