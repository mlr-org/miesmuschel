#' @title Proxy-Mutator that Mutates According to its Configuration parameter
#'
#' @include Mutator.R
#'
#' @description
#' Mutator that performs the operation in its `operation` configuration parameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s mutation operation fully parametrizable.
#'
#' Changes in the `operation` configuration parameter are only realized whenever `$prime()` is called, so `$prime()`
#' must be called every time when `operation` is changed, *even if* the new configuration parameter value is already primed.
#'
#' @section Configuration Parameters:
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
#' @examples
#' set.seed(1)
#' mp = mut("proxy")
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 5), y = rep(0, 5))
#'
#' mp$prime(p)
#' mp$operate(data)  # default operation: null
#'
#' mp$param_set$values$operation = mut("gauss", sdev = 5)
#' mp$prime(p)
#' mp$operate(data)
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
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` configuration parameter.
    #' This must be called whenever the `operation` configuration parameter changes, *even if* the configuration parameter is already primed.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      primed_with = self$param_set$get_values()$operation
      operation = primed_with$clone(deep = TRUE)
      operation$prime(param_set)
      super$prime(param_set)
      private$.operation = operation  # only change operation once everything else succeeded
      private$.primed_with = primed_with  # keep uncloned copy of configuration parameter value for check in `.mutate()`
      invisible(self)
    }
  ),
  private = list(
    .mutate = function(values) {
      if (!is.null(private$.primed_with) && !identical(self$param_set$get_values()$operation, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we ignore and hope the user is doing the right thing. After all, this message
        # is only to be informative.
        stop("MutatorProxy$prime() must be called again when the 'operation' configuration parameter changes.")
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
