#' @title Proxy-Mutator that Mutates According to its Configuration parameter
#'
#' @include Mutator.R
#'
#' @name dict_mutators_proxy
#'
#' @description
#' Mutator that performs the operation in its `operation` configuration parameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s mutation operation fully parametrizable.
#'
#' @section Configuration Parameters:
#' * `operation` :: [`Mutator`]\cr
#'   Operation to perform. Initialized to [`MutatorNull`].
#'   This is primed when `$prime()` of `MutatorProxy` is called, and also when `$operate()` is called, to make changing
#'   the operation as part of self-adaption possible. However, if the same operation gets used inside multiple `MutatorProxy`
#'   objects, then it is recommended to `$clone(deep = TRUE)` the object before assigning them to `operation` to avoid
#'   frequent re-priming.
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
      super$initialize(param_set = param_set, dict_entry = "proxy")
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` configuration parameter.
    #'   Note that this modifies the `$param_set$values$operation` object.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      primed_with = self$param_set$values$operation
      super$prime(param_set)
      if (inherits(primed_with, "MiesOperator")) {
        # if primed_with is context-dependent then we need to prime during operation.
        operation = primed_with$clone(deep = TRUE)
        operation$prime(param_set)
        private$.operation = operation  # only change operation once everything else succeeded
        private$.primed_with = primed_with  # keep uncloned copy of configuration parameter value for check in `.select()`
      }
      invisible(self)
    }
  ),
  active = list(
    #' @field packages (`character`)\cr
    #' Packages needed for the operator. Retrieved from the `operation` configuration parameter. Read-only.
    packages = function(val) {
      if (!missing(val)) stop("packages is read-only.")
      self$param_set$values$operation$packages
    }
  ),
  private = list(
    .mutate = function(values, context) {
      operation = self$param_set$get_values(context = context)$operation
      if (is.null(private$.primed_with) || !identical(operation$primed_ps, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we try to stay safe by priming again.
        operation$prime(private$.primed_ps)
        private$.primed_with = operation$primed_ps
      }
      operation$operate(values)
    },
    .primed_with = NULL,
    deep_clone = function(name, value) {
      if (name == ".primed_with") return(NULL)  # don't even bother cloning this
      super$deep_clone(name, value)
    }
  )
)
dict_mutators$add("proxy", MutatorProxy)
