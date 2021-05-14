#' @title Proxy-Scalor that Scales According to its Configuration parameter
#'
#' @include Scalor.R
#'
#' @description
#' [`Scalor`] that performs the operation in its `operation` configuration parameter. This is useful, e.g., to make
#' [`SelectorBest`]'s operation fully parametrizable.
#'
#' @section Configuration Parameters:
#' * `operation` :: [`Scalor`]\cr
#'   Operation to perform. Initialized to [`ScalorOne`].
#'   This is primed when `$prime()` of `ScalorProxy` is called, and also when `$operate()` is called, to make changing
#'   the operation as part of self-adaption possible. However, if the same operation gets used inside multiple `ScalorProxy`
#'   objects, then it is recommended to `$clone(deep = TRUE)` the object before assigning them to `operation` to avoid
#'   frequent re-priming.
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @family scalor wrappers
#' @examples
#' set.seed(1)
#' sp = scl("proxy")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that ScalorOne does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sp$param_set$values$operation = scl("one")
#' sp$prime(p)
#' sp$operate(data, fitnesses)
#'
#' @export
ScalorProxy = R6Class("ScalorProxy",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorProxy` object.
    initialize = function() {
      param_set = ps(operation = p_uty(custom_check = function(x) check_r6(x, "Scalor")))
      param_set$values = list(operation = ScalorOne$new())
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
      primed_with = self$param_set$get_values(context = context)$operation
      operation = primed_with$clone(deep = TRUE)
      operation$prime(param_set)
      super$prime(param_set)
      private$.operation = operation  # only change operation once everything else succeeded
      private$.primed_with = primed_with  # keep uncloned copy of configuration parameter value for check in `.scale()`
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
    .scale = function(values, fitnesses, context) {
      operation = self$param_set$get_values(context = context)$operation
      if (is.null(private$.primed_with) || !identical(operation$primed_ps, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we try to stay safe by priming again.
        operation$prime(private$.primed_ps)
        private$.primed_with = operation$primed_ps
      }
      operation$operate(values, fitnesses)
    },
    .operation = NULL,
    .primed_with = NULL,
    deep_clone = function(name, value) {
      if (name == ".primed_with") return(NULL)  # don't even bother cloning this
      super$deep_clone(name, value)
    }
  )
)
dict_scalors$add("proxy", ScalorProxy)
