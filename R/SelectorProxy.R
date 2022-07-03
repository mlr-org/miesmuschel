#' @title Proxy-Selector that Selects According to its Configuration Parameter
#'
#' @include Selector.R
#'
#' @name dict_selectors_proxy
#'
#' @description
#' [`Selector`] that performs the operation in its `operation` configuration parameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s selection operations fully parametrizable.
#'
#' @section Configuration Parameters:
#' * `operation` :: [`Selector`]\cr
#'   Operation to perform. Initialized to [`SelectorBest`].
#'   This is primed when `$prime()` of `SelectorProxy` is called, and also when `$operate()` is called, to make changing
#'   the operation as part of self-adaption possible. However, if the same operation gets used inside multiple `SelectorProxy`
#'   objects, then it is recommended to `$clone(deep = TRUE)` the object before assigning them to `operation` to avoid
#'   frequent re-priming.
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @family selector wrappers
#' @examples
#' set.seed(1)
#' sp = sel("proxy")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorBest does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sp$param_set$values$operation = sel("random")
#' sp$prime(p)
#' sp$operate(data, fitnesses, 2)
#'
#' sp$param_set$values$operation = sel("best")
#' sp$operate(data, fitnesses, 2)
#' @export
SelectorProxy = R6Class("SelectorProxy",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorProxy` object.
    initialize = function() {
      param_set = ps(operation = p_uty(custom_check = crate(function(x) check_r6(x, "Selector"))))
      param_set$values = list(operation = SelectorBest$new())
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
        private$.primed_with = primed_with  # keep uncloned copy of configuration parameter value for check in `.select()`
      }
      invisible(self)
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select) {
      operation = self$param_set$get_values()$operation
      if (is.null(private$.primed_with) || !identical(operation$primed_ps, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we try to stay safe by priming again.
        operation$prime(private$.primed_ps)
        private$.primed_with = operation$primed_ps
      }
      operation$operate(values, fitnesses, n_select)
    },
    .primed_with = NULL,
    deep_clone = function(name, value) {
      if (name == ".primed_with") return(NULL)  # don't even bother cloning this
      super$deep_clone(name, value)
    }
  )
)
dict_selectors$add("proxy", SelectorProxy)
