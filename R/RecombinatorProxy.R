#' @title Proxy-Recombinator that Recombines According to its Configuration parameter
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_proxy
#'
#' @description
#' Recombinator that performs the operation in its `operation` configuration parameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s recombination operation fully parametrizable.
#'
#' @section Configuration Parameters:
#' * `operation` :: [`Recombinator`]\cr
#'   Operation to perform. Must be set by the user.
#'   This is primed when `$prime()` of `RecombinatorProxy` is called, and also when `$operate()` is called, to make changing
#'   the operation as part of self-adaption possible. However, if the same operation gets used inside multiple `RecombinatorProxy`
#'   objects, then it is recommended to `$clone(deep = TRUE)` the object before assigning them to `operation` to avoid
#'   frequent re-priming.
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @family recombinator wrappers
#' @examples
#' set.seed(1)
#' rp = rec("proxy")
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5), z = p_lgl())
#' data = data.frame(x = 1:4, y = 0:3, z = rep(TRUE, 4))
#'
#' rp$prime(p)
#' rp$operate(data)  # default operation: null
#'
#' rp$param_set$values$operation = rec("xounif", p = 0.5)
#' rp$operate(data)
#' @export
RecombinatorProxy = R6Class("RecombinatorProxy",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorProxy` object.
    #' @param n_indivs_in (`integer(1)`)\cr
    #'   Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    #'   Furthermore, the [`Recombinator`] assigned to the `operation` configuration parameter must have an `n_indivs_in` that is a divisor of this number.
    #'   Default 2.\cr
    #'   The `$n_indivs_in` field will reflect this value.
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. Must be at most `n_indivs_in`.
    #'   The ratio of `$n_indivs_in` to `$n_indivs_out` of the [`Recombinator`] assigned to the `operation` configuration parameter must be the same as
    #'  `n_indivs_in` to `n_indivs_out` of this object.
    #'   Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    initialize = function(n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      assert_int(n_indivs_in, lower = n_indivs_out, tol = 1e-100)

      param_set = ps(operation = p_uty(custom_check = crate(function(x) {
        if (test_r6(x, "Recombinator") && (n_indivs_in %% x$n_indivs_in == 0) && (n_indivs_out * x$n_indivs_in == x$n_indivs_out * n_indivs_in)) {
          return(TRUE)
        }
        sprintf("Must be a 'Recombinator' where n_indivs_in is a divisor of %s, and where n_indivs_in / n_indivs_out must be %s / %s",
          n_indivs_in, n_indivs_in, n_indivs_out)
      }, n_indivs_in, n_indivs_out)))
      # call initialization with standard options: allow everything etc.
      super$initialize(param_set = param_set, n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out, dict_entry = "proxy")
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the operator given to the `operation` configuration parameter.
    #'   Note that this modifies the `$param_set$values$operation` object.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      operation = self$param_set$get_values()$operation
      operation$prime(param_set)
      super$prime(param_set)
      private$.primed_with = operation$primed_ps  # keep uncloned copy of configuration parameter value for check in `.select()`
      ######### the following may be necessary for context dependent params

      ## primed_with = self$param_set$values$operation
      ## super$prime(param_set)
      ## if (inherits(primed_with, "MiesOperator")) {
      ##   # if primed_with is context-dependent then we need to prime during operation.
      ##   operation = primed_with$clone(deep = TRUE)
      ##   operation$prime(param_set)
      ##   private$.primed_with = primed_with  # keep uncloned copy of configuration parameter value for check in `.select()`
      ## }
      invisible(self)
    }
  ),
  private = list(
    .recombine = function(values) {
      operation = self$param_set$get_values()$operation
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
dict_recombinators$add("proxy", RecombinatorProxy)
