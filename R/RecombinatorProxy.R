#' @title Proxy-Recombinator that Recombines According to its Hyperparameter
#'
#' @include Recombinator.R
#'
#' @description
#' Recombinator that performs the operation in its `operation` hyperparameter. This is useful, e.g., to make
#' [`OptimizerMies`]'s recombination operation fully parametrizable.
#'
#' Changes in the `operation` hyperparameter are only realized whenever `$prime()` is called, so `$prime()`
#' must be called every time when `operation` is changed, *even if* the new hyperparameter value is already primed.
#'
#' @section Hyperparameters:
#' * `operation` :: [`Recombinator`]\cr
#'   Operation to perform. Initialized to [`RecombinatorNull`] with appropriate `n_indivs_in` and `n_indivs_out` values.
#'
#' @templateVar id proxy
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @family recombinator wrappers
#' @export
RecombinatorProxy = R6Class("RecombinatorProxy",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorProxy` object.
    #' @param n_indivs_in (`integer(1)`)\cr
    #'   Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    #'   Furthermore, the [`Recombinator`] assigned to the `operation` hyperparameter must have an `n_indivs_in` that is a divisor of this number.
    #'   Default 2.\cr
    #'   The `$n_indivs_in` field will reflect this value.
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. Must be at most `n_indivs_in`.
    #'   The ratio of `$n_indivs_in` to `$n_indivs_out` of the [`Recombinator`] assigned to the `operation` hyperparameter must be the same as
    #'  `n_indivs_in` to `n_indivs_out` of this object.
    #'   Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    initialize = function(n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      assert_int(n_indivs_in, lower = n_indivs_out, tol = 1e-100)

      param_set = ps(operation = p_uty(custom_check = crate(.parent = topenv(), function(x) {
        if (test_r6(x, "Recombinator") && (n_indivs_in %% x$n_indivs_in == 0) && (n_indivs_out * x$n_indivs_in == x$n_indivs_out * n_indivs_in)) {
          return(TRUE)
        }
        sprintf("Must be a 'Recombinator' where n_indivs_in is a divisor of %s, and where n_indivs_in / n_indivs_out must be %s / %s",
          n_indivs_in, n_indivs_in, n_indivs_out)
      }, n_indivs_in, n_indivs_out)))
      param_set$values = list(operation = RecombinatorNull$new(n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out))
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
      private$.primed_with = primed_with  # keep uncloned copy of hyperparameter value for check in `.recombine()`
      invisible(self)
    }
  ),
  private = list(
    .recombine = function(values) {
      if (!is.null(private$.primed_with) && !identical(self$param_set$get_values()$operation, private$.primed_with)) {
        # Unfortunately, when we clone, we can't keep track of self$param_set$values$operation.
        # In that case we ignore and hope the user is doing the right thing. After all, this message
        # is only to be informative.
        stop("RecombinatorProxy$prime() must be called again when the 'operation' hyperparameter changes.")
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
dict_recombinators$add("proxy", RecombinatorProxy)
