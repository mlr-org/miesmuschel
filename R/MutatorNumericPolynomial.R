#' @title Polynomial Numeric Mutator
#'
#' @include Mutator.R
#'
#' @name dict_mutators_numericpoly
#'
#' @description
#' Numeric components are mutated by sampling from a polynomial distribution having its mean at the
#' current value and its variance as a function of the distribution index `n`.
#'
#' Since the information loss is rather high, this should in most cases be combined with [`MutatorCmpMaybe`].
#'
#' @section Configuration Parameters:
#' * `max_mut` :: `numeric`\cr
#'  Maximum permissible perturbance allowed for each individual.
#'  See `r cite_bib("deb1996combined")` for more details.
#'  This may either be a scalar in which case it is applied to all input components, or a vector,
#'  in which case it must have the length of the input and applies to components in order in which
#'  they appear in the priming [`ParamSet`][paradox::ParamSet]. Initialized to 1.
#' *  `n` :: `integer()`\cr
#'  Distribution index of the polynomial distribution for each individual.
#'  Generally spoken, the higher `n`, the lower the variance of the distribution.
#'  See `r cite_bib("deb1996combined")` for more details.
#'  This may either be a scalar in which case it is applied to all input components, or a vector,
#'  in which case it must have the length of the input and applies to components in order in which
#'  they appear in the priming [`ParamSet`][paradox::ParamSet]. Initialized to 1.
#'
#' @templateVar id numericpoly
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#'
#' @references
#' `r format_bib("deb1996combined")`
#'
#' @examples
#' set.seed(1)
#' mnp = mut("numericpoly")
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 1000), y = rep(0, 1000))
#'
#' mnp$prime(p)
#' mnp$operate(data)
#' @export
MutatorNumericPolynomial = R6Class("MutatorNumericPolynomial",
  inherit = MutatorNumeric,
  public = list(
    #' @description
    #' Initialize the `MutatorNumericPolynomial` object.
    initialize = function() {
      param_set = ps(
        max_mut = p_uty(custom_check = crate(function(x) {
          check_numeric(x, lower = tol_bound(0, "lower"), any.missing = FALSE, min.len = 1L)
        }, .parent = topenv()), tags = "required"),
        n = p_uty(custom_check = crate(function(x) {
          check_integerish(x, lower = 0L, any.missing = FALSE, min.len = 1L)
        }, .parent = topenv()), tags = "required")
      )
      param_set$values = list(max_mut = 1, n = 1L)
      super$initialize("ParamDbl", param_set)
    }
  ),
  private = list(
    .mutate_numeric = function(values, lowers, uppers) {
      n_values = length(values)
      params = self$param_set$get_values()

      max_mut = pmax(params$max_mut, 0)
      if (length(max_mut) %nin% c(1L, n_values)) {
        stop("max_mut must have either length 1, or length of input.")
      }

      exponent = 1 / (params$n + 1)
      if (length(exponent) == 1L) {
        exponent = rep(exponent, n_values)  # make the ifelse() further down work
      }
      if (length(exponent) != n_values) {
        stop("n must have either length 1, or length of input.")
      }

      u = stats::runif(n_values, min = 0, max = 1)
      delta = ifelse(u < 0.5,
        yes = ((2 * u) ^ exponent) - 1,
        no = 1 - ((2 * (1 - u)) ^ exponent))
      pmax(pmin(values + delta * max_mut, uppers), lowers)
    }
  )
)
dict_mutators$add("numericpoly", MutatorNumericPolynomial)
