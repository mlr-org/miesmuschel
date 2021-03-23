#' @title Polynomial Numeric Mutator
#'
#' @include Mutator.R
#'
#' @name dict_mutators_numericpoly
#'
#' @description
#' Numeric components are mutated by sampling from a polynomial distribution having its mean at the
#' current value and its variance as a function of the distribution index `n`.
#' See `r cite_bib("deb1996combined")` for more details.
#'
#' Since the information loss is rather high, this should in most cases be combined with [`MutatorCmpMaybe`].
#'
#' @section Configuration Parameters:
#' *  `n` :: `double`\cr
#'  Non-negative distribution index of the polynomial distribution for each individual.
#'  Generally spoken, the higher `n`, the lower the variance of the distribution.
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
      param_set = ps(n = p_uty(custom_check = crate(function(x) {
          check_double(x, lower = 0L, any.missing = FALSE, min.len = 1L)
        }, .parent = topenv()), tags = "required")
      )
      param_set$values = list(n = 1)
      super$initialize("ParamDbl", param_set = param_set)
    }
  ),
  private = list(
    .mutate_numeric = function(values, lowers, uppers) {
      n_values = length(values)
      params = self$param_set$get_values()
      n = params$n

      if (length(n) == 1L) {
        n = rep(n, n_values)  # make the ifelse() further down work
      }
      if (length(n) != n_values) {
        stop("n must have either length 1, or length of input.")
      }
      exponent = 1 / (n + 1)


      delta = uppers - lowers
      r = stats::runif(n_values, min = 0, max = 1)
      deltaq = ifelse(r <= 0.5,
        yes = ((2 * r + (1 - 2 * r) * (1 - (values - lowers) / delta) ^ (n + 1)) ^ exponent) - 1,
        no = 1 - (2 * (1 - r) + 2 * (r - 0.5) * (1 - (uppers - values) / delta) ^ (n + 1)) ^ exponent)
      pmax(pmin(values + deltaq * delta, uppers), lowers)
    }
  )
)
dict_mutators$add("numericpoly", MutatorNumericPolynomial)
