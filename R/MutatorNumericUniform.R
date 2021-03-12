#' @title Uniform Numeric Mutator
#'
#' @include Mutator.R
#'
#' @name dict_mutators_numericunif
#'
#' @description
#' Numeric components are mutated by sampling from a (continuous or discrete) uniform distribution.
#'
#' Since the information loss is very high, this should in most cases be combined with [`MutatorCmpMaybe`].
#'
#' @section Configuration Parameters:
#' [`MutatorNumericUniform`] has no configuration parameters.
#'
#' @templateVar id numericunif
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @examples
#' set.seed(1)
#' mnu = mut("numericunif")
#' p = ps(x = p_int(0, 2), y = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 10000), y = rep(0, 10000))
#'
#' mnu$prime(p)
#' mnu$operate(data)
#' @export
MutatorNumericUniform = R6Class("MutatorNumericUniform",
  inherit = MutatorNumeric,
  public = list(
    #' @description
    #' Initialize the `MutatorNumericUniform` object.
    initialize = function() {
      super$initialize(c("ParamDbl"))
    }
  ),
  private = list(
    .mutate_numeric = function(values, lowers, uppers) {
      # discrete uniform for ParamInt is handled via corrections in
      # MutatorNumeric$private$.mutate
      stats::runif(length(values), min = lowers, max = uppers)
    }
  )
)
dict_mutators$add("numericunif", MutatorNumericUniform)
