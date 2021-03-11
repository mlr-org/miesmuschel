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
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5))
#' data = data.frame(x = rep(0, 5), y = rep(0, 5))
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
      super$initialize(c("ParamInt", "ParamDbl"))
    }
  ),
  private = list(
    .mutate_numeric = function(values, lowers, uppers) {
      ints = which(self$primed_ps$class == "ParamInt")
      dbls = which(self$primed_ps$class == "ParamDbl")
      mutated = numeric(length(values))
      if (length(ints)) {
        mutated[ints] = map_int(ints, .f = function(int) {
          # discrete uniform
          sample(uppers[int] - lowers[int] + 1L, size = 1L) + as.integer(lowers[int]) - 1L
        })
      }
      if (length(dbls)) {
        mutated[dbls] = stats::runif(length(dbls), min = lowers[dbls], max = uppers[dbls])
      }
      mutated
    }
  )
)
dict_mutators$add("numericunif", MutatorNumericUniform)
