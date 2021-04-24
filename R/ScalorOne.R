#' @title Single Dimension Scalor
#'
#' @include Scalor.R
#'
#' @name dict_scalors_one
#'
#' @description
#' [`Scalor`] that returns a the fitness value of a single objective dimension as scale.
#'
#' @section Configuration Parameters:
#' * `objective` :: `integer(1)`\cr
#'   objective to return as scale, ranges from 1 (the default, first objective) to the number of objectives
#'   of the function being optimized.
#'
#' @templateVar id one
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' so = scl("one")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that ScalorOne does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2)
#'
#' so$prime(p)
#'
#' so$operate(data, fitnesses)
#'
#' so$param_set$values$objective = 2
#'
#' so$operate(data, fitnesses)
#' @export
ScalorOne = R6Class("ScalorOne",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorOne` object.
    initialize = function() {
      param_set = ps(objective = p_int(lower = 1, tags = "required"))
      param_set$values$objective = 1
      super$initialize(param_set = param_set, dict_entry = "one")
    }
  ),
  private = list(
    .scale = function(values, fitnesses) {
      pvals = self$param_set$get_values()
      fitnesses[, pvals$objective]
    }
  )
)
dict_scalors$add("one", ScalorOne)
