#' @title Hypervolume Scalor
#'
#' @include Scalor.R
#'
#' @name dict_scalors_hypervolume
#'
#' @description
#' [`Scalor`] that returns the hypervolume of each individual, relative to `nadir` and as a contribution over `baseline`.
#' The returned scalar value is the measure of all points that have fitnesses that are
#' * greater than the respective value in `nadir` in all dimensions, and
#' * smaller than the respective value in the given point in all dimensions, and
#' * greater than all points in `baseline` in at least one dimension.
#'
#' `baseline` should probably be a [`ContextPV`][paradox::ContextPV] and generate fitness values from the [`Archive`][bbotk::Archive]
#' in the context using [`mies_get_fitnesses`].
#'
#' @section Configuration Parameters:
#' * `scale_output` :: `logical(1)`\cr
#'   Whether to scale output to lie between 0 and 1.
#' * `nadir` :: `numeric`\cr
#'   Nadir of fitness values relative to which hypervolume ution is calculated.
#' * `baseline` :: `matrix`\cr
#'   Fitness-matrix with one column per objective, giving a population over which the hypervolume improvement should be calculated.
#'
#' @templateVar id hypervolume
#' @template autoinfo_prepare_scl
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family scalors
#' @examples
#' sv = scl("hypervolume")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that ScalorHV does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = matrix(c(1, 5, 2, 3, 0, 3, 1, 0, 10, 8), ncol = 2)
#' sv$param_set$values$baseline = matrix(c(1, 1), ncol = 2)
#' sv$param_set$values$nadir = c(0, -1)
#'
#' sv$prime(p)
#'
#' sv$operate(data, fitnesses)
#' @export
ScalorHypervolume = R6Class("ScalorHypervolume",
  inherit = Scalor,
  public = list(
    #' @description
    #' Initialize the `ScalorHypervolume` object.
    initialize = function() {
      param_set = ps(
        scale_output = p_lgl(tags = "required"),
        nadir = p_vct(tags = "required"),
        baseline = p_uty(function(x) if (is.null(x)) TRUE else check_matrix(x, any.missing = FALSE, min.cols = 1))
      )
      param_set$values = list(scale_output = FALSE, nadir = 0)
      super$initialize(param_set = param_set, dict_entry = "hypervolume")
    }
  ),
  private = list(
    .scale = function(values, fitnesses, context) {
      params = self$param_set$get_values(context = context)
      # TODO
    }
  )
)
dict_scalors$add("hypervolume", ScalorHypervolume)

