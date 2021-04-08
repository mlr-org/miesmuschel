#' @include OptimizerSumoHB.R
#' @rdname OptimizerSumoHB
#' @export
TunerSumoHB = R6Class("TunerSumoHB", inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    #' @description
    #' Initialize the `TunerSumoHB` object.
    #' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)
    initialize = function(surrogate_learner = NULL) {
      super$initialize(OptimizerSumoHB$new(surrogate_learner = surrogate_learner))
    }
  )
)
