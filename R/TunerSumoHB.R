#' @include OptimizerSumoHB.R
#' @rdname OptimizerSumoHB
#' @export
TunerSumoHB = R6Class("TunerSumoHB", inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    #' @description
    #' Initialize the `TunerSumoHB` object.
    #' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)
    initialize = function(filtor = FiltorProxy$new()) {
      super$initialize(OptimizerSumoHB$new(filtor = filtor))
    }
  )
)
