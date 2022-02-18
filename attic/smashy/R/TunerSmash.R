#' @include OptimizerSmashy.R
#' @rdname OptimizerSmashy
#' @export
TunerSmashy = R6Class("TunerSmashy", inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    #' @description
    #' Initialize the `TunerSmashy` object.
    #' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)
    initialize = function(filtor = FiltorProxy$new(), selector = SelectorProxy$new()) {
      super$initialize(OptimizerSmashy$new(filtor = filtor, selector = selector))
    }
  )
)
