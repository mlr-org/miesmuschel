#' @include OptimizerSmashy.R
#' @rdname OptimizerSmashy
#' @export
TunerSmash = R6Class("TunerSmash", inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    #' @description
    #' Initialize the `TunerSmash` object.
    #' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)
    initialize = function(filtor = FiltorProxy$new(), selector = SelectorProxy$new()) {
      super$initialize(OptimizerSmashy$new(filtor = filtor, selector = selector))
    }
  )
)
