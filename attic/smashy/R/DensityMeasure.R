
#' @title Density Estimation Measure
#'
#' @description
#' This measure implements [`mlr3::Measure`] for density estimation.
#'
#' * `task_type` is set to `"density"`.
#' * `predict_type` must always be `"prob"`.
#'
#'
#' @family density estimation classes
#' @export
MeasureDensity = R6Class("MeasureDensity", inherit = mlr3::Measure, cloneable = FALSE,
  public = list(
    #' @description
    #' Initialize the `MeasureDensity` object.
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(0), predict_type = "prob", task_properties = character(0),
        packages = character(0), man = NA_character_) {
      super$initialize(id, task_type = "density", range = range, minimize = minimize, aggregator = aggregator, properties = properties,
        predict_type = predict_type, task_properties = task_properties, packages = packages, man = man)
    }
  )
)

#' @title Density Logloss
#'
#' @name mlr_measures_density.logloss
#'
#' @description
#' Logloss measure: Negative log likelihood of all predictions.
#'
#' @family density estimation classes
#' @export
MeasureDensityLogloss = R6Class("MeasureDensityLogloss", inherit = MeasureDensity,
  public = list(
    initialize = function() {
      super$initialize(id = "density.logloss", range = c(-Inf, Inf), minimize = TRUE, man = "miesmuschel::mlr_measures_density.logloss")
    }
  ),
  private = list(
    .score = function(prediction, ...) {
      mean(-prediction$logprob)
    }
  )
)
