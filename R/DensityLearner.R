#' @title Density Estimation Learner
#'
#' @description
#' This learner implements the [`mlr3::Learner`] for density estimation [`Task`][mlr3::Task]s.
#' * `task_type` is set to `"density"`.
#' * `Creates [`Prediction`][mlr3::Prediction]s of class [`PredictionDensity`].
#' * `predict_types` always has the value `"prob"`.
#'
#' @family density estimation classes
#' @export
LearnerDensity = R6Class("LearnerDensity", inherit = mlr3::Learner,
  public = list(
    #' @description
    #' Initialize the `LearnerDensity` object.
    initialize = function(id, param_set = ps(), predict_types = "prob", feature_types = character(0),
        properties = character(0), data_formats = "data.table", packages = character(0), man = NA_character_) {
      super$initialize(id = id, task_type = "density", param_set = param_set, feature_types = feature_types,
        predict_types = predict_types, properties = properties, data_formats = data_formats, packages = packages, man = man)
    },
    sample = function(n) {
      assertInt(n, tol = 1e-100)
      if (is.null(self$model)) stop("Learner is not trained.")
      if (!"sample" %in% self$properties) stop("sample() not supported.")
      private$.sample(n)
    }
  ),
  private = list(
    .sample = function(n) stop("sample() not implemented, even though \"sample\" property is given?!\nThis is a bug!")
  )
)

#' @title Featureless Density Estimation Learner
#'
#' @name mlr_learners_density.featureless
#'
#' @description
#' Simple [`LearnerDensity`] fallback learner which gives all predictions the density `.Machine$double.xmin` (typically `2.225074e-308`).
#' # TODO: could make this a hyperparameter.
#' @family density estimation classes
#' @export
LearnerDensityFeatureless = R6Class("LearnerDensityFeatureless",
  inherit = LearnerDensity,
  public = list(
    #' @description
    #' Initialize the `LearnerDensityFeatureless` object.
    initialize = function() {
      super$initialize(id = "density.featureless", feature_types = unname(mlr3::mlr_reflections$task_feature_types),
        properties = setdiff(mlr3::mlr_reflections$learner_properties$density, "sample"))
    }
  ),
  private = list(
    .train = function(task) invisible(NULL),
    .predict = function(task) {
      PredictionDensity$new(task = task, prob = .Machine$double.xmin)
    }
  )
)
