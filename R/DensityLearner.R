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
    #' @description
    #' Sample from the fitted density.
    #' @param n (`integer(1)`)\cr
    #'   Number of samples to generate.
    #' @param lower (`numeric` | `NULL`)\cr
    #'   lower bounds, to sample truncated distributions. Must contain `NA` for non-numeric features. `NULL` (default): do not truncate.
    #' @param upper (`numeric` | `NULL`)\cr
    #'   upper bounds, to sample truncated distributions. Must contain `NA` for non-numeric features. `NULL` (default): do not truncate.
    sample = function(n, lower = NULL, upper = NULL) {
      assertInt(n, tol = 1e-100)
      if (is.null(self$model)) stop("Learner is not trained.")
      if (!"sample" %in% self$properties) stop("sample() not supported.")

      types = self$state$train_task$feature_types[self$state$train_task$feature_names]$type
      needs_na = types %nin% c("integer", "numeric")

      assertNumeric(lower, null.ok = TRUE, len = length(need_na))
      assertNumeric(upper, null.ok = TRUE, len = length(need_na))

      if (!is.null(lower)) {
        if (!all(is.na(lower) == needs_na)) stop("lower must be `NA` for train task columns that are not NA")
      } else {
        lower = ifelse(needs_na, NA_real_, -Inf)
      }

      if (!is.null(upper)) {
        if (!all(is.na(upper) == needs_na)) stop("upper must be `NA` for train task columns that are not NA")
      } else {
        upper = ifelse(needs_na, NA_real_, Inf)
      }

      private$.sample(n, lower, upper)
    }
  ),
  private = list(
    .sample = function(n, lower, upper) stop("sample() not implemented, even though \"sample\" property is given?!\nThis is a bug!")
  )
)

#' @title Featureless Density Estimation Learner
#'
#' @name mlr_learners_density.featureless
#'
#' @description
#' Simple [`LearnerDensity`] fallback learner which gives all predictions the density `.Machine$double.xmin` (typically `2.225074e-308`).
#'
#' ` # TODO: could make this a hyperparameter. `
#'
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
    .train = function(task) list(),
    .predict = function(task) {
      PredictionDensity$new(task = task, prob = .Machine$double.xmin)
    }
  )
)
