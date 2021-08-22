#' @title Density Estimation Prediction Object
#'
#' @description
#' Predictions returned by [`LearnerDensity`] and evaluated by [`MeasureDensity`].
#'
#' @family density estimation classes
#' @export
PredictionDensity = R6Class("PredictionDensity", inherit = mlr3::Prediction,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Initialize the `PredictionDensity` object. One of `prob` or `logprob` must be given.
    #' @param task ([`TaskDensity`])\cr
    #'   [`TaskDensity`] for which prediction was made, used to extract defaults for `row_ids`.
    #' @param row_ids (`integer`)\cr
    #'   Row ids of the predicted observations.
    #' @param prob (`numeric`)\cr
    #'   Predicted density
    #' @param logprob (`numeric`)\cr
    #'   Predicted log density.
    #' @param check (`logical(1)`)\cr
    #'   Whether to perform argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = mlr3::assert_task(task)$row_ids, prob = exp(logprob), logprob = log(prob), check = TRUE) {
      self$data = structure(list(row_ids = row_ids, prob = prob, logprob = logprob), class = c("PredictionDataDensity", "PredictionData"))
      if (check) {
        self$data = mlr3::check_prediction_data(self$data)
      }
      self$task_type = "density"
      self$man = "miesmuschel::PredictionDensity"
      self$predict_types = "prob"
    }
  ),
  active = list(
    #' @field prob (`numeric`)\cr
    #'   Predicted density.
    prob = function(val) {
      if (!missing(val)) stop("prob is read-only.")
      self$data$prob
    },
    #' @field prob (`numeric`)\cr
    #'   Predicted log density.
    logprob = function(val) {
      if (!missing(val)) stop("logprob is read-only.")
      self$data$logprob
    }
  )
)

#' @export
as.data.table.PredictionDensity = function(x, ...) {
  as.data.table(x$data[c("row_ids", "prob", "logprob")])
}

#' @exportS3Method mlr3::as_prediction
as_prediction.PredictionDataDensity = function(x, check = TRUE, ...) {
  invoke(PredictionDensity$new, check = check, .args = x)
}

#' @exportS3Method mlr3::check_prediction_data
check_prediction_data.PredictionDataDensity = function(pdata) {
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  assert_numeric(pdata$prob, lower = 0)
  assert_true(all.equal(exp(pdata$logprob), pdata$prob))
  pdata
}

#' @exportS3Method mlr3::is_missing_prediction_data
is_missing_prediction_data.PredictionDataDensity = function(pdata) {
  pdata$row_ids[is.na(pdata$prob) | is.na(pdata$logprob)]
}

#' @export
c.PredictionDataDensity = function(..., keep_duplicates = TRUE) {
  args = list(...)
  assert_list(args, "PredictionDataDensity")
  assert_flag(keep_duplicates)
  fulltable = rbindlist(dots)
  if (!keep_duplicates) fulltable = unique(fulltable, by = "row_ids", fromLast = TRUE)
  structure(as.list(fulltable), class = c("PredictionDataDensity", "PredictionData"))
}

#' @title Convert to a Density Estimation Prediction
#'
#' @description
#' Convert an object to a [`PredictionDensity`]
#'
#' @return [`PredictionDensity`].
#' @export
#' @family density estimation classes
as_prediction_density = function(x, ...) UseMethod("as_prediction_density")

#' @rdname as_prediction_density
#' @export
as_prediction_density.PredictionDensity = function(x, ...) x

#' @rdname as_prediction_density
#' @export
as_prediction_density.data.frame = function(x, ...) {
  assert_names(names(x), must.include = "row_ids", subset.of = c("row_ids", "prob", "logprob"))
  invoke(PredictionDensity$new, .args = x)
}


