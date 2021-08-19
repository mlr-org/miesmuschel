#' @title BOHB KDE Ratio Learner
#'
#' @name mlr_learners_regr.kdratio
#'
#' @description
#' A [`LearnerRegr`][mlr3::LearnerRegr] that imitates the BOHB KDE method.
#'
#' # TODO
#' @export
LearnerRegrKDRatio = R6Class("LearnerRegrKDRatio", inherit = mlr3::LearnerRegr,
  public = list(
    #' @description
    #' Initialize the `LearnerRegrKDRatio` object.
    initialize = function(kdelearner) {
      param_set = ps(alpha = p_dbl(0, 1, tags = "train"), sampling_bw_factor = p_dbl(0, tags = "predict"),
        min_points_in_model = p_int(1, tags = "train"), min_bandwidth = p_dbl(0, tags = "train"),
        separators = p_uty(tags = "train", custom_check = crate(function(x) check_character(x, any.missing = FALSE)))
      )
      param_set$values = list(alpha = .15, sampling_bw_factor = 3, min_points_in_model = 1, min_bandwidth = 1e-3, separators = character(0))

      private$.kdelearner = mlr3::assert_learner(kdelearner)

      super$initialize(
        id = "regr.kdratio",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = "response",
        packages = character(0),
        param_set = param_set,
        properties = c("weights", "missings", "importance", "selected_features"),
        man = "miesmuschel::mlr_learners_regr.kdratio"
      )
    },

    #' @description
    #' Selected features are extracted from the model slot `frame$var`.
    #' @return `character()`.
    selected_features = function() {
    }
  ),
  active = list(
    kdelearner = function(val) {
      if (!missing(val)) stop("kdelearner is read-only.")
      private$.kdelearner
    }
  )

  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      names(pv) = replace(names(pv), names(pv) == "keep_model", "model")
      if ("weights" %in% task$properties) {
        pv = insert_named(pv, list(weights = task$weights$weight))
      }

      invoke(rpart::rpart, formula = task$formula(), data = task$data(), .args = pv, .opts = allow_partial_matching)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)
      response = invoke(predict, self$model, newdata = newdata, .opts = allow_partial_matching)
      list(response = unname(response))
    },
    .kdelearner = NULL
  )
)

# Kernel estimation in python is broken lol:
# https://github.com/statsmodels/statsmodels/issues/3790
# plan:
# - implement kde learner type
# - use 'np' package's npcdensbw + etc for training / predicting with a real learner
# - implement buggy kde
# - use those in the learner regr above
