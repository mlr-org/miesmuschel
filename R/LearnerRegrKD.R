#' @title BOHB KDE Ratio Learner
#'
#' @name mlr_learners_regr.kdratio
#'
#' @description
#' A [`LearnerRegr`][mlr3::LearnerRegr] that imitates the BOHB KDE method.
#'
#' @section Hyperparameters
#' * `alpha` :: `numeric(1)`\cr
#'   What proportion of values to consider 'good'. BOHB has this at `0.15`.
#' * `min_points_in_model` :: `integer(1)`\cr
#'   Minimum number of points for both 'good' and 'bad' points model. Defaults to number of features + 1 if not given.
#' * `min_density` :: `numeric(1)`\cr
#'   Minimum density to consider for density ratio. Should be marginally greater than 0 to avoid too large numbers when dividing. BOHB has this as `1e-32`.
#'
#'
#' # TODO
#' @export
LearnerRegrKDRatio = R6Class("LearnerRegrKDRatio", inherit = mlr3::LearnerRegr,
  public = list(
    #' @description
    #' Initialize the `LearnerRegrKDRatio` object.
    initialize = function(kdelearner_good, kdelearner_bad) {
      private$.own_param_set = ps(alpha = p_dbl(0, 1, tags = c("train", "required")), min_points_in_model = p_int(1, tags = "train"),
        separators = p_uty(tags = c("train", "required"), custom_check = crate(function(x) check_character(x, any.missing = FALSE)))
      )
      private$.own_param_set$values = list(alpha = .15)

      private$.kdelearner_good = mlr3::as_learner(kdelearner_good, clone = TRUE)
      private$.kdelearner_bad = mlr3::as_learner(kdelearner_bad, clone = TRUE)
      private$.kdelearner_good$param_set$set_id = "kdelearner_good"
      private$.kdelearner_bad$param_set$set_id = "kdelearner_bad"

      super$initialize(
        id = "regr.kdratio",
        feature_types = c("logical", "integer", "numeric", "factor", "ordered"),
        predict_types = "response",
        packages = character(0),
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
    #' @field kdelearner_good ([`LearnerDensity`])\cr
    #' [`LearnerDensity`] for the `alpha` 'good' training points. Read-only.
    kdelearner_good = function(val) {
      if (!missing(val)) stop("kdelearner_good is read-only.")
      private$.kdelearner_good
    },
    #' @field kdelearner_bad ([`LearnerDensity`])\cr
    #' [`LearnerDensity`] for the 1 - `alpha` 'bad' training points. Read-only.
    kdelearner_bad = function(val) {
      if (!missing(val)) stop("kdelearner_bad is read-only.")
      private$.kdelearner_bad
    },
    #' @field param_set ([`ParamSet`][paradox::ParamSet])\cr
    #' Configuration parameters of the `MiesOperator` object. Read-only.
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        # TODO: need test that checks that all paramset elements have good context
        private$.param_set = ParamSetCollection$new(
          private$.own_param_set,
          private$.kdelearner_good$param_set,
          private$.kdelearner_bad$param_set
        )
        if (!is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },

  ),
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
    .kdelearner_good = NULL,
    .kdelearner_bad = NULL,
    deep_clone = function(name, value) {
      # This is taken from MiesOperator
      if (!is.null(private$.param_set)) {
        private$.param_set_id = private$.param_set$set_id
        private$.param_set = NULL  # required to keep clone identical to original, otherwise tests get really ugly
      }

      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      if (name == "state") {
        value$log = copy(value$log)
      }
      value
    },
    .param_set_id = NULL,
    .param_set_source = NULL,
    .own_param_set = NULL
  )
)

# Kernel estimation in python is broken lol:
# https://github.com/statsmodels/statsmodels/issues/3790
# plan:
# - [X] implement kde learner type
# - [X] use 'np' package's npudensbw + etc for training / predicting with a real learner
# - implement buggy kde
# - use those in the learner regr above
