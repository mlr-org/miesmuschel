#' @title Surrogate Model Filtering
#'
#' @include Filtor.R
#'
#' @description
#' ` # TODO: enter the stuff in `OptimizerSumoHB.R` here. `
#'
#' @section Configuration Parameters:
#' ` # TODO: enter the stuff in `OptimizerSumoHB.R` here. `
#'
#' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)\cr
#'   Regression learner for the surrogate model filtering algorithm.
#'
#' @family filtors
#' @export
FiltorSurrogateProgressive = R6Class("FiltorSurrogateProgressive",
  inherit = Filtor,
  public = list(
    initialize = function(surrogate_learner) {
      # TODO: could be extended with selector and then support multi-crit.
      private$.surrogate_learner = assert_r6(surrogate_learner, "LearnerRegr")
      private$.own_param_set = ps(
        filter_rate_first = p_dbl(1, tags = "required"),
        filter_rate_per_sample = p_dbl(0, tags = "required")
      )
      private$.own_param_set$values = list(filter_rate_first = 0, filter_rate_per_sample = 1)

      param_classes = c("ParamInt", "ParamDbl", "ParamLgl", "ParamFct")
      if (!is.null(surrogate_learner)) {
        param_classes = param_classes[c("integer", "numeric", "logical", "factor") %in% surrogate_learner$feature_types]
      }

      super$initialize(param_classes, alist(private$.own_param_set, private$.surrogate_learner$param_set), supported = "single-crit")
    }
  ),
  active = list(
    #' @field surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)\cr
    #' Regression learner for the surrogate model filtering algorithm.
    surrogate_learner = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.surrogate_learner)) {
        stop("surrogate_learner is read-only.")
      }
      private$.surrogate_learner
    }
  ),
  private = list(
    .filter = function(values, known_values, fitnesses, n_filter) {
      params = self$param_set$values()
      values = first(values, self$needed_input)
      fcolname = "fitnesses"
      while (fcolname %in% colnames(known_values)) {
        fcolname = paste0(".", fcolname)
      }
      known_values[[fcolname]] = fitnesses
      surrogate_prediction = self$surrogate_learner$train(
        mlr3::TaskRegr$new("surrogate", known_values, target = fcolname)
      )$predict_newdata(values)$data$response
      selected = integer(0)
      for (i in seq_len(n_filter)) {
        population_size = round(params$filter_rate_first + params$filter_rate_per_sample * (i - 1))
        consider = first(surrogate_prediction, population_size)
        selected[[i]] = which.max(consider)
        consider[[selected[[i]]]] = -Inf
      }
      selected
    },
    .needed_input = function(output_size) {
      params = self$param_set$values()
      requested = params$filter_rate_first + params$filter_rate_per_sample * (output_size - 1)
      if (requested < output_size) stopf("filter_rate_first (which is %s) + filter_rate_per_sample (which is %s) times (output_size (which is %s) minus one) must at least be output_size.",
        params$filter_rate_first, params$filter_rate_per_sample, output_size)
      round(requested)
    },
    .surrogate_learner = NULL,
    .own_param_set = NULL
  )
)

