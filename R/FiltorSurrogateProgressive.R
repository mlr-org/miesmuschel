#' @title Progressive Surrogate Model Filtering
#'
#' @include Filtor.R
#'
#' @name dict_filtors_surprog
#'
#' @description
#' Performs progressive surrogate model filtering.
#'
#' A *surrogate model* is a regression model, based on an [`mlr3::Learner`], which predicts the approximate performance of newly sampled configurations
#' given the empirical performance of already evaluated configurations. If the optional `surrogate_learner` construction argument is given to `SumoHB`,
#' then the surrogate model is used to propose points that have, according to the surrogate model, a relatively high chance of performing well.
#'
#' The filtering is "progressive" in that successive values are filtered more agressively.
#'
#' @section Algorithm:
#'
#' Given the number `n_filter` of of individuals to sample, progressive surrogate model filtering proceeds as follows:
#' 1. Train the `surrogate_learner` [`LearnerRegr`][mlr3::LearnerRegr] on the `known_values` and their `fitnesses`.
#' 2. Take `filter_rate_first` configurations, predict their expected performance using the surrogate model, and put them
#'    into a pool `P` of configurations to consider.
#' 3. Take the individual that is optimal according to predicted performance, remove it from `P` and add it to solution set `S`.
#' 4. If the number of solutions in `S` equals `n_filter`, quit.
#' 5. Take the next `filter_rate_per_sample` configurations, predict their expected performance using the surrogate model, and add them to `P`.
#' 6. Jump to 3.
#'
#' (The algorithm presented here is optimized for clarity; the actual implementation does all the surrogate model prediction in one go, but is functionally
#' equivalent).
#'
#' The `filter_rate_first` and `filter_rate_per_sample` configuration parameters of this algorithm determine how agressively the surrogate model is used to
#' filter out sampled configurations. If the filtering is agressive (`filter_rate_first` is large), then more "exploitation" at the cost of "exploration" is performed.
#' When `filter_rate_first` is small but `filter_rate_per_sample` is large, then successive individuals are filtered successively more agressively, potentially
#' leading to a tradeoff between "exploration" and "exploitation".
#'
#' When `filter_rate_per_sample` is set to 0, then the method is equivalent to sampling the top `n_filter` individuals from `filter_rate_first`
#' sampled ones. When `filter_rate_per_sample` is 1 and `filter_rate_first` is 0, then the method is equivalent to random sampling.
#'
#' `filter_rate_first` and `filter_rate_per_sample` may be fractional; the total number of individuals to select from when selecting the `i`th
#' individuals is always `round(filter_rate_first + (filter_rate_per_sample - 1) * (i - 1))`. However, `filter_rate_first` must
#' be at least 1, and `filter_rate_first + filter_rate_per_sample * (n_filter - 1)` must be at least `n_filter`.
#'
#' @section Configuration Parameters:
#' `FiltorSurrogateProgressive`'s configuration parameters are the hyperparameters of the `surrogate_learner` [`Learner`][mlr3::Learner], as well as:
#'
#' @template param_filter_rate_first
#' @template param_filter_rate_per_sample
#'
#' @templateVar id surprog
#' @templateVar additional , <surrogate_learner>
#' @template autoinfo_prepare_ftr
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes depend on the supported feature types of the `surrogate_learner`, as reported
#' by `surrogate_learner$feature_types`: `"ParamInt"` requires
#' `"integer"`, `"ParamDbl"` requires `"numeric"`, `"ParamLgl"` requires `"logical"`, and `"ParamFct"` requires `"factor"`.
#'
#' @template autoinfo_dict
#'
#' @param surrogate_learner ([`mlr3::LearnerRegr`] | `NULL`)\cr
#'   Regression learner for the surrogate model filtering algorithm.
#'
#' @family filtors
#'
#' @example
#' library("mlr3")
#' library("mlr3learners")
#' fp = ftr("surprog", lrn("regr.lm"), filter_rate_first = 2)
#'
#' p = ps(x = p_dbl(-5, 5))
#' known_data = data.frame(x = 1:5)
#' fitnesses = 1:5
#' new_data = data.frame(x = c(2.5, 4.5))
#'
#' fp$prime(p)
#'
#' fp$needed_input(1)
#'
#' fp$operate(new_data, known_data, fitnesses, 1)
#'
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
      private$.own_param_set$values = list(filter_rate_first = 1, filter_rate_per_sample = 1)

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
      params = self$param_set$get_values()
      values = first(values, self$needed_input(n_filter))
      fcolname = "fitnesses"
      while (fcolname %in% colnames(known_values)) {
        fcolname = paste0(".", fcolname)
      }
      known_values[[fcolname]] = c(fitnesses)
      surrogate_prediction = self$surrogate_learner$train(
        mlr3::TaskRegr$new("surrogate", known_values, target = fcolname)
      )$predict_newdata(values)$data$response
      selected = integer(0)
      for (i in seq_len(n_filter)) {
        population_size = round(params$filter_rate_first + params$filter_rate_per_sample * (i - 1))
        consider = first(surrogate_prediction, population_size)
        selected[[i]] = which.max(consider)
        assert_true(is.finite(consider[[selected[[i]]]]))
        surrogate_prediction[[selected[[i]]]] = -Inf
      }
      selected
    },
    .needed_input = function(output_size) {
      params = self$param_set$get_values()
      requested = params$filter_rate_first + params$filter_rate_per_sample * (output_size - 1)
      if (requested < output_size) stopf("filter_rate_first (which is %s) + filter_rate_per_sample (which is %s) times (output_size (which is %s) minus one) must at least be output_size.",
        params$filter_rate_first, params$filter_rate_per_sample, output_size)
      round(requested)
    },
    .surrogate_learner = NULL,
    .own_param_set = NULL
  )
)
dict_filtors$add("surprog", FiltorSurrogateProgressive)
