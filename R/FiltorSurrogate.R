#' @title Abstract Surrogate Model Filtering Base Class
#'
#' @include Filtor.R
#'
#' @description
#' Abstract base class for surrogate model filtering.
#'
#' A *surrogate model* is a regression model, based on an [`mlr3::Learner`], which predicts the approximate performance of newly sampled configurations
#' given the empirical performance of already evaluated configurations. The surrogate model can be used to propose points that have, according to the
#' surrogate model, a relatively high chance of performing well.
#'
#' The `FiltorSurrogate` base class can be inherited from to create different [`Filtor`]s that filter based on a surrogate model, for example tournament
#' filtering or progresive filtering.
#'
#' @section Configuration Parameters:
#' `FiltorSurrogateProgressive`'s configuration parameters are the hyperparameters of the `surrogate_learner` [`Learner`][mlr3::Learner], as well as
#' the configuration parameters of the `surrogate_selector` [`Selector`].
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes depend on the supported feature types of the `surrogate_learner`, as reported
#' by `surrogate_learner$feature_types`: `"ParamInt"` requires
#' `"integer"`, `"ParamDbl"` requires `"numeric"`, `"ParamLgl"` requires `"logical"`, and `"ParamFct"` requires `"factor"`.
#'
#' @template param_surrogate_learner
#' @template param_surrogate_selector
#' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
#'   [`ParamSet`][paradox::ParamSet] of the method implemented in the inheriting class with configuration parameters that go beyond the
#'   parameters of the `surrogate_learner` and `surrogate_selector`.
#' @template param_packages
#' @template param_dict_entry
#'
#' @family base classes
#' @family filtors
#'
FiltorSurrogate = R6Class("FiltorSurrogate",
  inherit = Filtor,
  public = list(
    initialize = function(surrogate_learner, surrogate_selector = SelectorProxy$new(), param_set = ps(), packages = character(0), dict_entry = NULL) {
      private$.surrogate_learner = mlr3::as_learner(surrogate_learner, clone = TRUE)
      # can't assert LearnerRegr because GraphLearner doesn't announce that. Instead, we check $task_type
      assert_true(private$.surrogate_learner$task_type == "regr", .var.name = 'surrogate_learner$task_type == "regr"')

      private$.surrogate_selector = assert_r6(surrogate_selector, "Selector")$clone(deep = TRUE)
      private$.surrogate_selector$param_set$set_id = "select"
      private$.own_param_set = param_set
      private$.own_param_set$set_id = "filter"

      param_classes = c("ParamInt", "ParamDbl", "ParamLgl", "ParamFct")
      param_classes = param_classes[c("integer", "numeric", "logical", "factor") %in% surrogate_learner$feature_types]
      param_classes = intersect(param_classes, surrogate_selector$param_classes)

      super$initialize(param_classes, alist(private$.own_param_set,
        private$.surrogate_selector$param_set, private$.surrogate_learner$param_set),
        supported = surrogate_selector$supported,
        packages = c("mlr3", surrogate_selector$packages, surrogate_learner$packages, packages),
        dict_entry = dict_entry, own_param_set = quote(private$.own_param_set)
      )
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operator
    #' given to `surrogate_selector` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      private$.surrogate_selector$prime(param_set)
      if (param_set$has_deps && "missings" %nin% private$.surrogate_learner$properties) {
        stop("Surrogate learner %s needs to handle missing values for search space with dependencies", private$.surrogate_learner$id)
      }
      super$prime(param_set)
      invisible(self)
    }
  ),
  active = list(
    #' @field surrogate_learner ([`mlr3::LearnerRegr`])\cr
    #' Regression learner for the surrogate model filtering algorithm.
    surrogate_learner = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.surrogate_learner)) {
        stop("surrogate_learner is read-only.")
      }
      private$.surrogate_learner
    },
    #' @field surrogate_selector ([`Selector`])\cr
    #' [`Selector`] with which to select using surrogate-predicted performance
    surrogate_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.surrogate_selector)) {
        stop("surrogate_selector is read-only.")
      }
      private$.surrogate_selector
    }
  ),
  private = list(
    .filter = function(values, known_values, fitnesses, n_filter, context) {
      params = self$param_set$get_values(context = context)
      primed = self$primed_ps
      values = first(values, self$needed_input(n_filter, context))
      if (nrow(values) == n_filter) return(seq_len(n_filter))
      fcolname = "fitnesses"
      while (fcolname %in% colnames(known_values)) {
        fcolname = paste0(".", fcolname)
      }
      surrogate_prediction = apply(fitnesses, 2, function(f) {
        known_values[[fcolname]] = f
        self$surrogate_learner$train(
          mlr3::TaskRegr$new("surrogate", with_factor_cols(known_values, primed), target = fcolname)
        )$predict_newdata(with_factor_cols(values, primed))$data$response
      })
      # when things are one-dimensional they cease to be a matrix, so we force it here.
      surrogate_prediction = matrix(surrogate_prediction, nrow = nrow(values), ncol = ncol(fitnesses))
      private$.filter_surrogate(values, surrogate_prediction, known_values, fitnesses, n_filter, context)
    },
    .filter_surrogate = function(values, surrogate_prediction, known_values, fitnesses, n_filter, context) stop("abstract."),
    .surrogate_learner = NULL,
    .surrogate_selector = NULL,
    .own_param_set = NULL
  )
)

with_factor_cols = function(table, param_set) {
  table = copy(table)
  pclass  = param_set$class
  fcols = names(pclass)[pclass == "ParamFct"]
  plevels = param_set$levels

  for (col in fcols) {
    set(table, , col, factor(table[[col]], plevels[[col]]))
  }
  table
}
