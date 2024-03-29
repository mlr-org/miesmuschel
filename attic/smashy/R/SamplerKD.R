#' @title KDE-model based Sampler
#'
#' @description
#' Sample similar to BOHB
#'
#' `# TODO`
#'
#' This currently hardcodes lots of things.
#'
#' @export
SamplerKD = R6Class("SamplerKD", inherit = Sampler,
  public = list(
    #' @description
    #' Initialize the `SamplerKD` object.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   [`ParamSet`][paradox::ParamSet] to sample from.
    #' @param task ([`TaskRegr`][mlr3::TaskRegr)\cr
    #'   Regression [`Task`][mlr3::Task] representing past performance values.
    #' @param minimize (`logical`)\cr
    #'   Whether outcome in `task` is to be minimized.
    initialize = function(param_set, task, minimize, alpha = 0.15, min_points_in_model = 0, bandwidth_factor = 3, min_bandwidth = 1e-3) {
      super$initialize(param_set)
      private$.task = assert_r6(task, "TaskRegr")
      private$.minimize = assert_flag(minimize)
      budget_id = setdiff(task$feature_names, param_set$ids())



      if (length(budget_id) > 1) stopf("Need at most one budget parameter for multifidelity method, but found %s: %s",
        length(budget_id), str_collapse(budget_id))

      # TODO: make sure removeconstants won't remove the budget column. Right now this is just a crutch:
      if (length(budget_id) && uniqueN(task$data(cols = budget_id)[[1]], na.rm = TRUE) <= 1) {
        budget_id = character(0)
      }

      if (getOption("miesmuschel.testing")) param_set$assert_dt(task$data(cols = setdiff(task$feature_names, budget_id))[, lapply(.SD, function(x) if (is.factor(x)) as.character(x) else x)])



      # not using %>>% since we are not importing mlr3pipelines
      graph = mlr3pipelines::Graph$new()$
        add_pipeop(mlr3pipelines::po("colapply", id = "colapply0", applicator = as.factor,
          affect_columns = mlr3pipelines::selector_type("character")))$
        add_pipeop(mlr3pipelines::po("fixfactors"))$
        add_pipeop(mlr3pipelines::po("removeconstants", id = "removeconstants0", affect_columns = selector_type("factor")))$  # prevent 0-level-columns
        add_pipeop(mlr3pipelines::po("colapply", applicator = as.numeric,
          affect_columns = mlr3pipelines::selector_type("integer")))$
        add_pipeop(mlr3pipelines::po("densitysplit", alpha = if (minimize) 1 - alpha else alpha))$
        add_pipeop(mlr3pipelines::po("removeconstants"))$
        add_pipeop(mlr3pipelines::po("imputesample"))$
        add_pipeop(mlr3::lrn("density.np", bwmethod = "normal-reference-numeric",
          sampling_bw_factor = bandwidth_factor, min_bandwidth = min_bandwidth))

      if (length(budget_id)) {
        # budget_id is present --> stratify by budget
        graph$add_pipeop(mlr3pipelines::po("stratify", stratify_feature = budget_id, predict_choice = "exact_or_less"))
        graph$pipeops$stratify$param_set$context_available = "inputs"
        graph$pipeops$stratify$param_set$values$min_size = ContextPV(function(inputs) max(min_points_in_model, inputs[[1]]$ncol + 1) + 1, min_points_in_model)
        graph$
          add_edge("colapply", "stratify")$
          add_edge("stratify", "densitysplit", src_channel = "output")
      } else {
        graph$add_edge("colapply", "densitysplit")
      }
      graph$
        add_edge("colapply0", "fixfactors")$
        add_edge("fixfactors", "removeconstants0")$
        add_edge("removeconstants0", "colapply")$
        add_edge("densitysplit", "removeconstants", src_channel = if (minimize) "bottom" else "top")$
        add_edge("removeconstants", "imputesample")$
        add_edge("imputesample", "density.np")

      # workaround since context does not work properly yet
      graph$pipeops$densitysplit$param_set$context_available = "inputs"
      graph$pipeops$densitysplit$param_set$values$min_size = ContextPV(function(inputs) max(min_points_in_model, inputs[[1]]$ncol + 1), min_points_in_model)

      if (length(budget_id)) {
        graph$train(task)
        max_stratum_index = which.max(graph$state$stratify$stratify_values)
        if (!length(max_stratum_index)) {
          private$.sampler = SamplerUnif$new(param_set)
          return(NULL)
        } else {
          private$.model = graph$pipeops$density.np$learner_model[[max_stratum_index]]
          # do this instead? : private$.features = graph$pipeops$removeconstants$state[[max_stratum_index]]
        }
      } else {
        # need to manually catch the < min_points_in_model case here, since model crashes otherwise.
        if (task$nrow <= max(min_points_in_model, task$ncol + 1)) {
          private$.sampler = SamplerUnif$new(param_set)
          return(NULL)
        } else {
          graph$train(task)
          private$.model = graph$pipeops$density.np$learner_model
          # ...... private$.features = graph$pipeops$removeconstants$state
        }
      }
      fnames = private$.model$state$train_task$feature_names
      missingfnames <- setdiff(self$param_set$ids(), fnames)
      if (length(missingfnames)) {
        missingps = do.call(ps, self$param_set$params[missingfnames])
        private$.sampler = SamplerUnif$new(missingps)
      }
    }
  ),
  active = list(
    task = function(val) {
      if (!missing(val)) stop("task is read-only.")
      private$.task
    },
    minimize = function(val) {
      if (!missing(val)) stop("minimize is read-only.")
      private$.minimize
    }
  ),
  private = list(
    .task = NULL,
    .minimize = NULL,
    .model = NULL,
    .sampler = NULL,
    .sample = function(n) {
      if (is.null(private$.model)) {
        # no mlr3::Learner model, but a paradox::Sampler, because task was too small.
        return(private$.sampler$sample(n)$data)
      }
      fnames = private$.model$state$train_task$feature_names  # TODO this obviously sucks
      intparams <- self$param_set$class[fnames] == "ParamInt"
      result <- private$.model$sample(n, self$param_set$lower[fnames] - intparams / 2, self$param_set$upper[fnames] + intparams / 2)
      for (i in which(intparams)) {
        col = result[[i]]
        col = round(col)
        col[col < self$param_set$lower[fnames][[i]]] <- self$param_set$lower[fnames][[i]]
        col[col < self$param_set$upper[fnames][[i]]] <- self$param_set$upper[fnames][[i]]
        result[, (i) := as.integer(col)]
      }
      chrparam <- self$param_set$class[fnames] == "ParamFct"
      for (i in which(chrparam)) {
        result[, (i) := as.character(.SD[[i]])]
      }
      if (!is.null(private$.sampler)) {
        # features that were not modelled are sampled uniformly
        result = cbind(result, private$.sampler$sample(n)$data)[, self$param_set$ids(), with = FALSE]
      }
      result
    }
  )
)

