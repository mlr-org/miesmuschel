#' @title Stratify Training by Feature
#'
#' @usage NULL
#' @name mlr_pipeops_stratify
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Subset the input as a [`Multiplicity`], causing subsequent [`PipeOp`]s to be executed independently
#' for each stratum according to `stratify_feature`.
#'
#' ` # TODO: missing: 'build only the model with the highest stratify param value' `
#'
#' @section Construction:
#' ```
#' PipeOpStratify$new(id = "stratify", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, default `"stratify"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpStratify`] has one input channel named `"input"`, taking a [`Task`][mlr3::Task], both during training and prediction.
#'
#' [`PipeOpStratify`] has two output channels; one named `"output"`, returning [`Task`][mlr3::Task] subsets as a
#' [`Multiplicity`] during training and prediction; and one named `"fallback"`, containing the whole [`Task`][mlr3::Task]
#' during training and the subset of the input [`Task`][mlr3::Task] that doesn't fit any stratum during prediction.
#'
#' @section State:
#' The `$state` is a named `list` with the following entries:
#' * `stratify_values` :: `any`\cr
#'   Vector indicating the values of features by which
#'
#' @section Parameters:
#' * `stratify_feature` :: `character(1)`\cr
#'   Name of the feature to stratify by.
#' * `min_size` :: `integer(1)`\cr
#'   Minimum number of samples for a stratification level so that a model gets trained. Initializedto 1.
#' * `predict_choice` :: `character(1)`\cr
#'   How to choose the model with which to make predictions. `"exact"`: only model with exactly matching
#'   stratification value used. `"exact_or_greater"`: if exact model was not trained, take the
#'   model with next higher stratify value. `"exact_or_less"`: if exact model was not trained, take the
#'   model with the next lower stratify value. `"nearest"`: Take the model with nearest stratify value,
#'   whether lower or higher. Initialized to `"exact"`.\cr
#'   Whenever a model with exact / greater / smaller / any stratify value is not present, the "fallback"
#'   model is used.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @family Experimental Features
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
#' @export
PipeOpStratify = R6Class("PipeOpStratify",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(id = "stratify", param_vals = list()) {
      param_set = ps(
        stratify_feature = p_uty(tags = c("train", "predict", "required"), custom_check = crate(function(x) check_string(x))),
        min_size = p_int(1, tags = c("train", "required")),
        predict_choice = p_fct(c("exact", "exact_or_greater", "exact_or_less", "nearest"), tags = c("predict", "required"))
      )

      param_set$values = list(min_size = 1, predict_choice = "exact")

      super$initialize(id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = "Task", predict = "Task"),
        output = data.table(name = c("output", "fallback"), train = c("[Task]", "Task"), predict = c("[Task]", "Task")),
        tags = c("target transform", "multiplicity")
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      task = inputs[[1]]
      pv = self$param_set$get_values(tags = "train")
      if (pv$stratify_feature %nin% task$feature_names) stopf("stratify_feature '%s' not in Task's features.", pv$stratify_feature)
      stratcol = task$data(cols = pv$stratify_feature)
      sample_numbers = stratcol[, .N, by = c(pv$stratify_feature)]
      self$state = list(stratify_values = sample_numbers[[pv$stratify_feature]][sample_numbers$N >= pv$min_size])

      list(output = as.Multiplicity(sapply(self$state$stratify_values, function(l) {
        taskpart = task$clone(deep = TRUE)$filter(task$row_ids[stratcol[[1]] == l])
        taskpart$col_roles$feature = setdiff(taskpart$col_roles$feature, pv$stratify_feature)
        taskpart
      }, simplify = FALSE)), fallback = task)
    },
    .predict = function(inputs) {
      task = inputs[[1]]
      pv = self$param_set$get_values(tags = "predict")
      stratcol = task$data(cols = pv$stratify_feature)[[1]]
      if (pv$predict_choice != "exact" && !is.numeric(stratcol)) stopf("predict_choice can only be 'exact' for non-numeric features, but is '%s'.", pv$predict_choice)
      mapping = data.table(stratify_values = self$state$stratify_values)
      stratdest = mapping[J(stratcol), x.stratify_values, on = "stratify_values", roll = switch(pv$predict_choice,
        exact = FALSE, exact_or_less = Inf, exact_or_greater = -Inf, nearest = "nearest")]

      list(output = as.Multiplicity(sapply(self$state$stratify_values, function(l) {
        taskpart = task$clone(deep = TRUE)$filter(task$row_ids[!is.na(stratdest) & stratdest == l])
        taskpart$col_roles$feature = setdiff(taskpart$col_roles$feature, pv$stratify_feature)
        taskpart
      }, simplify = FALSE)), fallback = task$clone(deep = TRUE)$filter(task$row_ids[is.na(stratdest)]))
    }
  )
)

# mlr_pipeops$add("stratify", PipeOpStratify)

