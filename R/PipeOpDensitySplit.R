#' @title Split Regression Task into two Density Tasks
#'
#' @usage NULL
#' @name mlr_pipeops_densitysplit
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Create two [`TaskDensity`] from a [`TaskRegr`][mlr3::TaskRegr]: one comprising the upper `alpha` fraction, the other the 1 - `alpha` lower fraction
#' (but at least `min_size`).
#'
#' @section Construction:
#' ```
#' PipeOpDensitySplit$new(id = "densitysplit", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"densityratio"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpDensitySplit`] has one input channels named `"input"`, taking a [`TaskRegr`][mlr3::TaskRegr] during both training and prediction.
#'
#' [`PipeOpDensitySplit`] has two output channels, `"top"` and `"bottom"`, both [`TaskDensity`] during both training and prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOp`], as well as:
#' * `alpha` :: `numeric(1)`\cr
#'   What proportion of values to consider 'good'. BOHB has this at `0.15`.
#' * `min_size` :: `integer(1)`\cr
#'   Minimum size of both [`TaskDensity`] to create. Initialized to 1.
#'
#' @section Internals:
#' Can be used with [`PipeOpDensityRatio`].
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#' @family PipeOps
#' @family BOHB implementing operations
#' @export
PipeOpDensitySplit = R6Class("PipeOpDensitySplit",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(id = "densitysplit", param_vals = list()) {
      param_set = ps(alpha = p_dbl(tags = c("train", "required")), min_size = p_int(1, tags = c("train", "required")))
      param_set$values = list(alpha = 0.15, min_size = 1)
      super$initialize(id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = "input", train = "TaskRegr", predict = "TaskRegr"),
        output = data.table(name = c("top", "bottom"), train = "TaskDensity", predict = "TaskDensity"),
        tags = "ensemble"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()

      pv = self$param_set$get_values(tags = "train")
      task = inputs[[1]]

      if (task$nrow <= pv$min_size) stopf("Task must have more than min_size (%s) samples, but has %s.", pv$min_size, task$nrow)

      target = task$data(cols = task$target_names)
      rows = task$row_ids

      n_top = max(round(pv$alpha * task$nrow), pv$min_size)
      n_bottom = max(task$nrow - n_top, pv$min_size)
      order_target = order(target)

      rows_top = rows[rev(order_target)[seq_len(n_top)]]
      rows_bottom = rows[order_target[seq_len(n_bottom)]]

      new_col_roles = task$col_roles[intersect(names(task$col_roles), mlr3::mlr_reflections$task_col_roles$density)]

      top = TaskDensity$new(paste0(task$id, ".top"), task$backend)
      top$filter(rows = rows_top)
      top$col_roles = new_col_roles
      top$row_roles = task$row_roles

      bottom = TaskDensity$new(paste0(task$id, ".bottom"), task$backend)
      bottom$filter(rows = rows_bottom)
      bottom$col_roles = new_col_roles
      bottom$row_roles = task$row_roles

      list(top, bottom)
    },
    .predict = function(inputs) {
      task = inputs[[1]]
      new_col_roles = task$col_roles[intersect(names(task$col_roles), mlr3::mlr_reflections$task_col_roles$density)]
      new_row_roles = task$row_roles
      task = TaskDensity$new(paste0(task$id, ".density"), task$backend)
      task$col_roles = new_col_roles
      task$row_roles = new_row_roles

      list(task, task)
    },
    .splittask = function(task, pv) {
    }
  )
)

# mlr_pipeops$add("densitysplit", PipeOpDensitySplit)
