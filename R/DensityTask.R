#' @title Density Task
#'
#' @description
#' This task implements [`mlr3::Task`] for density estimation tasks.
#' This is an unsupervised task; there is no target column (but there may, e.g., be a weight column).
#' The `task_type` is `"density"`.
#'
#' @family density estimation classes
#' @export
TaskDensity = R6Class("TaskDensity",
  inherit = mlr3::Task,
  public = list(
    #' @description
    #' Initialize the `TaskDensity` object.
    initialize = function(id, backend) {
      super$initialize(id = id, task_type = "density", backend = backend)
    }
  )
)

#' @title Convert to a Density Task
#'
#' @description
#' Convert to a [`TaskDensity`].
#'
#' @inheritParams mlr3::as_task
#' @return [`TaskDensity`]
#' @family density estimation classes
#' @export
as_task_density = function(x, ...) UseMethod("as_task_density")

#' @rdname as_task_density
#' @param clone (`logical(1)`)'cr
#'   Whether to ensure the returned object is different from input `x`.
#' @export
as_task_density.TaskDensity = function(x, clone = FALSE, ...) {
  if (clone) x$clone() else x
}

#' @rdname as_task_density
#' @param id (`character(1)`)\cr
#'   ID for the new [`Task`][mlr3::Task].
#'   Defaults to the (deparsed) name of the expression given to `x`.
#' @export
as_task_density.data.frame = function(x, id = deparse1(substitute(x)), ...) {
  TaskDensity$new(id = id, backend = x)
}

#' @rdname as_task_density
#' @export
as_task_density.DataBackend = function(x, id = deparse1(substitute(x)), ...) {
  TaskDensity$new(id = id, backend = x)
}

#' @title Old Faithful Density Estimation Task
#'
#' @name mlr_tasks_faithful
#'
#' @format [R6::R6Class] inheriting from [`TaskDens`]
#'
#' @section Construction
#' ```
#' tsk("faithful")
#' ```
#'
#' @description
#' A density estimation task for the [`datasets::faithful`] dataset.
#' @family density estimation classes
NULL

load_faithful = function(id = "faithful") {
  ts = TaskDensity$new(id, mlr3::as_data_backend(load_dataset("faithful", "datasets")))
  ts$hash = ts$man = "miesmuschel::mlr_tasks_faithful"
  ts
}



