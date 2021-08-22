#' @title Combine Predictions Row-Wise.
#'
#' @usage NULL
#' @name mlr_pipeops_predictionunion
#' @format [`R6Class`] object inheriting from [`PipeOp`].
#'
#' @description
#' Combine [`Prediction`][mlr3::Prediction]` made for different subsets of a [`Task`][mlr3::Task] into a combined [`Prediction`][mlr3::Prediction].
#'
#' @section Construction:
#' ```
#' PipeOpPredictionUnion$new(id = "predictionunion", param_vals = list())
#' ```
#'
#' * `innum` :: `numeric(1)` | `character`\cr
#'   Determines the number of input channels.
#'   If `innum` is 0 (default), a vararg input channel is created that can take an arbitrary number
#'   of inputs. If `innum` is a `character` vector, the number of input channels is the length of
#'   `innum`, and the columns of the result are prefixed with the values.
#' * `collect_multiplicity` :: `logical(1)`\cr
#'   If `TRUE`, the input is a [`Multiplicity`] collecting channel. This means, a
#'   [`Multiplicity`] input, instead of multiple normal inputs, is accepted and the members are aggregated. This requires `innum` to be 0.
#'   Default is `FALSE`.
#' * `id` :: `character(1)`
#'   Identifier of the resulting object, default `"predictionunion"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise
#'   be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpPredictionUnion`] has multiple input channels depending on the `innum` construction
#' argument, named `"input1"`, `"input2"`, ... if `innum` is nonzero; if `innum` is 0, there is
#' only one *vararg* input channel named `"..."`. All input channels take `NULL` during training and a [`Prediction`][mlr3::Prediction]
#' object during prediction.
#'
#' [`PipeOpPredictionUnion`] has one output channel named `"output"`, producing `NULL` during training and a [`Prediction`][mlr3::Prediction]
#' object during prediction.
#'
#' The output is a [`Prediction`][mlr3::Prediction] constructed by `c()`ing (effectively `rbind`-ing) all [`Prediction`][mlr3::Prediction]s during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' [`PipeOpPredictionUnion`] has no parameters.
#'
#' @section Internals:
#' [`PipeOpPredictionUnion`] uses the [`Prediction`][mlr3::Prediction] S3 `c()` method provided by `mlr3`.
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#'
#' @family PipeOps
#' @family Multiplicity PipeOps
#' @seealso https://mlr3book.mlr-org.com/list-pipeops.html
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
PipeOpPredictionUnion = R6Class("PipeOpPredictionUnion",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(innum = 0L, collect_multiplicity = FALSE, id = "predictionunion", param_vals = list()) {
      assert_int(innum, lower = 0L)

      inname = if (innum) rep_suffix("input", innum) else "..."
      intype = "Task"

      private$.collect = assert_flag(collect_multiplicity)

      if (collect_multiplicity) {
        if (innum) {
          stop("collect_multiplicity only works with innum == 0.")
        }
        inname = "[...]"
        intype = sprintf("[%s]", intype)
      }

      super$initialize(id, param_vals = param_vals,
        input = data.table(name = inname, train = intype, predict = intype),
        output = data.table(name = "output", train = "Task", predict = "Task"),
        tags = "ensemble"
      )
    }
  ),
  private = list(
    .train = function(inputs) {
      self$state = list()
      list(NULL)
    },
    .predict = function(inputs) {
      if (private$.collect) inputs = keep(unclass(inputs[[1]]), function(x) length(x$row_ids))
      if (!length(inputs)) return(list(Prediction$new()))
      if (length(unique(map_chr(inputs, "task_type"))) > 1) stop("Mixing Prediction task_type not possible.")
      if (length(unique(map_chr(inputs, function(x) paste(sort(x$predict_types), collapse = ",")))) > 1) stop("Mixing Prediction predict_types not possible.")

      resultpred = invoke(c, .args = c(unname(inputs), list(keep_duplicates = TRUE)))  # can't put keep_duplicates in the front because S3 won't happen then.

      if (anyDuplicated(resultpred$row_ids)) stop("Some rows were predicted multiple times.")
      list(resultpred)

    },
    .collect = NULL
  )
)
# mlr_pipeops$add("predictionunion", PipeOpPredictionUnion)
