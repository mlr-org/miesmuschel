#' @title Predict Ratio of Densities
#'
#' @usage NULL
#' @name mlr_pipeops_densityratio
#' @format [`R6Class`] inheriting from [`PipeOp`].
#'
#' @description
#' Predict the ratio of two [`PredictionDensity`] inputs as [`PredictionRegr`], dividing the input to `"numerator"` by the input to `"denominator"`.
#'
#' @section Construction:
#' ```
#' PipeOpDensityRatio$new(id = "densityratio", param_vals = list())
#' ```
#'
#' * `id` :: `character(1)`\cr
#'   Identifier of the resulting object, default `"densityratio"`.
#' * `param_vals` :: named `list`\cr
#'   List of hyperparameter settings, overwriting the hyperparameter settings that would otherwise be set during construction. Default `list()`.
#'
#' @section Input and Output Channels:
#' [`PipeOpDensityRatio`] has two input channels named `"numerator"` and `"denominator"`, both taking `NULL` during training and a [`PredictionDensity`]
#' during prediction.
#'
#' [`PipeOpDensityRatio`] has one output channel named `"output"`, returning `NULL` during training and a [`PredictionRegr`][mlr3::PredictionRegr]
#' during prediction.
#'
#' @section State:
#' The `$state` is left empty (`list()`).
#'
#' @section Parameters:
#' The parameters are the parameters inherited from the [`PipeOp`], as well as:
#' * `min_density` :: `numeric(1)`\cr
#'   Minimum density to consider for density ratio. Should be marginally greater than 0 to avoid too large numbers when dividing. Initialized to `1e-32`.
#'
#' @section Internals:
#' Can best be used with [`PipeOpDensitySplit`]
#'
#' @section Fields:
#' Only fields inherited from [`PipeOp`].
#'
#' @section Methods:
#' Only methods inherited from [`PipeOp`].
#' @family PipeOps
#' @family Ensembles
#' @family BOHB implementing operations
#' @export
PipeOpDensityRatio = R6Class("PipeOpDensityRatio",
  inherit = mlr3pipelines::PipeOp,
  public = list(
    initialize = function(id = "densityratio", param_vals = list()) {
      param_set = ps(min_density = p_dbl(tags = c("predict", "required")))
      param_set$values = list(min_density = 1e-32)
      super$initialize(id, param_set = param_set, param_vals = param_vals,
        input = data.table(name = c("numerator", "denominator"), train = "NULL", predict = "PredictionDensity"),
        output = data.table(name = "output", train = "NULL", predict = "PredictionRegr"),
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
      assert_true(identical(inputs[[1]]$row_ids, inputs[[2]]$row_ids))

      pv = self$param_set$get_values(tags = "predict")
      lmd = log(pv$min_density)

      list(mlr3::PredictionRegr$new(row_ids = inputs[[1]]$row_ids, truth = NA_real_,
        response = exp(pmax(inputs$numerator$logprob, lmd) - pmax(inputs$denominator$logprob, lmd))))
    }
  )
)

# mlr_pipeops$add("densityratio", PipeOpDensityRatio)
