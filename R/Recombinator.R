

# TODO: dictionary & quick access function


#' @title Recombinator
#' @include MiesOperator
Recombinator = R6Class("Recombinator",
  inherit = MiesOperator,
  public = list(
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), n_indivs_in = 2, n_indivs_out = n_indivs_in) {
      assert_int(n_indivs_in, lower = 1, col = 1e-100)
      assert_int(n_indivs_out, lower = 1, col = 1e-100)
      private$.n_indivs_in = n_indivs_in
      private$.n_indivs_out = n_indivs_out
      super$initialize(param_classes, param_set)
    }
  ),
  active = list(
    n_indivs_in = function(val) {
      if (!missing(val)) stop("n_indivs_in is read-only.")
      private$.n_indivs_in
    },
    n_indivs_out = function(val) {
      if (!missing(val)) stop("n_indivs_out is read-only.")
      private$.n_indivs_out
    }
  ),
  private = list(
    .n_indivs_in = NULL,
    .n_indivs_out = NULL,
    .operate = function(values) {
      assert_true(nrow(values) %% self$n_indivs_in == 0)
      rbindlist(
        lapply(split(values, rep(nrow(values / self$n_indivs_in), each = self$n_indivs_in)), function(vs) {
          vs = private$.recombine(vs)
          assert_data_table(values, nrows = self$n_indivs_out)
        }), use.names = TRUE)
    },
    .recombine = function(values) stop(".recombine needs to be implemented by inheriting class.")
  )
)

RecombinatorNull = R6Class("RecombinatorNull",
  inherit = Recombinator,
  public = list(
    initialize = function() {
      super$initialize(n_indivs_in = 1)
    }
  ),
  private = list(
    .recombine = function(values) values
  )
)


#' @title Crossover Recombinator
#' @export
RecombinatorCrossoverUniform = R6Class("RecombinatorCrossoverUniform",
  inherit = Recombinator,
  public = list(
    initialize = function(keep_complement = TRUE) {
      param_set = ps(p = p_dbl(0, tags = "required"))
      param_set$values = list(p = 0.5)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set, 2, if (keep_complement) 2 else 1)
    }
  ),
  private = list(
    .recombine = function(values) {
      params = self$param_set$get_values()
      index = sample(1:2, length(values), TRUE, c(1 - params$p, params$p))
      values[, pmap(list(.SD, index), `[`)][seq_len(self$n_indivs_out)]
    }
  )
)

