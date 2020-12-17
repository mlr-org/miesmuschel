

# TODO: dictionary & quick access function


#' @title Recombinator
#' @include MiesOperator.R
#' @include dictionaries.R
#' @export
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
    .recombine = function(values) stop(".recombine needs to be implemented by inheriting class."),
    .operate = function(values) {
      assert_true(nrow(values) %% self$n_indivs_in == 0)
      rbindlist(
        lapply(split(values, rep(nrow(values / self$n_indivs_in), each = self$n_indivs_in)), function(vs) {
          vs = private$.recombine(vs)
          assert_data_table(vs, nrows = self$n_indivs_out)
        }), use.names = TRUE)
    }
  )
)

#' @export
RecombinatorNull = R6Class("RecombinatorNull",
  inherit = Recombinator,
  public = list(
    initialize = function(n_indivs_in = 1, n_indivs_out = 1) {
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      assert_int(n_indivs_in, lower = n_indivs_out, tol = 1e-100)
      super$initialize(n_indivs_in = n_indivs_in, n_indivs_out = n_indivs_out)
    }
  ),
  private = list(
    .recombine = function(values) first(values, self$n_indivs_out)
  )
)
mlr_recombinators$add("null", RecombinatorNull)

#' @export
RecombinatorMaybe = R6Class("RecombinatorMaybe",
  inherit = Mutator,
  public = list(
    initialize = function(recombinator, recombinator_not = NULL) {
      private$.wrapped = assert_r6(recombinator, "Recombinator")$clone(deep = TRUE)
      if (is.null(recombinator_not)) {
        private$.wrapped_not = RecombinatorNull$new(recombinator$n_indivs_in, recombinator$n_indivs_out)
      } else {
        private$.wrapped_not = assert_r6(recombinator_not, "Mutator")$clone(deep = TRUE)
      }
      if (private$.wrapped$n_indivs_in != private$.wrapped_not$n_indivs_in ||
          private$.wrapped$n_indivs_out != private$.wrapped_not$n_indivs_out) {
        stop("recombinator and recombinator_not must have the same number of in / out individuals.")
      }

      private$.wrapped$param_set$set_id = "maybe"
      private$.wrapped_not$param_set$set_id = "maybe_not"

      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"))
      private$.maybe_param_set$values = list(p = 1)
      super$initialize(recombinator$param_classes,
        alist(private$.maybe_param_set, private$.wrapped$param_set),
        recombinator$n_indivs_in, recombinator$n_indivs_out)
    },
    prime = function(param_set) {
      private$.wrapped$prime(param_set)
      private$.wrapped_not$prime(param_set)
      super$prime(param_set)
    }
  ),
  private = list(
    .recombine = function(values) {
      if (runif(1) < self$param_set$get_values()$p) {
        private$.wrapped$operate(values)
      } else {
        private$.wrapped_not$operate(values)
      }
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
mlr_recombinators$add("maybe", RecombinatorMaybe)


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
mlr_recombinators$add("xounif", RecombinatorCrossoverUniform)
