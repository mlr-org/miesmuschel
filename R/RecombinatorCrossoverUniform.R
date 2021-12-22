#' @title Crossover Recombinator
#'
#' @include Recombinator.R
#'
#' @name dict_recombinators_xounif
#'
#' @description
#' Values between two individuals are exchanged with component-wise independent probability.
#'
#' @section Configuration Parameters:
#' * `p` :: `numeric(1)`\cr
#'   Component-wise probability with which to exchange values. Initialized to 0.5.
#'
#' @templateVar id xounif
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family recombinators
#' @examples
#' set.seed(1)
#' rx = rec("xounif")
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rx$prime(p)
#' rx$operate(data)
#'
#' rx$param_set$values$p = 0.3
#' rx$operate(data)
#' @export
RecombinatorCrossoverUniform = R6Class("RecombinatorCrossoverUniform",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize the `RecombinatorCrossoverUniform` object.
    #' @param keep_complement (`logical(1)`)\cr
    #'   Whether the operation should keep both individuals that were crossed over (`TRUE`), or only the first and discard
    #'   the crossover complement (`FALSE`). Default `TRUE`.
    #'   The `$keep_complement` field will reflect this value.
    initialize = function(keep_complement = TRUE) {
      param_set = ps(p = p_dbl(0, tags = "required"))
      param_set$values = list(p = 0.5)
      super$initialize(c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set, 2, if (keep_complement) 2 else 1, dict_entry = "xounif")
    }
  ),
  active = list(
    #' @field keep_complement (`logical(1)`)\cr
    #' Whether the operation keeps both individuals that were crossed over or discards the crossover complement.
    keep_complement = function(val) {
      if (!missing(val)) stop("keep_complement is read-only.")
      private$.n_indivs_out == 2
    }
  ),
  private = list(
    .recombine = function(values, context) {
      params = self$param_set$get_values(context = context)
      index = lapply(sample(1:2, length(values), TRUE, c(1 - params$p, params$p)), function(x) c(x, 3 - x))
      # for hiearchical spaces we posthoc fix dependent parameters so that recombination essentially happens over blocks of parameters
      if (context$inst$search_space$has_deps) {
        deps = context$inst$search_space$deps
        deps[, nr := match(deps$id, context$inst$search_space$ids())]
        for (parent in unique(deps$on)) {
          parent_nr = match(parent, context$inst$search_space$ids())
          combination = index[[parent_nr]]
          index[deps[on == parent][["nr"]]] = list(combination)
        }
      }
      setnames(values[, pmap(list(.SD, index), `[`)][seq_len(self$n_indivs_out)], names(values))
    }
  )
)
dict_recombinators$add("xounif", RecombinatorCrossoverUniform)
