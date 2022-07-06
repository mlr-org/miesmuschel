#' @title Random Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_random
#'
#' @description
#' Random selector that disregards fitness and individual values and selects individuals randomly. Depending on the configuration parameter `replace`,
#' it samples with or without replacement.
#'
#' @section Configuration Parameters:
#' * `sample_unique` :: `character(1)`\cr
#'   Whether to sample individuals globally unique (`"global"`), unique within groups (`"groups"`), or not unique at all (`"no"`, sample with replacement).
#'   This is done with best effort; if `group_size` (when `sample_unique` is `"groups"`) or `nrow(values)` (when `sample_unique` is `"global"`)
#'   is smaller than `n_select`, then individuals are selected with as few repeats as possible. Initialized to `"groups"`.
#'
#' @templateVar id random
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @examples
#' set.seed(1)
#' sr = sel("random")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorRandom does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sr$prime(p)
#'
#' sr$operate(data, fitnesses, 2)
#' sr$operate(data, fitnesses, 2)
#' sr$operate(data, fitnesses, 2)
#'
#' sr$operate(data, fitnesses, 4)
#' sr$operate(data, fitnesses, 4)
#' sr$operate(data, fitnesses, 4)
#' @export
SelectorRandom = R6Class("SelectorRandom",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorRandom` object.
    initialize = function() {
      param_set = ps(sample_unique = p_fct(c("global", "groups", "no"), tags = "required"))
      param_set$values = list(sample_unique = "groups")
      super$initialize(param_set = param_set, dict_entry = "random")
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select, group_size) {
      params = self$param_set$get_values()
      if (group_size == 1 && params$sample_unique == "groups") params$sample_unique = "no"
      switch(params$sample_unique,
        groups = {
          unlist(lapply(vectorize_group_size(group_size, n_select), sample_least_replaces, set_size = nrow(values)))
        },
        global = {
          sample_least_replaces(n_select, nrow(values))
        },
        no = {
          sample(nrow(values), n_select, replace = TRUE)
        }
      )
    }
  )
)
dict_selectors$add("random", SelectorRandom)

sample_least_replaces = function(n_select, set_size) {
  if (set_size >= n_select) return(sample(set_size, n_select, replace = FALSE))
  c(rep.int(seq_len(set_size), n_select / set_size), sample(set_size, n_select %% set_size, replace = FALSE))[sample.int(n_select)]
}
