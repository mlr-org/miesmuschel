#' @title Tournament Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_tournament
#'
#' @description
#' [`Selector`] that selects two individuals randomly for a tournament and then selects the better one based on the fitness value.
#'andl
#' @section Configuration Parameters:
#' This operator has no configuration parameters.
#'
#' @templateVar id tournament
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @examples
#' sb = sel("tournament")
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorTournament does not depend on data content
#' data = data.frame(x = rep(0, 5))
#' fitnesses = c(1, 5, 2, 3, 0)
#'
#' sb$prime(p)
#'
#' sb$operate(data, fitnesses, 2)
#'
#' sb$operate(data, fitnesses, 4)
#' @export
SelectorTournament = R6Class("SelectorTournament",
  inherit = Selector,
  public = list(
    #' @description
    #' Initialize the `SelectorTournament` object.
    initialize = function(scalor = ScalorProxy$new()) {
      private$.scalor = assert_r6(scalor, "Scalor")$clone(deep = TRUE)
      super$initialize(supported = private$.scalor$supported,
        param_set = alist(private$.scalor$param_set),
        packages = scalor$packages, dict_entry = "tournament")
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operator
    #' given to `scalor` during construction.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      private$.scalor$prime(param_set)
      super$prime(param_set)
      invisible(self)
    }
  ),
  active = list(
    #' @field scalor ([`Scalor`])\cr
    #' [`Scalor`] used to scalarize fitnesses for selection.
    scalor = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.scalor)) {
        stop("scalor is read-only.")
      }
      private$.scalor
    }
  ),
  private = list(
    .select = function(values, fitnesses, n_select, context) {
      map_int(seq_len(n_select), function(i) {
        ids = sample(nrow(values), size = 2L, replace = FALSE)
        order(private$.scalor$operate(values[ids, ], fitnesses[ids, , drop = FALSE], context = context), decreasing = TRUE)[1L]
      })
    },
    .scalor = NULL
  )
)
dict_selectors$add("tournament", SelectorTournament)
