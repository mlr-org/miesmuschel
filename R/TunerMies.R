#' @include OptimizerMies.R
#' @rdname OptimizerMies
#' @export
TunerMies = R6Class("TunerMies", inherit = mlr3tuning::TunerFromOptimizer,
  public = list(
    #' @description
    #' Initialize the `TunerMies` object.
    #' @param mutator ([`Mutator`])
    #' @param recombinator ([`Recombinator`])
    #' @param parent_selector ([`Selector`])
    #' @param survival_selector ([`Selector`])
    #' @param elite_selector ([`Selector`] | `NULL`)
    #' @param multi_fidelity (`logical(1)`)
    initialize = function(mutator = MutatorProxy$new(), recombinator = RecombinatorProxy$new(), parent_selector = SelectorProxy$new(),
                          survival_selector = SelectorProxy$new(), elite_selector = NULL, init_selector = survival_selector, multi_fidelity = FALSE) {
      super$initialize(OptimizerMies$new(mutator = mutator, recombinator = recombinator, parent_selector = parent_selector,
        survival_selector = survival_selector, elite_selector = elite_selector, init_selector = init_selector, multi_fidelity = multi_fidelity))
    }
  )
)
