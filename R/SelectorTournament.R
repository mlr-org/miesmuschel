#' @title Tournament Selector
#'
#' @include Selector.R
#'
#' @name dict_selectors_tournament
#'
#' @description
#' [`Selector`] that repeatedly samples `k` individuals and selects the best ouf of these.
#'
#' @section Configuration Parameters:
#' * `k` :: `integer(1)`\cr
#'   Tournament size. Must be set by the user.
#' * `choose_per_tournament` :: Number of individuals to choose in each tournament. Must be smaller than `k`. The special value `0` sets this to the `group_size`
#'   hint given to the `$operate()`-call (but at most `k`). This is equal to `n_select` when used as survival-selector in [`mies_survival_plus()`]/[`mies_survival_comma()`],
#'   and equal to `n_indivs_in` of a [`Recombinator`] used in [`mies_generate_offspring()`].\cr
#'   Initialized to 1.
#' * `sample_unique` :: `character(1)`\cr
#'   Whether to sample individuals globally unique (`"global"`, selected individuals are removed from the population after each tournament),
#'   unique within groups (`"groups"`, individuals are replaced when `group_size` individuals were sampled), unique per tournament (`"tournament"`, individuals are replaced
#'   after each tournament), or not unique at all (`"no"`, individuals are sampled with replacement within tournaments).
#'   This is done with best effort; if `group_size` (when `sample_unique` is `"groups"`) or `n_select` (when `sample_unique` is `"global"`)
#'   is greater than `nrow(values)`, then the first `nrow(values) * floor(group_size / nrow(values))` or `nrow(values) * floor(n_select / nrow(values))` individuals
#'   are chosen deterministically by selecting every individual with the same frequency, followed by tournament selection for the remaining required individuals.
#'   Initialized to `"groups"`.
#'
#' @templateVar id tournament
#' @template autoinfo_prepare_sel
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family selectors
#' @examples
#' sb = sel("tournament", k = 4)
#' p = ps(x = p_dbl(-5, 5))
#' # dummy data; note that SelectorBest does not depend on data content
#' data = data.frame(x = rep(0, 7))
#' fitnesses = c(1, 5, 2, 3, 0, 4, 6)
#'
#' sb$prime(p)
#'
#' sb$operate(data, fitnesses, 2)
#'
#' sb$operate(data, fitnesses, 4, group_size = 2)
#' @export
SelectorTournament = R6Class("SelectorTournament",
  inherit = SelectorScalar,
  public = list(
    #' @description
    #' Initialize the `SelectorTournament` object.
    #' @template param_scalor
    initialize = function(scalor = ScalorSingleObjective$new()) {
      param_set = ps(
        k = p_int(1, tags = "required"),
        choose_per_tournament = p_int(0, tags = "required"),
        sample_unique = p_fct(c("global", "groups", "tournament", "no"), tags = "required")
      )
      param_set$values = list(choose_per_tournament = 1, sample_unique = "groups")

      super$initialize(scalor = scalor, is_deterministic = FALSE, param_set = param_set, dict_entry = "tournament", packages = "stats")
    }
  ),
  private = list(
    .select_scalar = function(values, fitnesses, n_select, group_size) {
      params = self$param_set$get_values()
      if (params$k < params$choose_per_tournament) stopf("k is %s, but must be >= choose_per_tournament (%s)", params$k, params$choose_per_tournament)
      if (params$sample_unique == "global") group_size = n_select
      if (params$sample_unique %in% c("tournament", "no")) group_size = 1
      group_size = vectorize_group_size(group_size, n_select)

      selected = list()  # collect individuals from each tournament-round, to un-list later.
      population_all = seq_along(fitnesses)

      # adapt this in case where choose_per_tournament is 0
      effective_cpt = params$choose_per_tournament

      overhang = integer(0)
      for (gs in group_size) {
        if (params$choose_per_tournament == 0) effective_cpt = gs
        if (gs >= length(population_all)) {
          # every individuum is selected at least once, we want to sample as evenly as we can
          # --> sample up to floor(gs / <population size>)
          # this is independent of overhang!
          preselected = rep.int(population_all, gs / length(population_all))
          gs = gs - length(preselected)
        } else {
          preselected = integer(0)
        }

        selected_round = list(preselected, overhang)
        have_selected = length(overhang)

        if (have_selected && have_selected < gs) {
          # population that can still be sampled inside this group
          # (can skip this part if the while loop below is skipped)
          population_group_remaining = setdiff(population_all, overhang)
        } else {
          population_group_remaining = population_all
        }
        while (have_selected < gs) {
          remaining = length(population_group_remaining)
          competitors = sample.int(remaining, size = min(remaining, params$k), replace = params$sample_unique == "no")
          ranking = order(fitnesses[population_group_remaining[competitors]], stats::runif(length(competitors)), decreasing = TRUE)
          hd_idx = seq_len(min(length(competitors), effective_cpt))
          selidx = competitors[ranking[hd_idx]]
          selected_round[[length(selected_round) + 1]] = population_group_remaining[selidx]
          population_group_remaining = population_group_remaining[-selidx]
          have_selected = have_selected + length(selidx)
        }
        if (have_selected > gs) {
          lastround = selected_round[[length(selected_round)]]
          skip_lastround = sample.int(length(lastround), have_selected - gs, replace = FALSE)
          overhang = lastround[skip_lastround]
          selected_round[[length(selected_round)]] = lastround[-skip_lastround]
        } else {
          overhang = integer(0)
        }

        selected_round_flat = unlist(selected_round)

        selected[[length(selected) + 1]] = selected_round_flat[sample.int(length(selected_round_flat))]
      }
      unlist(selected)
    }
  )
)
dict_selectors$add("tournament", SelectorTournament)

