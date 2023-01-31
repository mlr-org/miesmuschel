#' @title Terminator That Stops When a Generation-Wise Aggregated Value Stagnates
#'
#' @name mlr_terminators_genstag
#'
#' @description
#' [`Terminator`][bbotk::Terminator] that terminates when a value, aggregated over generations, stagnates.
#'
#' The user-supplied `fitness_aggregator` function is called whenever the archive of evaluated configurations contains a new generation.
#' The function is supplied with the fitness values, and optionally other data, of all individuals that are alive at that point
#' (`include_previous_generations = FALSE`) or at any point (`include_previous_generations = TRUE`).
#' Its result is saved inside the `$data_extra` field of the [`Archive`][bbotk::Archive] object.
#' Termination is then signaled when the aggregated value has stagnated, or not improved enough, for a given number of generations.
#'
#' The [`mies_aggregate_single_generation()`] function is used, see the documentation there for the functioning of `fitness_aggregator`.
#' The `fitness_aggregator` functions used for termination must return a scalar value or `NULL`, if a generation should be ignored.
#' The value returned by `fitness_aggregator` should be increasing for better performance, even if the underlying objective is being minimized.
#'
#' Termination is triggered in generation `i` when the aggregated value, returned by `fitness_aggregator`, of generation `i - patience`
#' is not greater than the maximum of all later generations (`i - patience + 1` .. `i`) by at least `min_delta`.
#'
#' If the aggregated value for generation `i - patience` is not known, for example because fewer than `patience + 1` generations
#' have been evaluated so far, because `fitness_aggregator` returned `NULL`, or because previous generations were evaluated
#' when `TerminatorGenerationStagnation` was not used, then termination is not triggered, regardless of whether values are known
#' for generations *before* generation `i - patience`.
#'
#' @section Multi-Fidelity Optimization:
#'
#' Multi-fidelity optimization can introduce a few edge-cases, for one because the individuals inside the generation(s) being aggregated may have
#' been evaluated with different fidelity values, which can give biased results. Besides that, it may also be problematic that increase of
#' fidelity could lead to "worse" aggregated results (e.g. because of reduced noise making max-aggregation worse), triggering an undesired termination.
#'
#' ## Termination from fidelity changes
#'
#' Higher fidelity values can sometimes lead to worse aggregated fitness values, which can trigger undesired termination.
#' However, in many multi-fidelity-setups, terminating before the last fidelity increase, controlled e.g. by the `fidelity`
#' configuration parameter of [`OptimizerMies`], may be undesirable to begin with.
#'
#' If the fidelity increase follows a fixed schedule based on evaluations or generations, one solution may be to use a
#' [`TerminatorCombo`][bbotk::TerminatorCombo] together with a [`TerminatorEvals`][bbotk::TerminatorEvals] or [`TerminatorGenerations`]
#' that prevents premature termination. Termination should happen at least `patience` generations after the last switch to the
#' highest fidelity if termination from biased values because of fidelity changes should be avoided.
#'
#' Otherwise it may be necessary to check whether the `budget` value given to `fitness_aggregator` reached the desired level,
#' and to prevent comparisons by letting `fitness_aggregator` return `NULL` if not.
#'
#' In both cases one may still have a problem with biased aggregations within an aggregated set of individuals if `include_previous_generations`
#' is `TRUE`.
#'
#' ## Biases within aggregated generation(s)
#'
#' When [`OptimizerMies`] is constructed with `multi_fidelity` set to `TRUE`, it typically evaluates some configurations multiple times,
#' at first with a lower fidelity, followed by an evaluation at "full" fidelity.
#' `fitness_aggregator` will only be called for generations containing entirely full-fidelity-evaluations will be aggregated.
#'
#' This is achieved by caching aggregated fitness values in the `$data_extra` field of the [`Archive`][bbotk::Archive] and only ever calling
#' `fitness_aggregator` for a generation that does not have a cached value. Since [`mies_step_fidelity()`] will
#' count low-fidelity evaluations as part of the "previous" generation, `fitness_aggregator` will not see them.
#'
#' It is possible for `fitness_aggregator` to see fitness values that were evaluated with different fidelities when using [`OptimizerMies`],
#' and
#'
#' 1. `fidelity_monotonic` is set to `TRUE` and fidelity decreases (unlikely setup), or
#' 2. if `fidelity_current_gen_only` is set to `FALSE` (advanced usage), or
#' 3. The value returned by the `fidelity` configuration parameter (not `fidelity_offspring`) changes over the course of optimization and
#'   `include_previous_generations` of `TerminatorGenerationStagnation` is set to `TRUE`.
#'
#' (1) and (2) are advanced scenarios that may need special consideration, but (3) may be a common, e.g. when doing multi-fidelity optimization
#' and stopping on overall dominated hypervolume stagnation. In this case, it may be necessary to inspect the `budget` value given to `fitness_aggregator`
#' and to remove all individuals evaluated with a different than the current fidelity.
#'
#' When using a custom-written optimization loop, case (1) relates to `fidelity_monotonic` argument of [`mies_step_fidelity()`] and [`mies_init_population()`],
#' and case (2) relates to the `current_gen_only` argument of [`mies_step_fidelity()`] and the `fidelity_new_individuals_only` argument of [`mies_init_population()`].
#' Case (3) relates to changing the fidelity given to [`mies_step_fidelity()`] if that function is used, or to changing the fidelity given to [`mies_evaluate_offspring()`] if
#' [`mies_step_fidelity()`] is not used.
#'
#' @section Dictionary:
#' This [`Terminator`][bbotk::Terminator] can be created with the short access form [`trm()`][bbotk::trm] ([`trms()`][bbotk::trm] to get a list),
#' or through the [dictionary][mlr3misc::Dictionary] [`mlr_terminators`][bbotk::mlr_terminators] in the following way:
#'
#' ```
#' # preferred
#' trm("genstag")
#' trms("genstag")  # takes vector IDs, returns list of Terminators
#'
#' # long form
#' mlr_terminators$get("genstag")
#' ```
#'
#' @section Configuration Parameters:
#' * `fitness_aggregator` :: `function`\cr
#'   Aggregation function, called with information about alive individuals of each generation.
#'   This argument is passed to [`mies_aggregate_single_generation()`], see there for more details.
#'   The aggregated values returned by `fitness_aggregator` should be maximized, so a larger value must be returned to indicate improvement in a generation,
#'   even if an underlying objective is being minimized. The return value must be a scalar `numeric(1)`.
#' * `include_previous_generations` :: `logical(1)`\cr
#'   Whether to aggregate over all individuals that were evaluated (`TRUE`), or only the individuals alive in the current generation (`FALSE`).
#'   If multi-fidelity optimization is being performed and individuals were re-evaluated with a different fidelity, their `x_id` will be the same and only
#'   the last fidelity-reevaluation will be given to `fitness_aggregator`. However, individuals from different generations may still have been evaluated
#'   with different fidelity and it may be necessary to inspect the `budget` value given to `fitness_aggregator` if `include_previous_generations` is `TRUE` in a
#'   multi-fidelity-setting. See the "Multi-Fidelity Optimization" section for more.
#' * `min_delta` :: `numeric(1)`\cr
#'   Minimum positive change of aggregated value to count as improvement.
#'   This value may also be negative, resulting in termination only when aggregated value *decreases* by at least the given amount.
#'   Initialized to 0.
#' * `patience` :: `integer(1)`\cr
#'   Number of generations with no improvement better than `min_delta` after which to terminate.
#'   Initialized to 1.
#'
#' @examples
#' library("bbotk")
#' # Terminate when hypervolume with nadir `c(0, 0, ...)`
#' # does not improve for 3 generations by at least 0.1:
#' trm("genstag",
#'   fitness_aggregator = function(fitnesses) domhv(fitnesses),
#'   include_previous_generations = TRUE,
#'   min_delta = 0.1,
#'   patience = 3
#' )
#' @export
TerminatorGenerationStagnation = R6Class("TerminatorGenerationStagnation", inherit = Terminator,
  public = list(
    #' @description
    #' Initialize the `TerminatorGenerationStagnation` object.
    initialize = function() {
      param_set = ps(
        fitness_aggregator = p_uty(tags = "required", custom_check = check_fitness_aggregator),
        include_previous_generations = p_lgl(tags = "required"),
        min_delta = p_dbl(tags = "required"),
        patience = p_int(1, tags = "required")
      )
      super$initialize(id = "genstag", param_set = param_set, properties = c("single-crit", "multi-crit"), unit = "stagnation")
    },

    #' @description
    #' Is `TRUE` if when the termination criterion is matched, `FALSE` otherwise.
    #' @param archive [`Archive`][bbotk::Archive]
    #'   Archive to check.
    #' @return `logical(1)`: Whether to terminate.
    is_terminated = function(archive) {
      assert_r6(archive, "Archive")
      pv = self$param_set$get_values()
      if (!nrow(archive$data)) return(FALSE)
      current_gen = max(archive$data$dob, 0)
      compare_gen = current_gen - pv$patience

      extra_entry = class(self)[[1]]
      if (!extra_entry %in% names(archive$data_extra)) {
        archive$data_extra[[extra_entry]] = numeric(0)
      }

      # fill entry in "data_extra" if no previous cached val exists.
      if (!as.character(current_gen) %in% names(archive$data_extra[[extra_entry]])) {
        aggregated = mies_aggregate_single_generation(archive, pv$fitness_aggregator, generation = current_gen,
          include_previous_generations = pv$include_previous_generations)
        assert_number(aggregated, null.ok = TRUE)
        if (!is.null(aggregated)) {
          archive$data_extra[[extra_entry]][[as.character(current_gen)]] = aggregated
        }
      }

      # do not terminate if generation 'i - patience' was not recorded.
      if (!as.character(compare_gen) %in% names(archive$data_extra[[extra_entry]])) {
        return(FALSE)
      }
      compare_gen_value = archive$data_extra[[extra_entry]][[compare_gen]]
      later_value = archive$data_extra[[extra_entry]][as.character(seq.int(compare_gen + 1, current_gen))]
      later_value_max = max(later_value, na.rm = TRUE)

      # stagnation if
      compare_gen_value >= later_value_max + pv$min_delta
    }
  )
)
