#' @title Evaluate Proposed Configurations Generated in a MIES Iteration
#'
#' @description
#' Calls `$eval_batch` of a given [`OptimInstance`][bbotk::OptimInstance] on a set
#' of configurations as part of a MIES operation. The `dob` extra-info in the archive
#' is also set properly to indicate a progressed generation.
#'
#' This function can be used directly, but it is easier to use it within the [`OptimizerMies`]
#' [`Optimizer`][bbotk::Optimizer] if standard GA operation is desired.
#'
#' "Rolling-tide" multifidelity is supported as described in [`OptimizerMies`] or more thoroughly
#' in `vignette("mies-multifid")`. For this,
#' an extra component named after `budget_id` is appended to each individual, chosen from
#' the `fidelity_schedule` depending on the value of `survivor_budget`. Both `fidelity_schedule`
#' and `budget_id` should be the same values as given to the other `mies_*` functions.
#'
#' @template param_inst
#' @param offspring (`data.frame`)\cr
#'   Proposed configurations to be evaluated, must have columns named after the `inst`'s search space, minus `budget_id` if not `NULL`.
#' @template param_fidelity_schedule_maybenull
#' @template param_budget_id_maybenull
#' @param survivor_budget (`logical(1)`)\cr
#'   When doing multi-fidelity optimization, determines which column of `fidelity_schedule` to use to determine the budget component value.
#' @return invible [`data.table`][data.table::data.table]: the performance values returned when evaluating the `offspring` values
#'   through `eval_batch`.
#' @family mies building blocks
#' @export
mies_evaluate_offspring = function(inst, offspring, fidelity_schedule = NULL, budget_id = NULL, survivor_budget = FALSE) {
  assert_r6(inst, "OptimInstance")
  offspring = as.data.table(assert_data_frame(offspring))
  ss_ids = inst$search_space$ids()
  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity_schedule))
  assert_flag(survivor_budget)
  current_gen = max(inst$archive$data$dob, 0) + 1
  assert_names(colnames(offspring), must.include = setdiff(ss_ids, budget_id), disjunct.from = survivor_budget)  # TODO: must not include survivor_budget, but can include other things

  if (!is.null(fidelity_schedule)) {
    assert(check_fidelity_schedule(fidelity_schedule))
    fidelity_schedule = as.data.table(fidelity_schedule)
    fidelity_schedule = setkeyv(copy(fidelity_schedule), "generation")
    fidelity_column = if (survivor_budget) "budget_survivors" else "budget_new"
    fidelity = fidelity_schedule[data.table(generation = current_gen), fidelity_column, on = "generation", roll = TRUE, with = FALSE]
    setnames(fidelity, budget_id)
    offspring = cbind(offspring, fidelity)
  }

  eval_batch_handle_zero(inst, cbind(offspring, dob = current_gen, eol = NA_real_))
}

#' @title Re-Evaluate Configurations with Higher Fidelity
#'
#' @description
#' As part of the "rolling-tide" multifidelity-setup, do reevaluation of configurations with
#' higher fidelity that have survived lower-fidelity selection.
#'
#' The `fidelity_schedule` specifies what the fidelity (i.e. value of the "budget" component) of evaluations
#' done or compared at generation `i` should be. The `generation_lookahead` parameter has a subtle influence.
#' If it is `TRUE`, then, at the end of generation `i`, reevaluation is done with the `"budget_survivors"`
#' fidelity of generation `i + 1`, otherwise the entry of generation `i` is used. This gives the `"budget_survivors"`
#' column a slightly different meaning in the two cases: either it is the budget, with which survivors "entering"
#' generation `i` have been evaluated, or it is the budget with which survivors "coming out of" that generation
#' were evaluated. The evaluations are done as part of the *current* generation in any case (see examples).
#'
#' This function should only be called when doing rolling-tide multifidelity, and should not be part of the
#' MIES cycle otherwise.
#'
#' @template param_inst
#' @param fidelity_schedule (`data.frame`)\cr
#'   `data.frame` with three columns `"generation"`, `"budget_new"`, `"budget_survivors"`, in that order. `"budget_new"` and `"budget_survivors"`
#'   are atomic columns. The value of `"budget_survivors"` of the appropriate row is assigned to the `budget_id` component of `offspring`.
#'   `"generation"` is an integer valued column, indicating the first generation at which a row is valid. At least one row with
#'   `generation == 1` must be present.
#' @param budget_id (`character(1)`)\cr
#'   Budget component that is set to the value found in `fidelity_schedule`.
#' @param generation_lookahead (`logical(1)`)\cr
#'   Whether to re-evaluate alive individuals with the `"budget_survivors"` entry of the current generation (`FALSE`), or
#'   of the next generation (`TRUE`).
#' @param current_gen_only (`logical(1)`)\cr
#'   Whether to only re-evaluate survivors individuals generated in the latest generation (`TRUE`), or re-evaluate all currently alive
#'   individuals (`FALSE`). In any case, only individuals that were not already evaluated with the chosen fidelity are evaluated,
#'   so this will usually only affect individuals from before the latest generation if the `"budget_survivors"` changed between
#'   generations (i.e. when a different row from `fidelity_schedule` is used).
#' @param monotonic (`logical(1)`)\cr
#'   Whether to only re-evaluate configurations for which the fidelity would increase. Default `TRUE`.
#' @return [invible] [`data.table`][data.table::data.table]: the performance values returned when evaluating the `offspring` values
#'   through `eval_batch`.
#' @family mies building blocks
#' @export
mies_step_fidelity = function(inst, fidelity_schedule, budget_id, generation_lookahead = TRUE, current_gen_only = FALSE, monotonic = TRUE) {
  assert_r6(inst, "OptimInstance")
  assert_flag(generation_lookahead)
  assert_flag(current_gen_only)
  assert_flag(monotonic)
  data = inst$archive$data
  current_gen = max(data$dob, 0)
  assert_choice(budget_id, inst$search_space$ids())
  assert(check_fidelity_schedule(fidelity_schedule))
  fidelity_schedule = as.data.table(fidelity_schedule)

  if (!any(is.na(data$eol))) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)

  next_fidelity = fidelity_schedule[data.table(generation = current_gen + generation_lookahead), "budget_survivors", on = "generation", roll = TRUE, with = FALSE][[1]]

  comparator = if (monotonic) `<` else `!=`
  reeval = which((!current_gen_only | data$dob == current_gen) & is.na(data$eol) & comparator(data[[budget_id]], next_fidelity))

  set(data, reeval, "eol", current_gen)
  indivs = data[reeval, inst$search_space$ids(), with = FALSE]
  indivs[, (budget_id) := next_fidelity]

  # I hate this... but there seems to be no way to avoid the TerminatorGenerations from terminating here :-(
  # What happens here is that we decrease dob by 1 everywhere, evaluate with current-generation-minus-one,
  # and then increase dob by 1 again. This avoids TerminatorGenerations from firing because if it *did*
  # mind about dob minus 1, then it would have fired already.
  # Weird edge-case: A TerminatorGenerations with `generations` set to 0 will not prevent reevals of initial
  # pop if the initial pop is entirely made up of preexisting values.
  inst$archive$data[, dob := dob - 1]
  # stupid race condition. Hope we don't get trapped here...
  on.exit(inst$archive$data[, dob := dob + 1])

  eval_batch_handle_zero(inst, set(indivs, , c("dob", "eol"), list(current_gen - 1, NA_real_)))
}

#' @title Choose Survivors According to the "Mu + Lambda" ("Plus") Strategy
#'
#' @description
#' Choose survivors during a MIES iteration using the "Plus" survival strategy, i.e.
#' combining all alive individuals from the latest and from prior generations indiscriminately and
#' choosing survivors using a survival [`Selector`] operator.
#'
#' @template param_inst
#' @template param_mu
#' @template param_survival_survival_selector
#' @template param_survival_dotdotdot
#' @template param_survival_return
#' @family mies building blocks
#' @export
mies_survival_plus = function(inst, mu, survival_selector, ...) {
  assert_r6(inst, "OptimInstance")
  assert_int(mu, lower = 1, tol = 1e-100)
  data = inst$archive$data

  alive = which(is.na(data$eol))
  if (!length(alive)) stop("No alive individuals. Need to run mies_init_population()?")

  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)

  survivors = mies_select_from_archive(inst, mu, alive, survival_selector, get_indivs = FALSE)
  if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")
  died = setdiff(alive, survivors)
  data[died, eol := max(dob)]
}

#' @title Choose Survivors According to the "Mu , Lambda" ("Comma") Strategy
#'
#' @description
#' Choose survivors during a MIES iteration using the "Comma" survival strategy, i.e.
#' selecting survivors from the latest generation only, using a [`Selector`] operator, and choosing
#' "elites" from survivors from previous generations using a different [`Selector`] operator.
#'
#' @template param_inst
#' @template param_mu
#' @template param_survival_survival_selector
#' @param n_elite (`integer(1)`)\cr
#'   Number of individuals to carry over from previous generations. `n_elite` individuals will be selected
#'   by `elite_selector`, while `mu - n_elite` will be selected by `survival_selector` from the most
#'   recent generation. `n_elite` may be 0 (no elitism), in which case only individuals from the newest
#'   generation survive. `n_elite` must be strictly smaller than `mu` to permit any optimization progress.
#' @param elite_selector ([`Selector`])\cr
#'   [`Selector`] operator that selects "elites", i.e. surviving individuals from previous generations,
#'   depending on configuration values
#'   and objective results. When `elite_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.\cr
#'   The [`Selector`] must be primed on `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization.\cr
#'   The given [`Selector`] may *not* return duplicates.
#' @template param_survival_dotdotdot
#' @template param_survival_return
#' @family mies building blocks
#' @export
mies_survival_comma = function(inst, mu, survival_selector, n_elite, elite_selector, ...) {
  assert_r6(inst, "OptimInstance")
  assert_int(mu, lower = 1, tol = 1e-100)
  assert_int(n_elite, lower = 0, upper = mu - 1, tol = 1e-100)

  data = inst$archive$data
  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)
  alive_before = data[, which(is.na(eol) & dob != max(dob))]
  current_offspring = data[, which(is.na(eol) & dob == max(dob))]

  if (!length(alive_before)) stop("No alive individuals. Need to run mies_init_population()?")
  if (!length(current_offspring)) stop("No current offspring. Need to run mies_evaluate_offspring()?")

  survivors = if (mu > n_elite) mies_select_from_archive(inst, mu - n_elite, current_offspring, survival_selector, get_indivs = FALSE)
  if (anyDuplicated(survivors)) stop("survival_selector may not generate duplicates.")

  elites = mies_select_from_archive(inst, n_elite, alive_before, elite_selector, get_indivs = FALSE)
  if (anyDuplicated(elites)) stop("elite_selector may not generate duplicates.")

  survivors = c(elites, survivors)

  died = setdiff(alive, survivors)
  data[died, eol := max(dob)]
}

#' @title Prime MIES Operators
#'
#' @include ParamSetShadow.R
#'
#' @description
#' Prime the given [`MiesOperator`]s for an optimization run with the given search space.
#'
#' In its simplest form, MIES optimization only optimizes the search space of the [`Objective`][bbotk::Objective] to be optimized. However,
#' more advanced optimization may handle a "budget" parameter for multi-fidelity optimization differently: It is still selected by [`Selector`]s,
#' but not mutated or recombined and instead handled separately. It is also possible to add additional components to the search space that are
#' not evaluated by the objective function, but that are used for self-adaption by other operators.
#'
#' The `mies_prime_operators()` function uses the information that the user usually has readily at hand -- the [`Objective`][bbotk::Objective]`s search space,
#' the budget parameter, and additional components -- and primes [`Mutator`], [`Recombinator`], and [`Selector`] objects in the right way:
#' * [`Selector`]s are primed on a union of `search_space` and `additional_components`
#' * [`Mutator`]s and [`Recombinator`]s are primed on the [`Selector`]'s space with the `budget_id` [`Param`][paradox::Param] removed.
#'
#' `mies_prime_operators()` is called with an arbitrary number of [`MiesOperator`] arguments; typically one [`Mutator`], one [`Recombinator`] and
#' at least two [`Selector`]: one for survival selection, and one parent selection. Supplied [`MiesOperator`]s are primed by-reference, but
#' they are also returned as [invisible] `list`.
#'
#' If neither additional components nor multi-fidelity optimization is used, it is also possible to use the `$prime()` function of hte [`MiesOperator`]s
#' directly, although using `mies_prime_operators()` gives flexibility for future extension.
#' @param mutators (`list` of [`Mutator`])\cr
#'   [`Mutator`] objects to prime. May be empty (default).
#' @param recombinators (`list` of [`Recombinator`])\cr
#'   [`Recombinator`] objects to prime. May be empty (default).
#' @param selectors (`list` of [`Selector`])\cr
#'   [`Selector`] objects to prime. May be empty (default).
#' @param search_space ([`ParamSet`][paradox::ParamSet])\cr
#'   Search space of the [`Objective`][bbotk::Objective] or [`OptimInstance`][bbotk::OptimInstance] to be optimized.
#' @param additional_components ([`ParamSet`][paradox::ParamSet] | `NULL`)\cr
#'   Additional components to optimize over, not included in `search_space`, but possibly used for self-adaption. This must be the [`ParamSet`][paradox::ParamSet]
#'   of `mies_init_population()`'s `additional_component_sampler` argument.
#' @param budget_id (`character(1)` | `NULL`)\cr
#'   Budget component used for multi-fidelity optimization.
#' @return `invisible` named `list` with entries `$mutators` (`list` of [`Mutator`], primed `mutators`), `$recombinators` (`list` of [`Recombinator`], primed `recombinators`),
#'   and `$selectors` (`list` of [`Selector`], primed `selectors`).
#' @export
mies_prime_operators = function(mutators = list(), recombinators = list(), selectors = list(), search_space, additional_components = NULL, budget_id = NULL) {
  assert_list(mutators, types = "Mutator", any.missing = FALSE)
  assert_list(recombinators, types = "Recombinator", any.missing = FALSE)
  assert_list(selectors, types = "Selector", any.missing = FALSE)
  assert_r6(search_space, "ParamSet")
  assert_r6(additional_components, "ParamSet", null.ok = TRUE)
  assert_choice(budget_id, search_space$ids(), null.ok = TRUE)

  if (is.null(additional_components)) {
    full_search_space = search_space
  } else {
    search_space = search_space$clone(deep = FALSE)
    additional_components = additional_components$clone(deep = FALSE)
    search_space$set_id = ""
    additional_components$set_id = ""
    full_search_space = ps_union(list(search_space, additional_components))
  }

  # selectors are primed with entire searchspace
  selectors = lapply(selectors, function(x) x$prime(full_search_space))

  nobudget_search_space = ParamSetShadow$new(full_search_space, budget_id)

  # mutators, recombinators are primed with search space minus budget_id
  mutators = lapply(mutators, function(x) x$prime(nobudget_search_space))
  recombinators = lapply(recombinators, function(x) x$prime(nobudget_search_space))

  invisible(list(mutators = mutators, recombinators = recombinators, selectors = selectors))
}

#' @title Initialize MIES Optimization
#'
#' @description
#' Set up an [`OptimInstance`][bbotk::OptimInstance] for MIES optimization.
#' This adds the `dob` and `eol` columns to the instance's archive, and makes sure there are at least `mu` survivors
#' (i.e. entries with `eol` set to `NA`) present. If there are already `>= mu` prior evaluations present, then the last
#' `mu` of these remain alive (the other's `eol` set to 0); otherwise, up to `mu` new randomly sampled configurations
#' are evaluated and added to the archive and have `eol` set to `NA`.
#'
#' @template param_inst
#' @template param_mu
#' @param initializer (`function`)\cr
#'   Function that generates a [`Design`][paradox::Design] object, with arguments `param_set` and `n`, functioning like [`paradox::generate_design_random`]
#'   or [`paradox::generate_design_lhs`]. Note that [`paradox::generate_design_grid`] can not be used and must be wrapped with
#'   a custom function that ensures that only `n` individuals are produced. The generated design must correspond to the `inst`'s `$search_space`; for
#'   components that are not in the objective's search space, the `additional_component_sampler` is used.
#' @template param_fidelity_schedule_maybenull
#' @template param_budget_id_maybenull
#' @param additional_component_sampler ([`Sampler`][paradox::Sampler] | `NULL`)\cr
#'   [`Sampler`][paradox::Sampler] for components of individuals that are not part of `inst`'s `$search_space`. These components
#'   are never used for performance evaluation, but they may be useful for self-adaptive [`OperatorCombination`]s. See the description
#'   of [`mies_prime_operators()`] on how operators need to be primed to respect additional components.\cr
#'   It is possible that `additional_component_sampler` is used for *more* rows than `initializer`, which happens
#'   when the `inst`'s `$archive` contains prior evaluations that are alive, but does not contain columns pertaining to additional columns,
#'   or contains *all* these columns but there are rows that are `NA` valued. If only *some* of the columns are present, or if all these columns
#'   are present but there are rows that are only `NA` valued for some columns, then an error is thrown.\cr
#'   Default is `NULL`: no additional components.
#' @return [invisible] [`OptimInstance`][bbotk::OptimInstance]: the input
#'   instance, modified by-reference.
#'
#' @family mies building blocks
#' @export
mies_init_population = function(inst, mu, initializer = generate_design_random, fidelity_schedule = NULL, budget_id = NULL, additional_component_sampler = NULL) {
  assert_r6(inst, "OptimInstance")

  assert_int(mu, lower = 1, tol = 1e-100)
  assert_function(initializer, nargs = 2)
  assert_r6(additional_component_sampler, "Sampler", null.ok = TRUE)

  ss_ids = inst$search_space$ids()
  ac_ids = if (is.null(additional_component_sampler)) character(0) else additional_component_sampler$param_set$ids()
  present_cols = colnames(inst$archive$data)

  assert_choice(budget_id, ss_ids, null.ok = is.null(fidelity_schedule))
  if (!is.null(fidelity_schedule)) {
    assert(check_fidelity_schedule(fidelity_schedule))
  }

  if (any(c("dob", "eol") %in% ss_ids)) {
    stop("'dob' and 'eol' may not be search space dimensions.")
  }
  if (any(c("dob", "eol") %in% ac_ids)) {
    stop("'dob' and 'eol' may not be additional component dimensions.")
  }
  if (any(ss_ids %in% ac_ids)) {
    stopf("Search space and additional components name clash: %s", str_collapse(intersect(ss_ids, ac_ids)))
  }
  if (any(ac_ids %in% present_cols) && !all(ac_ids %in% present_cols)) {
    stopf("Some, but not all, additoinal components already in archive: %s", str_collapse(intersect(present_cols, ac_ids)))
  }
  dob = batch_nr = NULL
  if (nrow(inst$archive$data)) {
    if ("eol" %nin% present_cols) {
      inst$archive$data[, eol := NA_real_]
    } else if ("dob" %nin% present_cols) {
      stop("'eol' but not 'dob' column found in archive; this is an undefined state that would probably lead to bad behaviour, so stopping.")
    }

    if ("dob" %nin% present_cols) {
      inst$archive$data[, dob := 0]
    }
    data = inst$archive$data  # adding columns is not guaranteed to happen by reference, so we need to be careful with copies of data!
    assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
    assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)
  }
  alive = which(is.na(inst$archive$eol))
  n_alive = length(alive)
  mu_remaining = mu - n_alive

  # take care of additional components
  # we need to check if / how many additional components we need to sample. If there are no alive individuals,
  # then this is just mu. If there are alive individuals, it *may* also be mu (we will have to insert components
  # at those rows where there are alive individuals!)
  additional_needed = mu  # how many more rows of additional components need to be sampled. by default: mu
  row_insert = last(alive, mu)  # at what rows of `data` the components are inserted
  if (n_alive && all(ac_ids %in% present_cols)) {
    # additional components already present in the archive. Let's check if there are any rows with NAs.
    missing_info = is.na(data[row_insert, ac_ids, with = FALSE])
    any_missing_ac = apply(missing_info, 1, all)
    if (any(apply(missing_info, 1, any) != any_missing_ac)) {
      stop("There are rows in inst$archive$data where some, but not all additional component values are NA.")
    }
    row_insert = row_insert[any_missing_ac]  # only insert at the rows where any/all values are missing
    additional_needed = max(0, mu - n_alive) + length(row_insert)  # need to generate components for the missing rows, and for the values that get sampled new
  }

  additional_components = NULL
  if (!is.null(additional_component_sampler)) {
    additional_components = assert_data_frame(additional_component_sampler$sample(additional_needed)$data, nrows = additional_needed)

    # assert here that:
    # we either have some individuals that need to be sampled (mu_remaining > 0)
    #   in which case the additional_components table's first `length(row_insert)` and last `mu_remaining` rows are complementary
    # or that we have no more individuals to sample, in which case `additional_components` are exactly equal to the number of rows where things get inserted.
    assert_true((mu_remaining > 0 && length(row_insert) + mu_remaining == additional_needed) || (mu_remaining <= 0 && length(row_insert) == additional_needed))

    # if we don't insert anything, then this operation just adds the necessary columns in case they are missing, and does nothing otherwise
    inst$archive$data[row_insert, ac_ids] <- first(additional_components, length(row_insert))
  }

  if (mu_remaining < 0) {
    # TODO: we are currently killing the earliest evals, maybe do something smarter.
    # TODO: also we are not doing anything about budget here, we just hope the user knows what he's doing.
    # TODO: also also the additional component handling above depends on killing earliest now.
    inst$archive$data[first(which(is.na(eol)), -mu_remaining), eol := max(dob)]
  } else if (mu_remaining > 0) {
    sample_space = inst$search_space
    if (!is.null(budget_id)) {
      sample_space = ParamSetShadow$new(sample_space, budget_id)
    }
    mies_evaluate_offspring(inst,
      cbind(as.data.table(assert_data_frame(initializer(sample_space, mu_remaining)$data, nrows = mu_remaining)),
        last(additional_components, mu_remaining)),
      fidelity_schedule, budget_id, survivor_budget = TRUE)
  }
  invisible(inst)
}

#' @title Get Fitness Values from OptimInstance
#'
#' @description
#' Get fitness values in the correct form as used by [`Selector`] operators from an
#' [`OptimInstance`][bbotk::OptimInstance].
#' This works for both single-criterion and multi-criterion optimization, and entails multiplying
#' objectives with -1 if they are being minimized, since [`Selector`] tries to maximize fitness.
#'
#' @template param_inst
#' @template param_rows
#' @return `numeric` `matrix` with `length(rows)` (if `rows` is given, otherwise `nrow(inst$archive$data)`) rows
#' and one column for each objective: fitnesses to be maximized.
#' @family mies building blocks
#' @export
mies_get_fitnesses = function(inst, rows) {
  assert_r6(inst, "OptimInstance")

  multiplier = map_dbl(inst$archive$codomain$tags, function(x) switch(x, minimize = -1, maximize = 1, 0))
  fitnesses = as.matrix(inst$archive$data[rows, inst$archive$codomain$ids()[multiplier != 0], with = FALSE])
  multiplier = multiplier[multiplier != 0]
  sweep(fitnesses, 2L, multiplier, `*`)
}

#' @title Select Individuals from an OptimInstance
#'
#' @include Selector.R
#'
#' @description
#' Apply a [`Selector`] operator to a subset of configurations inside
#' an [`OptimInstance`][bbotk::OptimInstance]
#' and return the index within the archive (when `get_indivs` `FALSE`) or the configurations themselves
#' (when `get_indivs` is `TRUE`).
#'
#' It is not strictly necessary for the selector to select unique individuals / individuals without replacement.
#'
#' @template param_inst
#' @param n_select (`integer(1)`)\cr
#'   Number of individuals to select.
#' @template param_rows
#' @param `selector` ([`Selector`])\cr
#'   [`Selector`] operator that selects individuals depending on configuration values
#'   and objective results. When `selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.
#'   Defaults to [`SelectorBest`].\cr
#'   The [`Selector`] must be primed on a superset of `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. All components on which `selector` is primed on must occur in the archive.\cr
#'   The given [`Selector`] *may* return duplicates.
#' @param get_indivs (`logical(1)`)\cr
#'   Whether to return configuration values from within the archive (`TRUE`) or just the indices within
#'   the archive (`FALSE`). Default is `TRUE`.
#' @return `integer` | [`data.table`][data.table::data.table]: Selected individuals, either index into `inst` or subset of archive table,
#'   depending on `get_indivs`.
#' @family mies building blocks
#' @export
mies_select_from_archive = function(inst, n_select, rows, selector = SelectorBest$new()$prime(inst$search_space), get_indivs = TRUE) {
  assert_r6(inst, "OptimInstance")

  assert_r6(selector, "Selector")
  assert_true(selector$is_primed)
  assert_int(n_select, lower = 0, tol = 1e-100)
  if (!missing(rows)) assert_integerish(rows, lower = 1, upper =  nrow(inst$archive$data), tol = 1e-100)

  selector_params <- selector$primed_ps$ids()
  assert_subset(inst$search_space$ids(), selector_params)
  assert_subset(selector_params, colnames(inst$archive$data))

  if (n_select == 0) if (get_indivs) return(inst$archive$data[0, selector_params, with = FALSE]) else return(integer(0))
  indivs = inst$archive$data[rows, selector_params, with = FALSE]

  fitnesses = mies_get_fitnesses(inst, rows)

  selected = selector$operate(indivs, fitnesses, n_select)
  if (get_indivs) {
    indivs[selected]
  } else {
    if (missing(rows)) selected else rows[selected]
  }
}

#' @title Generate Offspring Through Mutation and Recombination
#'
#' @include Selector.R
#' @include Mutator.R
#' @include Recombinator.R
#' @include ParamSetShadow.R
#'
#' @description
#' Generate new proposal individuals to be evaluated using [`mies_evaluate_offspring()`].
#'
#' Parent individuals are selected using `parent_selector`, then mutated using `mutator`, and thend
#' recombined using `recombinator`. If only a subset of these operations is desired, then
#' it is possible to set `mutator` or `recombinator` to the respective "null"-operators.
#'
#' @template param_inst
#' @param lambda (`integer(1)`)\cr
#'   Number of new individuals to generate. This is not necessarily the number with which `parent_selector`
#'   gets called, because `recombinator` could in principle need more than `lambda` input individuals to
#'   generate `lambda` output individuals.
#' @param parent_selector ([`Selector`] | `NULL`)\cr
#'   [`Selector`] operator that selects parent individuals depending on configuration values
#'   and objective results. When `parent_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses()`]), since [`Selector`]s always try to maximize fitness.
#'   When this is `NULL` (default), then a [`SelectorBest`] us used.\cr
#'   The [`Selector`] must be primed on a superset of `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization. All components on which `selector` is primed on must occur in the archive.\cr
#'   The given [`Selector`] *may* return duplicates.
#' @param mutator ([`Mutator` | `NULL`])\cr
#'   [`Mutator`] operation to apply to individuals selected out of `inst` using `parent_selector`.\cr
#'   The [`Mutator`] must be primed on a [`ParamSet`][paradox::ParamSet] similar to `inst$search_space`,
#'   but *without* the "budget" component when `budget_id` is given (multi-fidelity optimization). Such a
#'   [`ParamSet`][paradox::ParamSet] can be generated for example using [`mies_prime_operators`].\cr
#'   When this is `NULL` (default), then a [`MutatorNull`] is used, effectively disabling mutation.
#' @param recombinator ([`Recombinator` | `NULL`])\cr
#'   [`Recombinator`] operation to apply to individuals selected out of `int` using `parent_selector` after mutation using `mutator`.
#'   The [`Recombinator`] must be primed on a [`ParamSet`][paradox::ParamSet] similar to `inst$search_space`,
#'   but *without* the "budget" component when `budget_id` is given (multi-fidelity optimization). Such a
#'   [`ParamSet`][paradox::ParamSet] can be generated for example using [`mies_prime_operators`].\cr
#'   When this is `NULL` (default), then a [`RecombinatorNull`] is used, effectively disabling recombination.
#' @param budget_id (`character(1)` | `NULL`)\cr
#'   Budget compnent when doing multi-fidelity optimization. This component of the search space is removed from
#'   individuals sampled from the archive in `inst` before giving it to `mutator` and `recombinator`.
#'   Should be `NULL` when not doing multi-fidelity.
#' @return [`data.table`][data.table::data.table]: A table of configurations proposed as offspring to be evaluated
#' using [`mies_evaluate_offspring()`].
#' @family mies building blocks
#' @export
mies_generate_offspring = function(inst, lambda, parent_selector = NULL, mutator = NULL, recombinator = NULL, budget_id = NULL) {
  assert_r6(inst, "OptimInstance")

  assert_int(lambda, lower = 1, tol = 1e-100)
  assert_r6(parent_selector, "Selector", null.ok = TRUE)
  if (!is.null(parent_selector)) assert_true(parent_selector$is_primed)
  assert_r6(mutator, "Mutator", null.ok = TRUE)
  if (!is.null(mutator)) assert_true(mutator$is_primed)
  assert_r6(recombinator, "Recombinator", null.ok = TRUE)
  if (!is.null(recombinator)) assert_true(recombinator$is_primed)

  data = inst$archive$data
  assert_integerish(data$dob, lower = 0, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 0, upper = inst$archive$n_batch, tol = 1e-100)

  ss_ids = inst$search_space$ids()
  assert_choice(budget_id, ss_ids, null.ok = TRUE)

  if (is.null(parent_selector)) {
    # need to find out what to prime parent_selector with: If mutator or recombinator exist,
    # then we use their SS and add budget_id, if necessary. This correctly infers additional_components.
    # Only if no operator at all was given do we default to inst$search_space
    if (!is.null(mutator)) {
      ps_ps = mutator$primed_ps
    } else if (!is.null(recombinator)) {
      ps_ps = recombinator$primed_ps
    } else {
      ps_ps = inst$search_space
    }
    if (!is.null(budget_id) && budget_id %nin% ps_ps$ids()) {
      ps_ps = ps_ps$clone()$add(inst$search_space$params[[budget_id]])
    }
    parent_selector = SelectorBest$new()$prime(ps_ps)
  }

  ps_ids = parent_selector$primed_ps$ids()

  assert_subset(ss_ids, ps_ids)
  assert_subset(ps_ids, colnames(data))
  if (is.null(mutator) || is.null(recombinator)) {
    selector_space = parent_selector$primed_ps
    if (!is.null(budget_id)) {
      selector_space = ParamSetShadow$new(selector_space, budget_id)
    }
    mutator = mutator %??% MutatorNull$new()$prime(selector_space)
    recombinator = recombinator %??% RecombinatorNull$new()$prime(selector_space)
  }

  assert_set_equal(mutator$primed_ps$ids(), setdiff(ps_ids, budget_id))
  assert_set_equal(recombinator$primed_ps$ids(), setdiff(ps_ids, budget_id))

  needed_recombinations = ceiling(lambda / recombinator$n_indivs_out)
  needed_parents = needed_recombinations * recombinator$n_indivs_in

  parents = mies_select_from_archive(inst, needed_parents, which(is.na(data$eol)), parent_selector)
  if (!is.null(budget_id)) parents[, (budget_id) := NULL]

  recombined = recombinator$operate(parents[sample.int(nrow(parents))])
  recombined = first(recombined, lambda)  # throw away things if we have too many (happens when n_indivs_out is not a divider of lambda)

  mutator$operate(recombined)
}

