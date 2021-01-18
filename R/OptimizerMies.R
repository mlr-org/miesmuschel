


#' @title OptimizerMies
#'
#' @description
#' Mixed Integer Evolutionary Strategies
#' @export
OptimizerMies = R6Class("OptimizerMies", inherit = Optimizer,
  public = list(
    initialize = function() {
      param_set = ps(
        lambda = p_int(1, tags = c("required", "offspring")),
        mu = p_int(1, tags = c("required", "init", "survival")),
        initializer = p_uty(custom_check = function(x) check_function(x, nargs = 2), default = generate_design_random, tags = "init"),  # arguments: param_set, n
        mutator = p_uty(tags = "offspring", default = RecombinatorNull$new()),  # TODO custom check ...
        recombinator = p_uty(tags = "offspring", default = MutatorNull$new()),
        parent_selector = p_uty(tags = "offspring", default = SelectorBest$new()),
        survival_strategy = p_fct(c("plus", "comma"), tags = "required"),
        survival_selector = p_uty(tags = "required", tags = "survival"),
        n_elite = p_int(0, requires = survival_strategy == "comma", tags = "survival"),
        elite_selector = p_uty(requires = survival_strategy == "comma", tags = "survival"),
        fidelity_schedule = p_uty(custom_check = check_fidelity_schedule),
        reeval_fidelity_steps = p_lgl()
      )
      param_set$values = list(lambda = 10, mu = 1, mutator = MutatorGauss$new(),
        recombinator = RecombinatorCrossoverUniform$new(FALSE), parent_selector = SelectorRandom$new(),
        survival_selector = SelectorBest$new(), survival_strategy = "plus")
      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"),
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      params = self$param_set$get_values()
      search_space = inst$search_space
      fidelity_schedule = NULL
      if (!is.null(params$fidelity_schedule)) {
        budget_id = search_space$ids(tags = "budget")
        if (length(budget_id) != 1) stopf("Only allowing one budget parameter, but found %s: %s",
          length(budget_id), str_collapse(budget_id))
        search_space = ParamSetShadow$new(search_space, budget_id)
      }
      params = lapply(params, function(x) if (inherits(x, "Operator")) x$clone(deep = TRUE)$prime(search_space) else x)
      invoke(mies_init_population, inst, .args = self$param_set$get_values(tags = "init"))

      repeat {
        offspring = invoke(mies_generate_offspring, inst, .args = self$param_set$get_values(tags = "offspring"))
        mies_evaluate_offspring(inst, offspring, fidelity_schedule, budget_id)
        invoke(switch(params$survival_strategy,
            plus = mies_survival_plus,
            comma = mies_survival_comma),
          inst, .args = self$param_set$get_values(tags = "survival"))
        mies_step_fidelity(inst, fidelity_schedule, budget_id, only_latest_gen = !params$reeval_fidelity_steps)
      }
    }
  )
)

check_fidelity_schedule = function(x) {
  if (test_data_table(x, ncols = 2, min.rows = 1) &&
      test_names(colnames(x), identical.to = c("generation", "budget_new", "budget_survivors")) &&
      test_integerish(x$generation, tol = 1e-100, any.missing = FALSE, unique = TRUE) &&
      1 %in% x$generation) {
    TRUE
  } else {
    "must be a data.table with integer column 'generation' (with unique non-missing values and at least one row with value 1) and columns 'budget_new', 'budget_survivors'."
  }
}

#' @export
mies_evaluate_offspring = function(inst, offspring, fidelity_schedule = NULL, budget_id = NULL, survivor_budget = FALSE) {
  assert_data_table(offspring)
  assert_choice(budget_id, inst$search_space$ids(), null.ok = is.null(fidelity_schedule))
  assert_flag(survivor_budget)
  generation = max(inst$archive$data$dob, 0) + 1
  if (!is.null(fidelity_schedule)) {
    assert(check_fidelity_schedule(fidelity_schedule))
    fidelity_schedule = setkeyv(copy(fidelity_schedule), "generation")
    fidelity_column = if (survivor_budget) "budget_survivors" else "budget_new"
    fidelity = fidelity_schedule[data.table(generation = generation), fidelity_column, on = generation, roll = TRUE, with = FALSE]
    setnames(fidelity, budget_id)
    offspring = cbind(offspring, fidelity)
  }
  inst$eval_batch(cbind(offspring, dob = ..(generation), eol = NA_real_))
}

#' @export
mies_step_fidelity = function(inst, fidelity_schedule, budget_id, only_latest_gen = FALSE) {
  assert_flag(only_latest_gen)
  data = inst$archive$data
  generation = max(data$dob, 0)
  assert_choice(budget_id, inst$search_space$ids())
  assert(check_fidelity_schedule(fidelity_schedule))
  assert_integerish(data$dob, lower = 1, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 1, upper = inst$archive$n_batch, tol = 1e-100)

  if (!any(is.na(data$eol))) stop("No alive individuals. Need to run mies_init_population()?")

  next_fidelity = fidelity_schedule[data.table(generation = generation + 1), "budget_survivors", on = generation, roll = TRUE, with = FALSE]

  reeval = which((if (only_latest_gen) data$dob == generation else TRUE) & is.na(data$eol) & data[[budget_id]] != next_fidelity)

  data[reeval, eol := ..(generation)]
  indivs = data[reeval, inst$search_space$ids(), with = FALSE]
  indivs[, `:=`(budget_id, next_fidelity)]
  inst$eval_batch(indivs[, `:=`(dob = ..(generation), eol = NA_real_)])
}


#' @export
mies_survival_plus = function(inst, mu, survival_selector, ...) {
  data = inst$archive$data
  assert_integerish(data$dob, lower = 1, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 1, upper = inst$archive$n_batch, tol = 1e-100)
  alive = which(is.na(data$eol))
  if (!length(alive)) stop("No alive individuals. Need to run mies_init_population()?")
  survivors = mies_select_from_archive(inst, mu, alive, survival_selector, get_indivs = FALSE)
  died = setdiff(alive, survivors)
  data[died, eol := max(dob)]
}

#' @export
mies_survival_comma = function(inst, mu, survival_selector, n_elite, elite_selector, ...) {
  data = inst$archive$data
  assert_integerish(data$dob, lower = 1, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 1, upper = inst$archive$n_batch, tol = 1e-100)
  alive_before = data[, which(is.na(eol) & dob != max(dob))]
  current_offspring = data[, which(is.na(eol) & dob == max(dob))]

  if (!length(alive_before)) stop("No alive individuals. Need to run mies_init_population()?")
  if (!length(current_offspring)) stop("No current offspring. Need to run mies_evaluate_offspring()?")
  if (mu <= n_elite) stop("Mu must be > n_elite")

  survivors = c(
    mies_select_from_archive(inst, n_elite, alive_before, elite_selector, get_indivs = FALSE),
    mies_select_from_archive(inst, mu - n_elite, current_offspring, survival_selector, get_indivs = FALSE)
  )
  died = setdiff(alive, survivors)
  data[died, eol := max(dob)]
}

#' @export
mies_init_population = function(inst, mu, initializer = generate_design_random, fidelity_schedule = NULL, budget_id = NULL) {
  assert_int(mu, lower = 1)
  assert_function(initializer, nargs = 2)

  assert_choice(budget_id, inst$search_space$ids(), null.ok = is.null(fidelity_schedule))
  if (!is.null(fidelity_schedule)) {
    assert(check_fidelity_schedule(fidelity_schedule))
  }

  if (any(c("dob", "eol") %in% inst$search_space$ids())) {
    stop("'dob' and 'eol' may not be search space dimensions.")
  }
  dob = batch_nr = NULL
  if ("eol" %nin% colnames(inst$archive$data)) {
    inst$archive$data[, eol := 0]
  } else if ("dob" %nin% colnames(inst$archive$data)) {
    stop("'eol' but not 'dob' column found in archive; this is an undefined state that would probably lead to bad behaviour, so stopping.")
  }

  if ("dob" %nin% colnames(inst$archive$data)) {
    inst$archive$data[, dob := 0]
  }
  data = inst$archive$data  # adding columns is not guaranteed to happen by reference, so we only get the local copy here!
  assert_integerish(data$dob, lower = 1, upper = inst$archive$n_batch, any.missing = FALSE, tol = 1e-100)
  assert_integerish(data$eol, lower = 1, upper = inst$archive$n_batch, tol = 1e-100)

  mu_remaining = mu - sum(is.na(inst$archive$eol))

  if (mu_remaining < 0) {
    # TODO: we are currently killing the earliest evals, maybe do something smarter.
    # TODO: also we are not doing anything about budget here, we just hope the user knows what he's doing.
    data[head(which(is.na(eol)), -mu_remaining), eol := max(dob)]
  } else if (mu_remaining > 0) {
    mies_evaluate_offspring(inst, remove_named(initializer(inst$search_space, mu_remaining)$data, budget_id), fidelity_schedule, budget_id, survivor_budget = TRUE)
  }
  inst
}

#' @export
mies_get_fitnesses = function(inst, rows) {
  multiplier = map_dbl(inst$archive$codomain$tags, function(x) switch(x, minimize = -1, maximize = 1, 0))
  fitnesses = as.matrix(inst$archive$data[rows, ..(multiplier) != 0, with = FALSE])
  multiplier = multiplier[multiplier != 0]
  sweep(fitnesses, 2L, multiplier, `*`)
}

#' @export
mies_select_from_archive = function(inst, n_select, rows, selector = SelectorBest$new(), get_indivs = TRUE) {
  assert_r6(selector, "Selector")
  assert_int(n_select, lower = 0, tol = 1e-100)
  if (n_select == 0) if (get_indivs) return(inst$data[0]) else return(integer(0))
  indivs = inst$archive$data[rows, inst$archive$cols_x, with = FALSE]

  fitnesses = mies_get_fitnesses(inst, rows)

  selected = selector$operate(indivs, fitnesses, n_select)
  if (get_indivs) {
    indivs[selected]
  } else {
    rows[selected]
  }
}

#' @export
mies_generate_offspring = function(inst, lambda, parent_selector = SelectorBest$new(), mutator = MutatorNull$new(), recombinator = RecombinatorNull$new()) {
  assert_int(lambda, lower = 1, tol = 1e-100)
  assert_r6(mutator, "Mutator")
  assert_r6(recombinator, "Mutator")
  needed_recombinations = ceiling(lambda / recombinator$n_indivs_out)
  needed_parents = needed_recombinations * recombinator$n_indivs_in

  parents = mies_select_from_archive(inst, needed_parents, rows, parent_selector)

  recombined = recombinator$operate(parents[sample.int(nrow(parents))])
  recombined = first(recombined, lambda)  # throw away things if we have too many (happens when n_indivs_out is not a divider of lambda)

  mutator$operate(recombined)
}

