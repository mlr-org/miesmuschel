


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
        mu = p_int(1, tags = c("required", "init")),
        initializer = p_uty(custom_check = function(x) check_function(x, nargs = 2), default = generate_design_random, tags = "init"),  # arguments: param_set, n
        mutator = p_uty(tags = "offspring", default = RecombinatorNull$new()),  # TODO custom check ...
        recombinator = p_uty(tags = "offspring", default = MutatorNull$new()),
        parent_selector = p_uty(tags = "offspring", default = SelectorBest$new()),
        survival_selector = p_uty(tags = "required"),
        survival_strategy = p_fct(c("plus", "comma"), tags = "required"),
        n_elite = p_int(0, requires = survival_strategy == "comma"),
        elite_selector = p_uty(requires = survival_strategy == "comma"),
        fidelity_schedule = p_uty()
      )
      param_set$values = list(lambda = 10, mu = 1, mutator = MutatorGauss$new(),
        recombinator = RecombinatorCrossoverUniform$new(FALSE), parent_selector = SelectorRandom$new(),
        survival_selector = SelectorBest$new(), survival_strategy = "plus")
      super$initialize(
        param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), # TODO: do we have a list?
        properties = c("dependencies", "single-crit", "multi-crit")
      )
    }
  ),

  private = list(
    .optimize = function(inst) {
      params = self$param_set$get_values()
      if (params$survival_strategy == "comma") {
        if (params$mu <= params$n_elite) stop("Mu must be > n_elite")
      }
      invoke(MiesInitPopulation, inst, .args = self$param_set$get_values(tags = "init"))

      repeat {
        offspring = invoke(MiesGenerateOffspring, inst, .args = self$param_set$get_values(tags = "offspring"))
        inst$eval_batch(rbindlist(offspring))
        offspring = which(inst$archive$data()$batch_nr == inst$archive$n_batch)
        if (params$survival_strategy == "plus") {
          inst$archive$alive = c(inst$archive$alive, offspring)
          inst$archive$alive = MiesSelectFromArchive(inst, params$lambda, inst$archive$alive, params$survival_selector, get_indivs = FALSE)
        } else if (params$survival_strategy == "comma") {
          inst$archive$alive = c(
            MiesSelectFromArchive(inst, params$n_elite, inst$archive$alive, params$elite_selector, get_indivs = FALSE),
            MiesSelectFromArchive(inst, params$mu - params$n_elite, offspring, params$survival_selector, get_indivs = FALSE)
          )
        }
      }
    }
  )
)

MiesInitPopulation = function(inst, mu, initializer = generate_design_random) {
  assert_int(mu, lower = 1)
  assert_function(initializer, nargs = 2)
  mu_remaining = mu - length(inst$archive$alive)
  if (mu_remaining < 0) {
    inst$archive = tail(inst$archive, mu)  # TODO: maybe handle this differently
  } else if (mu_remaining > 0) {
    inst$eval_batch(initializer(inst$search_space, mu_remaining)$data)
    inst$archive$alive = c(inst$archive$alive, which(inst$archive$data()$batch_nr == inst$archive$n_batch))
  }
  inst
}

GetFitnesses = function(inst, rows) {
  multiplier = map_dbl(inst$archive$codomain$tags, function(x) switch(x, minimize = -1, maximize = 1, 0))
  fitnesses = as.matrix(inst$archive$data()[rows, multiplier != 0, with = FALSE])
  multiplier = multiplier[multiplier != 0]
  sweep(fitnesses, 2L, multiplier, `*`)
}

MiesSelectFromArchive = function(inst, n_select, rows, selector = SelectorBest$new(), get_indivs = TRUE) {
  assert_r6(selector, "Selector")
  assert_int(n_select, lower = 0, tol = 1e-100)
  if (n_select == 0) if (get_indivs) return(list()) else return(integer(0))
  indivs = transpose_list(inst$archive$data()[rows, inst$archive$cols_x, with = FALSE])

  fitnesses = GetFitnesses(inst, rows)

  selected = selector$select(indivs, inst$search_space, fitnesses, n_select)
  if (get_indivs) {
    indivs[selected]
  } else {
    rows[selected]
  }
}

MiesGenerateOffspring = function(inst, lambda, parent_selector = SelectorBest$new(), mutator = MutatorNull$new(), recombinator = RecombinatorNull$new()) {
  assert_int(lambda, lower = 1, tol = 1e-100)
  assert_r6(mutator, "Mutator")
  assert_r6(recombinator, "Mutator")
  needed_recombinations = ceiling(lambda / recombinator$n_indivs_out)
  needed_parents = needed_recombinations * recombinator$n_indivs_in

  parents = MiesSelectFromArchive(inst, needed_parents, rows, parent_selector)

  mating_groups = split(parents[sample.int(length(parents))],
    rep_len(seq_len(needed_recombinations), length(parents)))

  recombined = lapply(mating_groups, function(values) recombinator$recombine(values, inst$search_space))
  recombined = recombined[seq_len(lambda)]  # throw away things if we have too many (happens when n_indivs_out is not a divider of lambda)
  recombined = unlist(recombined, recursive = FALSE, use.names = FALSE)

  lapply(recombined, function(value) mutator$mutate(value, inst$search_space))
}

