#' @title Mixed Integer Evolutionary Strategies Optimizer
#'
#' @include Selector.R
#' @include Mutator.R
#' @include Recombinator.R
#' @include mies_methods.R
#'
#' @description
#' Perform optimization using evolutionary strategies. `OptimizerMies` and `TunerMies` implement a standard ES optimization
#' algorithm, performing initialization first, followed by a loop of performance evaluation, survival selection, parent selection, mutation, and
#' recombination. Currently two different survival modes ("comma" and "plus") are supported. Multi-fidelity optimization, similar
#' to the "rolling-tide" algorithm described in `r cite_bib("fieldsend2014rolling")`. The modular design and reliance on
#' [`MiesOperator`] objects to perform central parts of the optimization algorithm makes this [`Optimizer`][bbotk::Optimizer]
#' highly flexible and configurable. In combination with [`OperatorCombination`] mutators and recombinators, an algorithm
#' as presented in `r cite_bib("li2013mixed")` can easily be implemented.
#'
#' `OptimizerMies` implements a standard evolutionary strategies loop:
#' 1. Prime operators, using `mies_prime_operators()`
#' 2. Initialize population, using `mies_init_population()`
#' 3. Generate offspring by selecting parents, recombining and mutating them, using `mies_generate_offspring()`
#' 4. Evaluate performance, using `mies_evaluate_offspring()`
#' 5. Select survivors, using either `mies_survival_plus()` or `mies_survival_comma()`, depending on the `survival_strategy` hyperparameter
#' 6. Optionally, evaluate survivors with higher fidelity if the multi-fidelity functionality is being used
#' 7. Jump to 3.
#'
#' @section Multi-Fidelity:
#' `miesmuschel` provides a simple multi-fidelity optimization mechanism that both allows increasing fidelity by generation
#' number, and
#'
#' @references
#' `r format_bib("fieldsend2014rolling")`
#'
#' `r format_bib("li2013mixed")`
#'
#' @export
OptimizerMies = R6Class("OptimizerMies", inherit = Optimizer,
  public = list(
      initialize = function(mutator = MutatorProxy$new(), recombinator = RecombinatorProxy$new(), parent_selector = SelectorProxy$new(),
        survival_selector = SelectorProxy$new(), elite_selector = NULL, multi_fidelity = FALSE) {
      private$.mutator = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.recombinator = assert_r6(recombinator, "Recombinator")$clone(deep = TRUE)
      private$.parent_selector = assert_r6(parent_selector, "Selector")$clone(deep = TRUE)
      private$.survival_selector = assert_r6(survival_selector, "Selector")$clone(deep = TRUE)

      assert_r6(elite_selector, "Selector", null.ok = TRUE)
      if (!is.null(elite_selector)) {
        private$.elite_selector = elite_selector$clone(deep = TRUE)
      }
      private$.own_param_set = do.call(ps, c(list(
          lambda = p_int(1, tags = c("required", "offspring")),
          mu = p_int(1, tags = c("required", "init", "survival")),
          initializer = p_uty(custom_check = function(x) check_function(x, nargs = 2), default = generate_design_random, tags = "init"),  # arguments: param_set, n
          survival_strategy = p_fct(c("plus", if (!is.null(elite_selector)) "comma"), tags = "required"),
          additional_component_sampler = p_uty(custom_check = function(x) check_r6(x, "Sampler"))),
        if (multi_fidelity) list(
          fidelity_schedule = p_uty(custom_check = check_fidelity_schedule, tags = "required"),
          fidelity_generation_lookahead = p_lgl(tags = "required"),
          fidelity_current_gen_only = p_lgl(tags = "required"),
          fidelity_monotonic = p_lgl(tags = "required")),
        if (!is.null(elite_selector)) list(
          n_elite = p_int(0, depends = survival_strategy == "comma", tags = "survival"))
      ))
      private$.param_set_source = c(alist(
          private$.own_param_set,
          mutator = self$mutator$param_set,
          recombinator = self$recombinator$param_set,
          parent_selector = self$parent_selector$param_set,
          survival_selector = self$survival_selector$param_set),
        if (!is.null(elite_selector)) alist(
          elite_selector = self$elite_selector$param_set)
      )

      self$param_set$values = c(
        list(lambda = 10, mu = 1, survival_strategy = "plus"),
        if (multi_fidelity) list(
          fidelity_schedule = data.table(generation = 1, budget_new = 1, budget_survivors = 1),
          fidelity_generation_lookahead = TRUE, fidelity_current_gen_only = FALSE, fidelity_monotonic = TRUE)
      )

      param_class_determinants = c(
        list(parent_selector, survival_selector),
        if (!is.null(elite_selector)) list(elite_selector),

        # don't depend on mutate and recombine when we do multi-fidelity because budget may be any unsupported Param.
        if (!multi_fidelity) list(mutator, recombinator)
      )

      properties_determinants = discard(list(parent_selector, survival_selector, elite_selector), is.null)

      super$initialize(
        param_set = self$param_set,  # essentially a nop, since at this point we already set private$.param_set, but we can't give NULL here.
        param_classes = Reduce(intersect, map(param_class_determinants, "param_classes")),
        properties = c("dependencies", Reduce(intersect, map(properties_determinants, "supported")))
      )
    }
  ),
  active = list(
    mutator = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.mutator)) {
        stop("mutator is read-only.")
      }
      private$.mutator
    },
    recombinator = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.recombinator)) {
        stop("recombinator is read-only.")
      }
      private$.recombinator
    },
    parent_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.parent_selector)) {
        stop("parent_selector is read-only.")
      }
      private$.parent_selector
    },
    survival_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.survival_selector)) {
        stop("survival_selector is read-only.")
      }
      private$.survival_selector
    },
    elite_selector = function(rhs) {
      if (!missing(rhs) && !identical(rhs, private$.elite_selector)) {
        stop("elite_selector is read-only.")
      }
      private$.elite_selector
    },
    param_set = function(rhs) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        private$.param_set = ParamSetCollection$new(sourcelist)
        if (!is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        private$.param_set_id = private$.param_set$set_id
        private$.param_set = NULL  # required to keep clone identical to original, otherwise tests get really ugly
        if (name == ".param_set_source") {
          value = lapply(value, function(x) {
            if (inherits(x, "R6")) x$clone(deep = TRUE) else x
          })
        }
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .optimize = function(inst) {
      params = private$.own_param_set$get_values()

      budget_id = NULL
      if (!is.null(params$fidelity_schedule)) {
        budget_id = search_space$ids(tags = "budget")
        if (length(budget_id) != 1) stopf("Need exactly one budget parameter for multifidelity, but found %s: %s",
          length(budget_id), str_collapse(budget_id))
      }

      mies_prime_operators(mutators = list(self$mutator), recombinators = list(self$recombinator),
        selectors = discard(list(self$survival_selector, self$parent_selector, self$elite_selector), is.null),
        search_space = inst$search_space, additional_components = params$additional_component_sampler$param_set,
        budget_id = budget_id)

      mies_init_population(inst, mu = params$mu, initializer = params$initializer, fidelity_schedule = params$fidelity_schedule,
        budget_id = budget_id, additional_component_sampler = params$additional_component_sampler)

      survival = switch(params$survival_strategy,
          plus = mies_survival_plus,
          comma = mies_survival_comma)

      repeat {
        mies_step_fidelity(inst, params$fidelity_schedule, budget_id, generation_lookahead = params$fidelity_generation_lookahead,
          current_gen_only = params$fidelity_current_gen_only, monotonic = params$fidelity_monotonic)
        offspring = mies_generate_offspring(inst, lambda = params$lambda,
          parent_selector = self$parent_selector, mutator = self$mutator, recombinator = self$recombinator, budget_id = budget_id)
        mies_evaluate_offspring(inst, offspring = offspring, fidelity_schedule = params$fidelity_schedule, budget_id = budget_id)
        survival(inst, mu = params$mu, survival_selector = self$survival_selector, n_elite = params$n_elite, elite_selector = self$elite_selector)
      }
    },
    .mutator = NULL,
    .recombinator = NULL,
    .parent_selector = NULL,
    .survival_selector = NULL,
    .elite_selector = NULL,
    .own_param_set = NULL,
    .param_set_id = NULL,
    .param_set_sources = NULL
  )
)

check_fidelity_schedule = function(x) {
  if (test_data_frame(x, ncols = 2, min.rows = 1) &&
      test_names(colnames(x), identical.to = c("generation", "budget_new", "budget_survivors")) &&
      test_integerish(x$generation, tol = 1e-100, any.missing = FALSE, unique = TRUE) &&
      1 %in% x$generation) {
    TRUE
  } else {
    "must be a data.table with integer column 'generation' (with unique non-missing values and at least one row with value 1) and columns 'budget_new', 'budget_survivors'."
  }
}

