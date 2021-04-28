library(bbotk) # @irace
library(R6)
library(miesmuschel)
library(mfsurrogates)
library(data.table)
library(mlr3learners)
library(mlr3misc)

# lcbench
workdir = "./attic/data/"
cfg = cfgs("lcbench", workdir = workdir)
cfg$setup()

# sumo hyperband configuration parameter search space
meta_search_space = ps(
  budget_log_step = p_dbl(log(2) / 4, log(2) * 4, logscale = TRUE),
  mu = p_int(2, 200, logscale = TRUE),
  sample = p_fct(c("random")),  
  survival_fraction = p_dbl(0, 1), 
  filter_algorithm = p_fct(c("tournament", "progressive")),
  surrogate_learner = p_fct(list(
    ranger = mlr3::lrn("regr.ranger"),
    knn = mlr3::lrn("regr.kknn", fallback = mlr3::lrn("regr.featureless"), encapsulate = c(train = "evaluate", predict = "evaluate")))),  
  filter_with_max_budget = p_lgl(),
  filter_factor_first = p_dbl(1, 100, logscale = TRUE),
  filter_factor_last = p_dbl(1, 100, logscale = TRUE),
  filter_select_per_tournament = p_int(1, 10, logscale = TRUE),
  random_interleave_fraction = p_dbl(0, 1),
  random_interleave_random = p_lgl()
)

meta_domain = ps(
  budget_log_step = p_dbl(log(2) / 4, log(2) * 4),
  mu = p_int(2, 200),
  sample = p_fct(c("random")),  
  survival_fraction = p_dbl(0, 1), 
  filter_algorithm = p_fct(c("tournament", "progressive")),
  surrogate_learner = p_uty(custom_check = function(x) {x$id %in% c("regr.ranger", "regr.kknn")}),  
  filter_with_max_budget = p_lgl(),
  filter_factor_first = p_dbl(1, 100),
  filter_factor_last = p_dbl(1, 100),
  filter_select_per_tournament = p_int(1, 10),
  random_interleave_fraction = p_dbl(0, 1),
  random_interleave_random = p_lgl()
)

ObjectiveIrace = R6Class("ObjectiveIrace", inherit = bbotk::Objective, 
  public = list(
    irace_instance = NULL,
    check_values = FALSE
  ),

   private = list(
    .eval = function(xs) {
        objective = cfg$get_objective(task = self$irace_instance)

        search_space = objective$domain$search_space(list(
          batch_size = to_tune(),
          learning_rate = to_tune(),
          momentum = to_tune(),
          weight_decay = to_tune(),
          num_layers = to_tune(),
          max_units = to_tune(),
          max_dropout = to_tune()
        ))

        search_space$add(ParamDbl$new("epoch", 1, log(52), tags = "budget"))

        search_space$trafo = function(x, param_set) {
          x$batch_size = as.integer(round(exp(x$batch_size)))
          x$learning_rate = exp(x$learning_rate)
          x$max_units = as.integer(round(exp(x$max_units)))
          x$epoch = as.integer(exp(x$epoch))
          x
        }

        budget_param = search_space$ids(tags = "budget")
        fidelity_steps = floor((search_space$upper[budget_param] - search_space$lower[budget_param]) / xs$budget_log_step)
        search_space$params[[budget_param]]$lower = search_space$upper[budget_param] - fidelity_steps * xs$budget_log_step

        survivors = max(round(xs$survival_fraction * xs$mu), 1)
        lambda = xs$mu - survivors
        if (lambda < 1) return(list(y = 0))

        oi = OptimInstanceMultiCrit$new(objective, search_space,
          terminator = trm("budget", budget = 2^13, aggregate = function(x) sum(exp(as.numeric(x)))),   # budget in archive is in log-scale!
          check_values = FALSE
        )

        # scalor: scalarizes multi-objective results. "one": take the single objective. "nondom": nondominated sorting w/ crowding distance tie breaker
        scalor = if (self$codomain$length == 1) scl("one") else scl("nondom")
        # selector: take the best, according to scalarized objective
        selector = sel("best", scalor)
        # filtor: use surtour or surprog, depending on filter_algorithm config argument
        filtor = switch(xs$filter_algorithm,
          tournament = ftr("surtour", surrogate_learner = xs$surrogate_learner, surrogate_selector = selector,
            filter.per_tournament = xs$filter_select_per_tournament,
            filter.tournament_size = xs$filter_factor_first * xs$filter_select_per_tournament,
            filter.tournament_size_last = xs$filter_factor_last * xs$filter_select_per_tournament
          ),
          progressive = ftr("surprog", surrogate_learner = xs$surrogate_learner, surrogate_selector = selector,
            filter.pool_factor = xs$filter_factor_first,
            filter.pool_factor_last = xs$filter_factor_last
          )
        )

        interleaving_filtor = ftr("maybe", filtor, p = xs$random_interleave_fraction, random_choice = xs$random_interleave_random)

        sampling_fun = switch(xs$sample, random = paradox::generate_design_random, lhs = paradox::generate_design_lhs)

        optimizer = opt("sumohb", filtor = interleaving_filtor, selector = selector,
          mu = xs$mu, survival_fraction = xs$survival_fraction, sampling = sampling_fun,
          fidelity_steps = fidelity_steps + 1, filter_with_max_budget = xs$filter_with_max_budget
        )

        optimizer$optimize(oi)
        list(y = sum(pmap_dbl(oi$result_y, function(val_accuracy, val_cross_entropy, val_balanced_accuracy, test_cross_entropy, test_balanced_accuracy, time) {
          val_accuracy + val_cross_entropy + val_balanced_accuracy + test_cross_entropy + test_balanced_accuracy + 1 - time 
        }))/nrow(oi$result_y))
    }
  )
)

meta_codomain = ps(y = p_dbl(0, 6, tags = "maximize"))

irace_objective = ObjectiveIrace$new(domain = meta_domain, codomain = meta_codomain)
irace_instances = cfg$param_set$params$OpenML_task_id$levels
irace_instance = OptimInstanceSingleCrit$new(objective = irace_objective, search_space = meta_search_space, terminator = trm("evals", n_evals = 300))

optimizer_irace = opt("irace", instances = irace_instances)
optimizer_irace$optimize(irace_instance)
