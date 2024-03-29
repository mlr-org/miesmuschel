library(bbotk)
library(R6)
library(miesmuschel) # @multiobjective_hb
library(mfsurrogates)
library(data.table)
library(mlr3learners)
library(checkmate)

source("attic/optim2.R")

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

  filter_factor_first.end = p_dbl(1, 100, logscale = TRUE),
  filter_factor_last.end = p_dbl(1, 100, logscale = TRUE),
  filter_select_per_tournament.end = p_int(1, 10, logscale = TRUE),
  random_interleave_fraction.end = p_dbl(0, 1),

  random_interleave_random = p_lgl()
)

meta_domain = ps(
  budget_log_step = p_dbl(0),
  mu = p_int(2),
  sample = p_fct(c("random", "lhs")),
  survival_fraction = p_dbl(0, 1),
  filter_algorithm = p_fct(c("tournament", "progressive")),
  surrogate_learner = p_uty(custom_check = function(x) check_r6(x, classes = "LearnerRegr")),
  filter_with_max_budget = p_lgl(),
  filter_factor_first = p_dbl(1),
  filter_factor_last = p_dbl(1),
  filter_select_per_tournament = p_int(1),
  random_interleave_fraction = p_dbl(0, 1),

  filter_factor_first.end = p_dbl(1),
  filter_factor_last.end = p_dbl(1),
  filter_select_per_tournament.end = p_int(1),
  random_interleave_fraction.end = p_dbl(0, 1),

  random_interleave_random = p_lgl()
)


# objective_targets: target(s) being optimized by smashy
# test_targets: target(s) to do test evaluation on by smashy
# cfg: ...
makeIraceOI <- function(objective_targets, test_targets, cfg, evals = 300) {
  assert_character(objective_targets, any.missing = FALSE, min.len = 1)
  assert_character(test_targets, any.missing = FALSE, min.len = 1)

  ObjectiveIrace = R6Class("ObjectiveIrace", inherit = bbotk::Objective,
    public = list(
      irace_instance = NULL,
      check_values = FALSE
    ),

     private = list(
      .eval_many = function(xss) {

        eval = function(xs, instance) {
          t0 = Sys.time()
          # CONFIGURE THIS FOR THE OBJECTIVE
          objective = cfg$get_objective(task = instance, target_variables = objective_targets)
          test_objective = cfg$get_objective(task = instance, target_variables = test_targets)
          highest_budget_only = TRUE
          nadir = vapply(objective$codomain$tags, function(x) ifelse("minimize" %in% x, 1, 0), 0)

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

          budget_limit = search_space$length * 100 * 52

          performance <- mlr3misc::invoke(opt_objective_optimizable, objective = objective,
            test_objective = test_objective, budget_limit = budget_limit, search_space = search_space,
            highest_budget_only = highest_budget_only, nadir = nadir, .args = xs)
          time = as.numeric(difftime(Sys.time(), t0, units = "secs"))
          list(y = performance, time = time)
        }
        res = future.apply::future_mapply(eval, xss, self$irace_instance)
        as.data.table(t(res))
      }
    )
  )

  irace_objective = ObjectiveIrace$new(domain = meta_domain, codomain = ps(y = p_dbl(tags = "maximize")))

  OptimInstanceSingleCrit$new(objective = irace_objective, search_space = meta_search_space, terminator = trm("evals", n_evals = evals))
}

optimize_irace <- function(objective_targets, test_targets, instances, cfg, evals = 300, instance_file, log_file) {
  irace_instance = makeIraceOI(objective_targets, test_targets, cfg, evals)
  optimizer_irace = opt("irace", instances = instances, logFile = log_file)
  optimizer_irace$optimize(irace_instance)
  saveRDS(irace_instance, instance_file)
  irace_instance
}

# lcbench
# workdir = "./attic/data/"
# cfg = cfgs("lcbench", workdir = workdir)
# cfg$setup()





