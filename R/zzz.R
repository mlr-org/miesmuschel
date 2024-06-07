#' @import mlr3misc
#' @import checkmate
#' @import bbotk
#' @import paradox
#' @import R6
#' @import data.table
#'
#' @description
#'
#' `miesmuschel` offers both an `Optimizer` and a `Tuner` for general
#' MIES-optimization, as well as all the building blocks for building a custom optimization algorithm that
#' is more flexible and can be used for research into novel evolution strategies.
#'
#' The call-graph of the default algorithm in [`OptimizerMies`] / [`TunerMies`] is as follows, and is shown
#' here as an overview over the `mies_*` functions, and how they are usually connected. (Note that only the
#' exported `mies_*` functions are shown.) See the help information of these functions for more info.
#' ```
#' OptimizerMies$.optimize(inst)
#' |- mies_prime_operators()  # prime operators on instance's search_space
#' |- mies_init_population()  # sample and evaluate first generation
#' |  `- mies_evaluate_offspring()  # evaluate sampled individuals
#' |     `- inst$eval_batch()  # The OptimInst's evaluation method
#' `- repeat # Repeat the following until terminated
#'    |- mies_step_fidelity()  # Evaluate individuals with changing fidelity
#'    |  `- inst$eval_batch()  # The OptimInst's evaluation method
#'    |- mies_generate_offspring()  # Sample parents, recombine, mutate
#'    |  `- mies_select_from_archive()  # Use 'Selector' on 'Archive'
#'    |     `- mies_get_fitnesses()  # Get objective values as fitness matrix
#'    |- mies_evaluate_offspring()  # evaluate sampled individuals
#'    |  `- inst$eval_batch()  # The OptimInst's evaluation method
#'    `- mies_survival_plus() / mies_survival_comma()  # survival
#'       `- mies_select_from_archive()  # Use 'Selector' on 'Archive'
#' ```
"_PACKAGE"

lg = NULL
paradox_context_available = FALSE
paradox_s3 = FALSE

# make these point to whatever bbotk happens to name its classes today
Optimizer = NULL
TunerFromOptimizer = NULL

reg_bbotk = function(...) {  # nocov start
  mlr_optimizers = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  mlr_optimizers$add("mies", OptimizerMies)

  mlr_terminators = utils::getFromNamespace("mlr_terminators", ns = "bbotk")
  mlr_terminators$add("gens", TerminatorGenerations)
  mlr_terminators$add("genstag", TerminatorGenerationStagnation)
  mlr_terminators$add("genperfreached", TerminatorGenerationPerfReached)
  mlr_terminators$add("budget", TerminatorBudget)
}  # nocov end

reg_mlr3tuning = function(...) {  # nocov start
  if (requireNamespace("mlr3tuning", quietly = TRUE)) {
    mlr_tuners = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
    mlr_tuners$add("mies", TunerMies)
  }
}  # nocov end

.onLoad = function(libname, pkgname) {  # nocov start
  reg_bbotk()
  reg_mlr3tuning()


  if (!is.null(paradox::ps()$context_available)) { # use paradox context if variable
    # packageStartupMessage("Using context sensitive configuration parameters")
    assign("paradox_context_available", TRUE, envir = parent.env(environment()))
  }
  if (!"set_id" %in% names(ps())) {
    assign("paradox_s3", TRUE, envir = parent.env(environment()))
  }
  if (exists("OptimizerBatch", envir = asNamespace("bbotk"))) {
    assign("Optimizer", get("OptimizerBatch", envir = asNamespace("bbotk")), envir = parent.env(environment()))
  } else {
    assign("Optimizer", get("Optimizer", envir = asNamespace("bbotk")), envir = parent.env(environment()))
  }
  makeActiveBinding("TunerFromOptimizer", env = parent.env(environment()), fun = function() {
    if (exists("TunerBatchFromOptimizerBatch", envir = asNamespace("bbotk"))) {
      get("TunerBatchFromOptimizerBatch", envir = asNamespace("mlr3tuning"))
    } else {
      get("TunerFromOptimizer", envir = asNamespace("mlr3tuning"))
    }
  }

  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  setHook(packageEvent("bbotk", "onLoad"), reg_bbotk, action = "append")
  setHook(packageEvent("mlr3tuning", "onLoad"), reg_mlr3tuning, action = "append")
  options(miesmuschel.testing = getOption("miesmuschel.testing") %??% FALSE)
}  # nocov end

.onUnload = function(libpath) {  # nocov start
  for (pkg in c("bbotk", "mlr3tuning")) {
    load_event = packageEvent(pkg, "onLoad")
    setHook(load_event,
      keep(getHook(load_event), function(x) environmentName(topenv(environment(x))) != "miesmuschel"),
      action = "replace"
    )
  }
}  # nocov end


# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("dob", "eol", "."))

if (!Sys.getenv("DEVTOOLS_LOAD") == "true") {
  leanify_package()
}

