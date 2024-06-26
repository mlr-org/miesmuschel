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

Optimizer_internal = NULL
OptimInstanceSingleCrit_internal = NULL
OptimInstanceMultiCrit_internal = NULL

#' @export
#' @title Optimizer Class
#'
#' @description
#' `bbotk`'s `Optimizer` class.
#' Re-exported since `bbotk` will change the name.
Optimizer = R6::R6Class("Optimizer", inherit = Optimizer_internal)

#' @export
#' @title OptimInstanceSingleCrit Class
#'
#' @description
#' `bbotk`'s `OptimInstanceSingleCrit` class.
#' Re-exported since `bbotk` will change the name.
OptimInstanceSingleCrit = R6::R6Class("OptimInstanceSingleCrit", inherit = OptimInstanceSingleCrit_internal)

#' @export
#' @title OptimInstanceMultiCrit Class
#'
#' @description
#' `bbotk`'s `OptimInstanceMultiCrit` class.
#' Re-exported since `bbotk` will change the name.
OptimInstanceMultiCrit = R6::R6Class("OptimInstanceMultiCrit", inherit = OptimInstanceMultiCrit_internal)

#' @export
#' @title TuningInstanceSingleCrit Class
#'
#' @description
#' `mlr3tuning`'s `TuningInstanceSingleCrit` class.
#' Re-exported since `mlr3tuning` will change the name.
TuningInstanceSingleCrit = R6::R6Class("TuningInstanceSingleCrit", inherit = TuningInstanceSingleCrit_internal)

#' @export
#' @title TuningInstanceMultiCrit Class
#'
#' @description
#' `mlr3tuning`'s `TuningInstanceMultiCrit` class.
#' Re-exported since `mlr3tuning` will change the name.
TuningInstanceMultiCrit = R6::R6Class("TuningInstanceMultiCrit", inherit = TuningInstanceMultiCrit_internal)


# the following actually becomes an active binding, so the mlr3tuning
# package does not get loaded prematurely.
## TunerFromOptimizer = NULL

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
  if ("mlr3tuning" %in% loadedNamespaces()) {
    reg_mlr3tuning()
  }

  if (!is.null(paradox::ps()$context_available)) { # use paradox context if variable
    # packageStartupMessage("Using context sensitive configuration parameters")
    assign("paradox_context_available", TRUE, envir = parent.env(environment()))
  }
  if (!"set_id" %in% names(ps())) {
    assign("paradox_s3", TRUE, envir = parent.env(environment()))
  }
  ## backward compatibility with bbotk
  replacing = c("Optimizer%s", "OptimInstance%sSingleCrit", "OptimInstance%sMultiCrit")
  localnames = paste0(sprintf(replacing, ""), "_internal")
  if (exists("OptimizerBatch", envir = asNamespace("bbotk"))) {
    bbotknames = sprintf(replacing, "Batch")
  } else {
    bbotknames = sprintf(replacing, "")
  }
  for (i in seq_along(replacing)) {
    assign(localnames[[i]], get(bbotknames[[i]], envir = asNamespace("bbotk")), envir = parent.env(environment()))
  }

  replacing = c("Tuner%sFromOptimizer", "TuningInstance%sSingleCrit", "TuningInstance%sMultiCrit")
  oldnames = sprintf(replacing, "") 
  newnames = sprintf(replacing, "Batch")
  newnames[[1]] = paste0(newnames[[1]], "Batch")
  localnames = paste0(oldnames, c("", "_internal", "_internal"))
  for (i in seq_along(replacing)) {
    oldname = oldnames[[i]]
    newname = newnames[[i]]
    lname = localnames[[i]]
    makeActiveBinding(lname, env = parent.env(environment()), fun = crate(function() {
      if (!requireNamespace("mlr3tuning", quietly = TRUE)) return(NULL)
      if (exists(newname, envir = asNamespace("mlr3tuning"))) {
        get(newname, envir = asNamespace("mlr3tuning"))
      } else {
        get(oldname, envir = asNamespace("mlr3tuning"))
      }
    }, oldname, newname))
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

