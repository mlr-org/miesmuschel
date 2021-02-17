#' @import mlr3misc
#' @import checkmate
#' @import bbotk
#' @import paradox
#' @import R6
#' @import data.table
"_PACKAGE"

lg = NULL

.onLoad = function(libname, pkgname) {
  mlr_optimizers = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  mlr_optimizers$add("mies", OptimizerMies)


  mlr_terminators = utils::getFromNamespace("mlr_terminators", ns = "bbotk")
  mlr_terminators$add("gens", TerminatorGenerations)
  mlr_terminators$add("budget", TerminatorBudget)

  if (requireNamespace("mlr3tuning", quietly = TRUE)) {
    mlr_tuners = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
    mlr_tuners$add("mies", TunerMies)
  }

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

}

# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("dob", "eol"))

.datatable.aware = TRUE  # WTF
