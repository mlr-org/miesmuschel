#' @import mlr3misc
#' @import checkmate
#' @import bbotk
#' @import paradox
#' @import R6
#' @import data.table
"_PACKAGE"

lg = NULL

reg_bbotk = function(...) {
  mlr_optimizers = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  mlr_optimizers$add("mies", OptimizerMies)

  mlr_terminators = utils::getFromNamespace("mlr_terminators", ns = "bbotk")
  mlr_terminators$add("gens", TerminatorGenerations)
  mlr_terminators$add("budget", TerminatorBudget)
}

reg_mlr3tuning = function(...) {
  if (requireNamespace("mlr3tuning", quietly = TRUE)) {
    mlr_tuners = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
    mlr_tuners$add("mies", TunerMies)
  }
}

.onLoad = function(libname, pkgname) {
  reg_bbotk()
  reg_mlr3tuning()

  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  setHook(packageEvent("bbotk", "onLoad"), reg_bbotk, action = "append")
  setHook(packageEvent("mlr3tuning", "onLoad"), reg_mlr3tuning, action = "append")
}

.onUnload = function(libpath) {
  for (pkg in c("bbotk", "mlr3tuning")) {
    load_event = packageEvent(pkg, "onLoad")
    setHook(load_event,
      keep(getHook(load_event), function(x) x$pkgname != "miesmuschel"),
      action = "replace"
    )
  }
}


# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("dob", "eol"))

if (!Sys.getenv("DEVTOOLS_LOAD") == "true") {
  leanify_package()
}
