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

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))

}


.datatable.aware = TRUE  # WTF
