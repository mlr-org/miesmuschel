#' @import mlr3misc
#' @import checkmate
#' @import bbotk
#' @import paradox
NULL

lg = NULL

.onLoad = function(libname, pkgname) {
  mlr_optimizers = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  mlr_optimizers$add("mies")

  assign("lg", lgr::get_logger("bbotk"), envir = parent.env(environment()))


  Archive$set("public", "alive", integer(0))  # TODO: this needs to be added to bbotk I assume
}


