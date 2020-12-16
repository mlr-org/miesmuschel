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


# Global TODOs:
# - should selector do something about sampling with / without replacement?
#   - e.g. prevent crossover with identical individuals?
# - how to handle noisy functions? Re-evaluate every generation?
# - how to handle "alive" individuals?
# - maybe handle individuals as data.table
# - multicrit
# - sensible naming for 'strategy'
# - documentation
# - multi type operators
# - mutate / recombine call should be accelerated, e.g. load paramset somewhere else
# - operator paramset names
# - helper function for pattern: if (empty<x>) stopf("there are elements <x> that should not be there")
# - implement actual mies strategy parameters
# - handle missing values(?)
