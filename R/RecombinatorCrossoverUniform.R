#' @title Crossover Recombinator
#'
#' @name dict_recombinators_xounif
#'
#' @description
#' Values between two individuals are exchanged with component-wise independent probability.
#'
#' This is a pseudo-class: It does not create a single R6-object of a class;
#' instead, it creates the object `rec("cmpmaybe", rec("swap"), p = 0.5)`,
#' making use of the [`RecombinatorCmpMaybe`] and [`RecombinatorSwap`] operators.
#'
#' @templateVar id xounif
#' @template autoinfo_prepare_rec
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @template param_keep_complement
#' @return an object of class [`Recombinator`]: `rec("cmpmaybe", rec("swap"))`.
#'
#' @family recombinators
#' @examples
#' set.seed(1)
#' rx = rec("xounif")
#'
#' print(rx)
#'
#' p = ps(x = p_int(-5, 5), y = p_dbl(-5, 5), z = p_dbl(-5, 5))
#' data = data.frame(x = 0:5, y = 0:5, z = 0:5)
#'
#' rx$prime(p)
#' rx$operate(data)
#'
#' rx$param_set$values$p = 0.3
#' rx$operate(data)
#' @export
RecombinatorCrossoverUniform = function(keep_complement = TRUE) {
  xounif = RecombinatorCmpMaybe$new(RecombinatorSwap$new(keep_complement = keep_complement))
  xounif$param_set$values$p = 0.5
  xounif
}
dict_recombinators$add("xounif", RecombinatorCrossoverUniform)
