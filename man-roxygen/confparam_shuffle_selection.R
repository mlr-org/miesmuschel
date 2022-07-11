#' @section Configuration Parameters:
#' * `shuffle_selection` :: `logical(1)`\cr
#'   Whether to shuffle the selected output. When this is `TRUE`, selected individuals are returned in random order, so when this
#'   operator is e.g. used in [`mies_generate_offspring()`], then subsequent recombination operators effectively operate on pairs
#'   (or larger groups) of random individuals. Otherwise they are returned in order, and recombination operates on the first
#'   batch of `n_indivs_in` returned individuals first, then the second batch etc. in order. Initialized to `TRUE` (recommended).
