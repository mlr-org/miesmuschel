#' @param survival_selector ([`Selector`])\cr
#'   [`Selector`] operator that selects surviving individuals depending on configuration values
#'   and objective results. When `survival_selector$operate()` is called, then objectives that
#'   are being minimized are multiplied with -1 (through [`mies_get_fitnesses`]), since [`Selector`]s always try to maximize fitness.\cr
#'   The [`Selector`] must be primed on `inst$search_space`; this *includes* the "budget" component
#'   when performing multi-fidelity optimization.\cr
#'   The given [`Selector`] may *not* return duplicates.
