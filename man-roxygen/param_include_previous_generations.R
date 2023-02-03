#' @param include_previous_generations (`logical(1)`)\cr
#'   Aggregate all individuals that were alive at `generation` or at any point before that.
#'   Duplicates with the same `x_id` are removed, meaning that if an individual was re-evaluated with different fidelity, only the last
#'   re-evaluation is counted.
#'   However, note that individuals from different generations may still have been evaluated with different fidelity, so if
#"   `include_previous_generations` is `TRUE`, it may be necessary to inspect the "budget" component of `xdt`.
#'   Default `FALSE`.
