#' @param condition_on_budget_id (`character(1)` | `NULL`)\cr
#'   Budget component when doing multi-fidelity optimization. When this is given, then for each generation, only individuals with the highest value for this
#'   component are considered. If `survivors_only` is `TRUE`, this means the highest value of all survivors of a given generation, if it is `FALSE`, then it
#'   is the highest value of all individuals alive at any point of a generation. To ignore possible budget-parameters, set this to `NULL` (default).
#'   This is inparticular necessary when fidelity is not monotonically increasing (e.g. if it is categorical).
