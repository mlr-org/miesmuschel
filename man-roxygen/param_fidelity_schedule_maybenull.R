#' @param fidelity_schedule (`data.frame` | `NULL`)\cr
#'   `data.frame` with three columns `"generation"`, `"budget_new"`, `"budget_survivors"`, in that order. `"budget_new"` and `"budget_survivors"`
#'   are atomic columns assigned to the `budget_id` component of `offspring`; which one is chosen depends on `survivor_budget`.
#'   `"generation"` is an integer valued column, indicating the first generation at which a row is valid. At least one row with
#'   `generation == 1` must be present.\cr
#'   This value may be `NULL` if no multi-fidelity optimization is performed (the default).\cr
#'   Note that the multifidelity functionality is experimental and the UI may change in the future.
