#' @title Recombinator Base Class
#'
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Base class representing recombination operations, inheriting from [`MiesOperator`].
#'
#' Recombinators get a table of individuals as input and return a table of modified individuals as output. Individuals are acted on by
#' groups: every `$n_indivs_out` lines of output corresponds to a group of `$n_indivs_in` lines of input, and presence or absence
#' of other input groups does not affect the result.
#'
#' Recombination operations are performed in ES algorithms to facilitate exploration of the search space that combine partial
#' solutions.
#'
#' @section Inheriting:
#' `Recombinator` is an abstract base class and should be inherited from. Inheriting classes should implement the private `$.recombine()`
#' function. The user of the object calls `$operate()`, which calls `$.recombine()` for each `$n_indivs_in` sized group of individuals after checking that
#' the operator is primed, that the `values` argument conforms to the primed domain. `$.recombine()` should then return a table of
#' `$n_indivs_out` individuals for each call. Typically, the `$initialize()` function
#' should also be overloaded, and optionally the `$prime()` function; they should call their `super` equivalents.
#'
#' @family base classes
#' @family recombinators
#' @export
Recombinator = R6Class("Recombinator",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize base class components of the `Recombinator`.
    #' @template param_param_classes
    #' @template param_param_set
    #' @template param_n_indivs_in
    #' @param n_indivs_out (`integer(1)`)\cr
    #'   Number of individuals that result for each `n_indivs_in` lines of input. The number of results from the recombinator will be
    #'   `nrow(values) / n_indivs_in * n_indivs_out`. Default equal to `n_indivs_in`.\cr
    #'   The `$n_indivs_out` field will reflect this value.
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), n_indivs_in = 2, n_indivs_out = n_indivs_in, packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {
      assert_int(n_indivs_in, lower = 1, tol = 1e-100)
      assert_int(n_indivs_out, lower = 1, tol = 1e-100)
      private$.n_indivs_in = n_indivs_in
      private$.n_indivs_out = n_indivs_out
      super$initialize(param_classes, param_set, packages = packages, dict_entry = dict_entry, dict_shortaccess = "rec", own_param_set = own_param_set)
    }
  ),
  active = list(
    #' @field n_indivs_in (`integer(1)`)\cr
    #' Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    n_indivs_in = function(val) {
      if (!missing(val)) stop("n_indivs_in is read-only.")
      private$.n_indivs_in
    },
    #' @field n_indivs_out (`integer(1)`)\cr
    #' Number of individuals produced for each group of `$n_indivs_in` individuals.
    n_indivs_out = function(val) {
      if (!missing(val)) stop("n_indivs_out is read-only.")
      private$.n_indivs_out
    }
  ),
  private = list(
    .n_indivs_in = NULL,
    .n_indivs_out = NULL,
    .recombine = function(values) stop(".recombine needs to be implemented by inheriting class."),
    .operate = function(values) {
      assert_true(nrow(values) %% self$n_indivs_in == 0)
      rbindlist(
        lapply(split(values, rep(seq_len(nrow(values) / self$n_indivs_in), each = self$n_indivs_in)), function(vs) {
          vs = private$.recombine(vs)
          assert_data_table(vs, nrows = self$n_indivs_out)
        }), use.names = TRUE)
    }
  )
)


#' @title Pair Recombinator Base Class
#'
#' @description
#' Base class for recombination that covers the common case of combining two individuals, where two (typically complementary) child individuals
#' could be taken as the result, such as [bitwise crossover][RecombinatorCrossoverUniform] or [SBX crossover][RecombinatorSimulatedBinaryCrossover].
#'
#' This is a relatively lightweight class, it adds the `keep_complement` active binding and sets `$n_indivs_in` and `$n_indivs_out` appropriately.
#'
#' @section Inheriting:
#' `RecombinatorPair` is an abstract base class and should be inherited from. Inheriting classes should implement the private
#' `$.recombine_pair()` function. During `$operate()`, the `$.recombine_pair()` function is called with the same input as the `$.recombine()` function
#' of the [`Recombinator`] class. It should return a `data.table` of two individuals.
#'
#' Constructors of inheriting  classes should have a `keep_complement` argument.
#'
#' @family base classes
#' @family recombinators
#' @export
RecombinatorPair = R6Class("RecombinatorPair",
  inherit = Recombinator,
  public = list(
    #' @description
    #' Initialize base class components of the `RecombinatorPair`.
    #' @template param_keep_complement
    #' @template param_param_classes
    #' @template param_param_set
    #' @template param_packages
    #' @template param_dict_entry
    #' @template param_own_param_set
    initialize = function(keep_complement = TRUE, param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), packages = character(0), dict_entry = NULL, own_param_set = quote(self$param_set)) {

      super$initialize(param_classes = param_classes, param_set = param_set, n_indivs_in = 2, n_indivs_out = if (keep_complement) 2 else 1,
        packages = packages, dict_entry = dict_entry, own_param_set = own_param_set)
    }
  ),
  active = list(
    #' @field keep_complement (`logical(1)`)\cr
    #' Whether the operation keeps both resulting individuals of the operation or discards the complement.
    keep_complement = function(val) {
      if (!missing(val)) stop("keep_complement is read-only.")
      private$.n_indivs_out == 2
    }
  ),
  private = list(
    .recombine = function(values) {
      private$.recombine_pair(values)[seq_len(self$n_indivs_out)]
    },
    .recombine_pair = function(values) stop(".mutate_numeric needs to be implemented by inheriting class.")
  )
)
