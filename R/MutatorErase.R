#' @title Uniform Sample Mutator
#'
#' @include Mutator.R
#'
#' @name dict_mutators_erase
#'
#' @description
#' "Mutates" individuals by forgetting the current value and sampling new individuals from scratch.
#'
#' Since the information loss is very high, this should in most cases be combined with [`MutatorCmpMaybe`].
#'
#' @section Configuration Parameters:
#' * `initializer` :: `function`\cr
#'   Function that generates the initial population as a [`Design`][paradox::Design] object,
#'   with arguments `param_set` and `n`, functioning like [`paradox::generate_design_random`] or [`paradox::generate_design_lhs`].
#'   This is equivalent to the `initializer` parameter of [`mies_init_population()`], see there for more information. Initialized to
#'   [`generate_design_random()`][paradox::generate_design_random].
#'
#' @templateVar id erase
#' @template autoinfo_prepare_mut
#' @template autoinfo_operands
#' @template autoinfo_dict
#'
#' @family mutators
#' @examples
#' set.seed(1)
#' mer = mut("erase")
#' p = ps(x = p_lgl(), y = p_fct(c("a", "b", "c")), z = p_dbl(0, 1))
#' data = data.frame(x = rep(TRUE, 5), y = rep("a", 5),
#'   z = seq(0, 1, length.out = 5),
#'   stringsAsFactors = FALSE)  # necessary for R <= 3.6
#'
#' mer$prime(p)
#' mer$operate(data)
#'
#' @export
MutatorErase = R6Class("MutatorErase",
  inherit = Mutator,
  public = list(
    #' @description
    #' Initialize the `MutatorErase` object.
    initialize = function() {
      param_set = ps(
        initializer = p_uty(custom_check = crate(function(x) check_function(x, args = c("param_set", "n"))), tags = c("init", "required"))  # arguments: param_set, n
      )
      param_set$values = list(initializer = generate_design_random)
      super$initialize(param_set = param_set, dict_entry = "erase")
    }
  ),
  private = list(
    .mutate = function(values) {
      params = self$param_set$get_values()
      assert_data_frame(params$initializer(private$.primed_ps, nrow(values))$data, nrows = nrow(values))
    }
  )
)
dict_mutators$add("erase", MutatorErase)
