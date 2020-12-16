
# TODO: dictionary & quick access function


#' @title Mutator
#' @include MiesOperator.R
Mutator = R6Class("Mutator",
  inherit = MiesOperator,
  private = list(
    .operate = function(values) private$.mutate(values)
    .mutate = function(values) stop(".mutate needs to be implemented by inheriting class.")
  )
)

MutatorNull = R6Class("Mutator",
  inherit = Mutator,
  private = list(
    .mutate = function(values) values
  )
)

# mutator that has a .mutate_numeric method that gets vector + lower and upper bounds
MutatorNumeric = R6Class("MutatorNumeric",
  inherit = Mutator,
  public = list(
    initialize = function(param_classes, param_set = ps()) {
      assert_subset(param_classes, c("ParamInt", "ParamDbl"))  # TODO: do we have a list of things with lower / upper bound?
      super$initialize(param_classes, param_set)
    }
  ),
  private = list(
    .mutate = function(values) {
      mutated <- t(apply(values, 1, private$.mutate_numeric, private$.primed_ps$lower, private$.primed_ps$$upper))
      colnames(mutated) <- colnames(values)
      as.data.table(mutated)
    },
    .mutate_numeric = function(values, lowers, uppers) stop(".mutate_numeric needs to be implemented by inheriting class.")
  )
)

# mutator that has a .mutate_discrete method
MutatorDiscrete = R6Class("MutatorDiscrete",
  inherit = Mutator,
  public = list(
    initialize = function(param_classes, param_set = ps()) {
      assert_subset(param_classes, c("ParamLgl", "ParamFct"))  # TODO: do we have a list of things here?
      super$initialize(param_classes, param_set)
    }
  ),
  private = list(
    .mutate = function(values) {
      vals = as.matrix(values)
      mode(vals) <- "character"
      vals = as.data.table(t(apply(vals, 1, private$.mutate_discrete, map(private$.primed_ps$levels, as.character)))

      vals = vals[, pmap(list(.SD, private$.primed_ps$class), function(val, class) if (class == "ParamLgl") as.logical(val) else val)]  # TODO maybe this can be done more elegantly
      setnames(vals, private$.primed_ps$ids())
    },
    .mutate_discrete = function(values, levels) stop(".mutate_discrete needs to be implemented by inheriting class.")
  )
)

MutatorGauss = R6Class("MutatorGauss",
  inherit = MutatorNumeric,
  public = list(
    initialize = function() {
      param_set = ps(sdev = p_dbl(0, tags = "required"), sdev_is_relative = p_lgl(tags = "required"), truncated_normal = p_lgl(tags = "required"))
      param_set$values = list(sdev = 1, sdev_is_relative = TRUE, truncated_normal = TRUE)
      super$initialize("ParamDbl", param_set)
    }
  ),
  private = list(
    .mutate_numeric = function(values, lowers, uppers) {
      params = self$param_set$get_values()
      if (params$sdev_is_relative) {
        assert_numeric(lowers, finite = TRUE, any.missing = FALSE)
        assert_numeric(uppers, finite = TRUE, any.missing = FALSE)
        sdev = params$sdev * (uppers - lowers)
      }
      if (params$truncated_normal) {
        # TODO: this truncated normal has rounding issues
        mutated <- qnorm(runif(length(values),
          pnorm(lowers, values, sdev),
          pnorm(uppers, values, sdev)),
          values, sdev)
      } else {
        mutated <- rnorm(length(values), values, sdev)
      }
      pmax(pmin(mutated, uppers), lowers)
    }
  )
)

MutatorDiscreteUniform = R6Class("MutatorDiscreteUniform",
  inherit = MutatorDiscrete,
  public = list(
    initialize = function() {
      param_set = ps(p = p_dbl(0, 1, tags = "required"), can_mutate_to_same = p_lgl(tags = "required"))
      param_set$values = list(p = 0.5, can_mutate_to_same = TRUE)
      super$initialize(c("ParamLgl", "ParamFct"), param_set)
    }
  ),
  private = list(
    .mutate_discrete = function(values, levels) {
      params = self$param_set$get_values()
      pmap(list(values, levels, runif(length(values)) < params$p), function(v, l, m) {
        if (m) {
          if (!params$can_mutate_to_same) l = setdiff(l, v)
          sample(l, 1)
        } else {
          v
        }
      })
    }
  )
)
