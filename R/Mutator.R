
#' @title Mutator
#' @include MiesOperator.R
#' @include dictionaries.R
#' @export
Mutator = R6Class("Mutator",
  inherit = MiesOperator,
  private = list(
    .operate = function(values) private$.mutate(values),
    .mutate = function(values) stop(".mutate needs to be implemented by inheriting class.")
  )
)

#' @export
MutatorNull = R6Class("MutatorNull",
  inherit = Mutator,
  private = list(
    .mutate = function(values) values
  )
)
mlr_mutators$add("null", MutatorNull)

#' @export
MutatorMaybe = R6Class("MutatorMaybe",
  inherit = Mutator,
  public = list(
    initialize = function(mutator, mutator_not = MutatorNull$new()) {
      private$.wrapped = assert_r6(mutator, "Mutator")$clone(deep = TRUE)
      private$.wrapped_not = assert_r6(mutator_not, "Mutator")$clone(deep = TRUE)

      private$.wrapped$param_set$set_id = "maybe"
      private$.wrapped_not$param_set$set_id = "maybe_not"
      private$.maybe_param_set = ps(p = p_dbl(0, 1, tags = "required"))
      private$.maybe_param_set$values = list(p = 1)
      super$initialize(mutator$param_classes,
        alist(private$.maybe_param_set, private$.wrapped$param_set, private$.wrapped_not$param_set))
    },
    prime = function(param_set) {
      private$.wrapped$prime(param_set)
      private$.wrapped_not$prime(param_set)
      super$prime(param_set)
      invisible(self)
    }
  ),
  private = list(
    .mutate = function(values) {
      mutating = runif(nrow(values)) < self$param_set$get_values()$p
      if (any(mutating)) {
        mutated = private$.wrapped$operate(values[mutating])
      } else {
        mutated = values[mutating]
      }
      if (any(!mutating)) {
        mutated_not = private$.wrapped_not$operate(values[!mutating])
      } else {
        mutated_not = values[!mutating]
      }
      rownumbers = seq_len(nrow(values))
      rowoder = order(c(rownumbers[mutating], rownumbers[!mutating]))
      rbind(mutated, mutated_not)[rowoder]
    },
    .wrapped = NULL,
    .wrapped_not = NULL,
    .maybe_param_set = NULL
  )
)
mlr_mutators$add("maybe", MutatorMaybe)

# mutator that has a .mutate_numeric method that gets vector + lower and upper bounds
#' @export
MutatorNumeric = R6Class("MutatorNumeric",
  inherit = Mutator,
  public = list(
    initialize = function(param_classes, param_set = ps()) {
      assert_subset(param_classes, c("ParamInt", "ParamDbl"))
      super$initialize(param_classes, param_set)
    }
  ),
  private = list(
    .mutate = function(values) {
      mutated <- apply(values, 1, private$.mutate_numeric, private$.primed_ps$lower, private$.primed_ps$upper)
      if (is.matrix(mutated)) {
        mutated <- t(mutated)
      } else {
        mutated <- as.matrix(mutated)
      }
      colnames(mutated) <- colnames(values)
      as.data.table(mutated)
    },
    .mutate_numeric = function(values, lowers, uppers) stop(".mutate_numeric needs to be implemented by inheriting class.")
  )
)

# mutator that has a .mutate_discrete method

#' @export
MutatorDiscrete = R6Class("MutatorDiscrete",
  inherit = Mutator,
  public = list(
    initialize = function(param_classes, param_set = ps()) {
      assert_subset(param_classes, c("ParamLgl", "ParamFct"))
      super$initialize(param_classes, param_set)
    }
  ),
  private = list(
    .mutate = function(values) {
      vals = as.matrix(values)
      mode(vals) <- "character"
      vals = apply(vals, 1, private$.mutate_discrete, map(private$.primed_ps$levels, as.character))
      if (is.matrix(vals)) {
        vals = t(vals)
      } else {
        vals = as.matrix(vals)
      }
      vals = as.data.table(vals)

      vals = vals[, pmap(list(.SD, private$.primed_ps$class), function(val, class) if (class == "ParamLgl") as.logical(val) else val)]  # TODO maybe this can be done more elegantly
      setnames(vals, private$.primed_ps$ids())
    },
    .mutate_discrete = function(values, levels) stop(".mutate_discrete needs to be implemented by inheriting class.")
  )
)

#' @export
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
      sdev = params$sdev
      if (params$sdev_is_relative) {
        assert_numeric(lowers, finite = TRUE, any.missing = FALSE)
        assert_numeric(uppers, finite = TRUE, any.missing = FALSE)
        sdev = sdev * (uppers - lowers)
      }
      if (params$truncated_normal) {
        mutated <- ifelse(sdev == 0, values,
          qnorm(runif(length(values),
            pnorm(lowers, values, sdev),
            pnorm(uppers, values, sdev)),
            values, sdev)
        )
      } else {
        mutated <- rnorm(length(values), values, sdev)
      }
      pmax(pmin(mutated, uppers), lowers)
    }
  )
)
mlr_mutators$add("gauss", MutatorGauss)

#' @export
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
      unlist(pmap(list(values, levels, runif(length(values)) < params$p), function(v, l, m) {
        if (m) {
          if (!params$can_mutate_to_same) l = setdiff(l, v)
          sample(l, 1)
        } else {
          v
        }
      }))
    }
  )
)
mlr_mutators$add("unif", MutatorDiscreteUniform)
