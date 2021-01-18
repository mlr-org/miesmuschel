#' @title MiesOperator
#' @export
MiesOperator = R6Class("MiesOperator",
  public = list(
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps(), endomorphism = TRUE) {
      assert_subset(param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), empty.ok = FALSE)
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
        private$.param_set_source = NULL
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
        private$.param_set_source = param_set
      }
      private$.param_classes = param_classes
      private$.endomorphism = assert_flag(endomorphism)
    },
    prime = function(param_set) {
      assert_subset(param_set$class, self$param_classes)
      private$.primed_ps = param_set
      invisible(self)
    },
    operate = function(values, ...) {
      if (is.null(private$.primed_ps)) stop("Operator must be primed first!")
      ids = private$.primed_ps$ids()
      private$.primed_ps$assert_dt(values)
      # make sure input / output cols are in the order as inndicated by paramset --> use `match` on both
      values = private$.operate(values[, match(ids, colnames(values), 0), with = FALSE], ...)
      if (self$endomorphism) {
        private$.primed_ps$assert_dt(values)[, match(ids, colnames(values), 0), with = FALSE]
      } else {
        values
      }
    }
  ),
  active = list(
    param_set = function(val) {
      if (is.null(private$.param_set)) {
        sourcelist = lapply(private$.param_set_source, function(x) eval(x))
        if (length(sourcelist) > 1) {
          private$.param_set = ParamSetCollection$new(sourcelist)
        } else {
          private$.param_set = sourcelist[[1]]
        }
        if (!is.null(private$.param_set_id)) private$.param_set$set_id = private$.param_set_id
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },
    param_classes = function(val) {
      if (!missing(val)) stop("param_classes is read-only.")
      private$.param_classes
    },
    endomorphism = function(val) {
      if (!missing(val)) stop("endomorphism is read-only.")
      private$.endomorphism
    }
  ),
  private = list(
    deep_clone = function(name, value) {
      if (!is.null(private$.param_set_source)) {
        private$.param_set_id = private$.param_set$set_id
        private$.param_set = NULL  # required to keep clone identical to original, otherwise tests get really ugly
        if (name == ".param_set_source") {
          value = lapply(value, function(x) {
            if (inherits(x, "R6")) x$clone(deep = TRUE) else x
          })
        }
      }
      if (is.environment(value) && !is.null(value[[".__enclos_env__"]])) {
        return(value$clone(deep = TRUE))
      }
      value
    },
    .param_set = NULL,
    .param_set_id = NULL,
    .primed_ps = NULL,
    .param_classes = NULL,
    .param_set_source = NULL,
    .operate = function(values, ...) stop(".operate needs to be implemented by inheriting class."),
    .endomorphism = NULL
  )
)
