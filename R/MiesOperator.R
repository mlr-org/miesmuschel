

#' @title MiesOperator
MiesOperator = R6Class("MiesOperator",
  public = list(
    initialize = function(param_classes = c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), param_set = ps()) {
      assert_subset(param_classes, c("ParamLgl", "ParamInt", "ParamDbl", "ParamFct"), empty.ok = FALSE) # TODO: do we have a list?
      if (inherits(param_set, "ParamSet")) {
        private$.param_set = assert_param_set(param_set)
        private$.param_set_source = NULL
      } else {
        lapply(param_set, function(x) assert_param_set(eval(x)))
        private$.param_set_source = param_set
      }
      private$.param_classes = param_classes
    },
    prime = function(param_set) {
      assert_subset(param_set$class, self$param_classes)
      private$.primed_ps = param_set
      invisible(self)
    },
    operate = function(values, ...) {
      ids = private$.primed_ps$ids()
      private$.primed_ps$assert_dt(values)
      values = private$.operate(values[, match(ids, colnames(values), 0), with = FALSE], ...)
      private$.primed_ps$assert_dt(values)[, match(ids, colnames(values), 0), with = FALSE]
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
      }
      if (!missing(val) && !identical(val, private$.param_set)) {
        stop("param_set is read-only.")
      }
      private$.param_set
    },
    param_classes = function(val) {
      if (!missing(val)) stop("param_classes is read-only.")
      private$.param_classes
    }
  ),
  private = list(
      # TODO: cloning
    .param_set = NULL,
    .primed_ps = NULL,
    .param_classes = NULL,
    .param_set_source = NULL,
    .operate = function(values, ...) stop(".operate needs to be implemented by inheriting class.")
  )
)
