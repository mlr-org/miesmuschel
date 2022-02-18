

assignInNamespace(".__OptimInstance__clear", function (self, private, super) {
  self$archive$clear()
  private$.result = NULL
  self$progressor = NULL
  invisible(self)
}, pos = "package:bbotk")
