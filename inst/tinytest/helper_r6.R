

expect_read_only = function(object, components) {
  lapply(components, function(cn) {
    expect_error(eval(substitute({x$y = 1}, list(x = quote(object), y = as.symbol(cn)))), sprintf("^%s is read-only\\.$", cn))
  })
}

expect_equal_without_id = function(a, b) {
  if (miesmuschel:::paradox_s3) return(expect_equal(a, b))
  a = a$clone(deep = TRUE)
  a$param_set$set_id = ""
  b = b$clone(deep = TRUE)
  b$param_set$set_id = ""
  expect_equal(a, b)
}

operator_public_state = function(x) {
  if (R6::is.R6(x)) {
    state = list(
      class = class(x),
      representation = repr(x, skip_defaults = FALSE)
    )
    if ("param_set" %in% names(x)) {
      state$param_values = operator_public_state(x$param_set$values)
    }
    return(state)
  }
  if (is.list(x)) {
    return(lapply(x, operator_public_state))
  }
  if (is.function(x)) {
    return(repr(x))
  }
  x
}

expect_equal_operator_public = function(a, b, info = NULL) {
  expect_equal(operator_public_state(a), operator_public_state(b), info = info)
}
