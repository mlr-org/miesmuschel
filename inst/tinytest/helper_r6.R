

expect_read_only = function(object, components) {
  lapply(components, function(cn) {
    expect_error(eval(substitute({x$y = 1}, list(x = quote(object), y = as.symbol(cn)))), sprintf("^%s is read-only\\.$", cn))
  })
}

expect_equal_without_id = function(a, b) {
  a = a$clone(deep = TRUE)
  a$param_set$set_id = ""
  b = b$clone(deep = TRUE)
  b$param_set$set_id = ""
  expect_equal(a, b)
}
