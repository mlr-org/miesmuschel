
# access variables from outside a data.table. Does not chain, unfortunately.
.. <- function(x, level = 1) eval.parent(substitute(x), 1 + assert_int(level, tol = 1e-100))
