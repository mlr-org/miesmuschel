#' @section Dictionary:
#' This [`<%= classname %>`] can be created with the short access form [`<%= shortname %>()`]
#' ([`<%= shortnameplural %>()`] to get a list), or through the the [dictionary][mlr3misc::Dictionary]
#' [`<%= dictname %>`] in the following way:
#' ```
#' # preferred:
#' <%= shortname %>("<%= id %>"<%= tryCatch(additional, error = function(e) "") %>)
#' <%= shortnameplural %>("<%= id %>"<%= tryCatch(additional, error = function(e) "") %>)  # takes vector IDs, returns list of [`<%= classname %>`]s
#'
#' # long form:
#' <%= dictname %>$get("<%= id %>"<%= tryCatch(additional, error = function(e) "") %>)
#' ```
