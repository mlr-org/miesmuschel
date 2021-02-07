#' @param param_classes (`character`)\cr
#'   Classes of parameters that the operator can handle. May contain any of <%= { allowedparams <- tryCatch(allowedparams, error = function(e) "ParamLgl ParamInt ParamDbl ParamFct") ; allowedparams <- strsplit(allowedparams, " ")[[1]] ; paste('\\code{"', allowedparams, '"}', sep = "", collapse = ", ") } %>.
#'   Default is <%= if (length(allowedparams) > 2) "all of them" else if (length(allowedparams == 2)) "both of them" else sprintf('\\code{"%s"}', allowedparams) %>.\cr
#'   The `$param_classes` field will reflect this value.
