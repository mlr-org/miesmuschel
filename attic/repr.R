

#' @export
repr = function(obj, width = getOption("width", 80), indent = 0) {
  UseMethod("repr", obj)
}

#' @export
repr.default = function(obj, width = getOption("width", 80), indent = 0) {
  paste(deparse(obj, backtick = TRUE,
      control = c("keepInteger", "quoteExpressions", "showAttributes", "useSource", "niceNames"),
      width.cutoff = width),
    collapse = "\n")
}

#' @export
repr.list = function(obj, width = getOption("width", 80), indent = 0) {
  assert_int(indent, lower = 0, tol = 1e-100)
  listparts = imap(obj, function(x, n) {
    if (is.numeric(n) || n == "") {
      repr(x)
    } else {
      sprintf("%s = %s",
        if (make.names(n) == n) n else sprintf("`%s`", n),
        repr(x))
    }
  })
  leninfo = pmap(list(len = nchar(listparts), pos = gregexpr("\n", listparts)),
    function(len, pos) {
      if (pos[[1]] == -1) {
        c(len, -1)
      } else {
        c(pos[[1]] - 1, len - pos[[length(pos)]])
      }
    }
  )
  separators = numeric(0)
  hpos = 5 + indent
  for (i in seq_along(listparts)) {
    hpos = hpos + leninfo[[i]][[1]] + 2
    separators[[i]] = 1
    if (hpos > width + 1) {
      separators[[i]] = 2
      hpos = indent + 2 + if (leninfo[[i]][[2]] == -1) leninfo[[i]][[1]] else leninfo[[i]][[2]]
    }
  }
  indentstring = paste(rep(" ", indent), collapse = "")
  charseparators = c(", ", paste0(",\n", indentstring))[separators]
  charseparators[[1]] = paste0(indentstring, "list(", c("", "\n")[[separators[[1]]]])
  paste(c(rbind(charseparators, listparts), ")"), collapse = "")
}

#' @export
repr.R6 = function(obj, width = getOption("width", 80), indent = 0) {
  if (is.function({cl = .subset2(obj, "repr")})) {
    cl(width, indent)
  } else {
    NextMethod()
  }
}

#' @export
`!.MiesOperator` = function(x) {
  cat(repr(x))
  cat("\n")
  invisible(structure(list(), class = "INVISIBLE"))
}

#' @export
print.INVISIBLE = function(x, ...) invisible(NULL)
