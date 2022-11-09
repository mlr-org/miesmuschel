library("checkmate")

arg <- function(info, long, short = NULL, type = "logical", default = if (identical(type, "logical")) FALSE else NULL, choices = NULL, name = long, checkmate.args = list()) {
  assertString(info)
  assertString(name, pattern = "^[.[:alpha:]][.[:alnum:]]*$")
  assertString(short, pattern = "^.$", null.ok = TRUE)
  assertString(long, pattern = "^[-.[:alnum:]]+$")
  assertChoice(type, c("logical", "integer", "numeric", "character", "choice"))
  assertList(checkmate.args, names = "unique")
  switch(type,
    logical = assertFALSE(default),
    integer = assertInt(default, tol = 1e-100, null.ok = TRUE),
    numeric = assertNumber(default, null.ok = TRUE),
    character = assertString(default, null.ok = TRUE),
    choice = {
      assertCharacter(choices, min.len = 1, any.missing = FALSE)
      assertChoice(default, choices, null.ok = TRUE)
    }
  )
  structure(list(info = info, name = name, short = short %??% "", long = long, type = type, default = default, choices = choices), class = "arg")

}



argparse <- function(argdefs, print.help = FALSE, inputs = commandArgs(trailingOnly = TRUE), helparg = NULL) {
  assertList(argdefs, types = "arg")

  if (print.help) {
    for (arg in argdefs) {
      quote = if (arg$type %in% c("character", "choice")) '"' else ""
      cat(sprintf("  %20s %s: %s.%s (%s%s)\n",
        paste0("--", arg$long),
        if (!is.null(arg$short)) sprintf("[-%s] ", arg$short) else "",
        arg$info,
        if (!is.null(arg$default) && arg$type != "logical") sprintf(" Default: %s%s%s.", quote, arg$default, quote) else "",
        arg$type,
        if (arg$type == "choice") sprintf(": {%s}", paste(sprintf('"%s"', arg$choices), collapse = ", ")) else ""
      ))
    }
    cat("\n")

    return(NULL)
  }

  result <- structure(lapply(argdefs, `[[`, "default"), names = sapply(argdefs, `[[`, "name"))

  assertChoice(helparg, names(result), null.ok = TRUE)
  if (!is.null(helparg) && argdefs[[match(helparg, names(result))]]$type != "logical") stop(sprintf("help argument %s must be a 'logical'", helparg))

  longdescs <- sapply(argdefs, `[[`, "long")
  shortdescs <- sapply(argdefs, `[[`, "short")

  if (anyDuplicated(c(shortdescs, longdescs))) stop("duplicated argument names")
  if (anyDuplicated(names(result))) stop("duplicated names")

  i <- 1
  while (i <= length(inputs)) {
    token <- inputs[[i]]

    can.gobble <- TRUE
    if (grepl("^--", token)) {
      curargidx <- match(sub("^--", "", token), longdescs)
    } else if (grepl("^-", token)) {

      if (nchar(token) > 2) {
        inputs[[i]] <- paste0("-", substring(token, 3, nchar(token)))
        can.gobble <- FALSE
      }

      token <- paste0("-", substring(token, 2, 2))
      curargidx <- match(substring(token, 2, 2), shortdescs)
    } else {
      Recall(argdefs, print.help = TRUE)
      stop(sprintf("Not an argument and not in an argument position: %s", token))
    }

    if (is.na(curargidx)) {
      Recall(argdefs, print.help = TRUE)
      stop(sprintf("Not a valid argument: %s\n", token))
    }

    curarg <- argdefs[[curargidx]]

    if (curarg$type != "logical") {
      if (!can.gobble) stop("Non-flag short arguments can only be given at the end of an option group")
      if (i == length(inputs)) {
        Recall(argdefs, print.help = TRUE)
        stop(sprintf("Argument %s requires an option.", token))
      }
      i <- i + 1
      parameter <- inputs[[i]]
    }

    result[[curarg$name]] <- switch(curarg$type,
      logical = TRUE,
      numeric = suppressWarnings(as.numeric(parameter)),
      integer = suppressWarnings(as.numeric(parameter)),
      character = parameter,
      choice = parameter,
      stop()
    )
    asserter <- switch(curarg$type,
      logical = function(x, ...) x,
      numeric = assertNumber,
      integer = assertInt,
      character = assertString,
      choice = assertChoice,
      stop()
    )
    addargs <- switch(curarg$type,
      integer = list(tol = 1e-100),
      choice = list(choices = curarg$choices),
      list()
    )
    do.call(asserter, c(list(x = result[[curarg$name]], .var.name = token), addargs, curarg$checkmate.args))

    if (can.gobble) i <- i + 1
  }

  if (!is.null(helparg) && result[[helparg]]) {
    Recall(argdefs, print.help = TRUE)
    invokeRestart("abort")
    print("eeh")
  }

  missings <- longdescs[which(sapply(result, is.null))]
  if (length(missings)) {
    Recall(argdefs, print.help = TRUE)
    stop(sprintf("Missing argument%s: %s", if (length(missings)) "s", paste(sprintf('"%s"', missings), collapse = ", ")))
  }

  result

}

