#' @include ParamSetShadow.R
#' @export
OperatorCombination = R6Class("OperatorCombination",
  inherit = MiesOperator,
  public = list(
    initialize = function(operators = list(), groups = list(), strategies = list(), binary_fct_as_logical = FALSE, on_type_not_present = "warn", on_name_not_present = "error", granularity = 1) {
      private$.granularity = assert_int(granularity, lower = 1, tol = 1e-100)
      private$.on_name_not_present = assert_choice(on_name_not_present, c("quiet", "warn", "stop"))
      private$.on_type_not_present = assert_choice(on_type_not_present, c("quiet", "warn", "stop"))
      private$.binary_fct_as_logical = assert_flag(binary_fct_as_logical)
      private$.operators = assert_list(operators, types = "MiesOperator", any.missing = FALSE, names = "unique", min.len = 1)
      private$.groups = assert_list(groups, types = "character", names = "unique", any.missing = FALSE)
      private$.strategies = assert_list(strategies, types = "function", names = "unique", any.missing = FALSE)

      lapply(strategies, assert_function, nargs = 1)

      lapply(groups, assertCharacter, min.chars = 1, any.missing = FALSE, unique = TRUE)
      allgroups = unlist(groups, use.names = FALSE)
      if (anyDuplicated(allgroups)) {
        stopf("Duplicate group references: %s", str_collapse(unique(allgroups[duplicated(allgroups)])))
      }
      if (any(allgroups %in% names(groups))) {
        stopf("No recursive groups allowed: %s", str_collapse(intersect(allgroups, names(groups))))
      }
      if (any(allgroups %in% names(operators))) {
        stopf("Dimensions %s that have their own operator must not be present in groups.",
          str_collapse(intersect(allgroups, names(operators))))
      }
      if (any(names(groups) %nin% names(operators))) {
        stopf("No operator for group(s) %s", str_collapse(setdiff(names(operators), names(groups))))
      }
      types = c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl")
      if (any(names(groups) %in% types)) {
        stop('Special group names "ParamInt", "ParamDbl", "ParamFct", "ParamLgl" may not be used.')
      }

      param_classes_list = list()
      for (on in names(operators)) {
        operators[[on]] = operators[[on]]$clone(deep = TRUE)
        op = operators[[on]]
        op$param_set$set_id = on

        if (on %in% types) {
          if (on %nin% op$param_classes) {
            stopf("Operator for %s does not support the type.", on)
          }
          pcl = on
        } else if (on %in% names(groups)) {
          neededtypes = intersect(types, groups[[on]])
          if (any(neededtypes %nin% op$param_classes)) {
            stopf("Operator for group %s does not support type(s) %s.",
              on, str_collapse(setdiff(neededtypes, op$param_classes)))
          }
          if (all(groups[[on]] %in% types)) {
            pcl = groups[[on]]
          } else {
            pcl = op$param_classes
          }
        } else {
          pcl = op$param_classes
        }

        param_classes_list[[length(param_classes_list) + 1]] = pcl
      }
      params = ParamSetCollection$new(map(self$operators, "param_set"))$ids()
      if (any(names(strategies) %nin% params)) {
        stopf("Strategy for %s which is not an operator parameter.",
          str_collapse(setdiff(names(strategies), params)))
      }

      super$initialize(
        param_classes = unique(unlist(param_classes_list, use.names = FALSE)),
        param_set = alist(
          ParamSetShadow$new(
            ParamSetCollection$new(map(self$operators, "param_set")),
            names(self$strategies)
          )
        )
      )
    },
    prime = function(param_set) {
      super$prime(param_set)
      types = c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl")
      groupnames = c(types, names(self$groups))
      if (any(groupnames %in% param_set$ids())) {
        stop("groupnames / Param class names and ids of param_set must be disjoint")
      }
      ids = param_set$ids()
      classes = param_set$class

      capturing = c(setdiff(names(self$operators), names(self$groups)), unlist(self$groups))
      if (any(setdiff(capturing, types)  %nin% ids)) {
        switch(self$on_name_not_present,
          "quiet" = function(...) NULL,
          "warn" = warnf,
          "stop" = stopf)(
          "Named operators %s have no corresponding dimension.",
          str_collapse(setdiff(capturing, c(types, ids))))
      }
      captured_types = classes[ids %nin% capturing]
      type_captured_ids = ids[ids %nin% capturing]
      type_mapping = sapply(unique(captured_types), function(x) type_captured_ids[captured_types == x])
      if (any(intersect(capturing, types) %nin% captured_types)) {
        switch(self$on_type_not_present,
          "quiet" = function(...) NULL,
          "warn" = warnf,
          "stop" = stopf)(
          "Operators for types %s have no corresponding dimensions (or dimensions are overriden).",
          str_collapse(setdiff(intersect(capturing, types), captured_types)))
      }
      if (any(captured_types %nin% capturing)) {
        badtypes = setdiff(unique(captured_types), capturing)
        stopf("No operators for dimensions %s of types %s",
          str_collapse(type_captured_ids[captured_types %in% badtypes]),
          str_collapse(badtypes))
      }
      mapping = c(
        keep(type_mapping[names(self$operators)], length),
        sapply(self$groups, function(g) intersect(c(setdiff(g, types), unlist(type_mappings[g], use.names = FALSE)), ids))
      )
      subsettable = ParamSet$new(param_set$params)
      imap(mapping, function(pars, op) {
        self$operators[[op]]$prime(subsettable$clone()$subset(pars))
      })
      private$.mapping = mapping
      invisible(self)
    }
  ),
  active = list(
    operators = function(val) {
      if (!missing(val)) stop("operators is read-only.")
      private$.operators
    },
    groups = function(val) {
      if (!missing(val)) stop("groups is read-only.")
      private$.groups
    },
    strategies = function(val) {
      if (!missing(val)) stop("strategies is read-only.")
      private$.strategies
    },
    binary_fct_as_logical = function(val) {
      if (!missing(val)) stop("binary_fct_as_logical is read-only.")
      private$.binary_fct_as_logical
    },
    on_type_not_present = function(val) {
      if (!missing(val)) {
        private$.on_type_not_present = assert_choice(val, c("quiet", "warn", "stop"))
      }
      private$.on_type_not_present
    },
    on_name_not_present = function(val) {
      if (!missing(val)) {
        private$.on_name_not_present = assert_choice(val, c("quiet", "warn", "stop"))
      }
      private$.on_name_not_present
    }
  ),
  private = list(
    .operators = NULL,
    .groups = NULL,
    .strategies = NULL,
    .binary_fct_as_logical = NULL,
    .on_type_not_present = NULL,
    .on_name_not_present = NULL,
    .mapping = NULL,
    .granularity = NULL,
    .operate = function(values, ...) {
      granularity = if (!length(private$.strategies)) nrow(values) else private$.granularity
      assert_true(nrow(values) %% granularity == 0)
      rbindlist(
        lapply(split(values, rep(nrow(values / granularity, each = granularity))), function(vs) {
          strategy_values = lapply(private$.strategies, function(f) lapply(vs, f))
          self$param_set$origin$values = insert_named(self$param_set$origin$values, strategy_values)
          do.call(cbind, unname(imap(private$.mapping, function(pars, op) {
            self$operators[[op]]$operate(vs[, match(pars, names(vs), 0), with = FALSE])
          })))
        }),
        use.names = TRUE
      )
    }
  )
)

#' @export
MutatorCombination = R6Class("MutatorCombination",
  inherit = OperatorCombination,
  public = list(
    initialize = function(operators = list(), groups = list(), strategies = list(), binary_fct_as_logical = FALSE, on_type_not_present = "warn", on_name_not_present = "stop") {
      assert_list(operators, types = "Mutator")
      super$initialize(operators = operators, groups = groups, strategies = strategies, binary_fct_as_logical = binary_fct_as_logical, on_type_not_present = on_type_not_present, on_name_not_present = on_name_not_present)
      class(self) = c("Mutator", class(self))
    }
  )
)

#' @export
RecombinatorCombination = R6Class("RecombinatorCombination",
  inherit = OperatorCombination,
  public = list(
    initialize = function(operators = list(), groups = list(), strategies = list(), binary_fct_as_logical = FALSE, on_type_not_present = "warn", on_name_not_present = "stop") {
      assert_list(operators, types = "Recombinator")
      inout = map_dtr(operators, function(o) list(nin = o$n_indivs_in, nout = o$n_indivs_out))
      if (nrow(unique(inout)) == 1) {
        private$.n_indivs_in = inout$nin
        private$.n_indivs_out = inout$nout
      } else if (all(inout$nin %in% c(1, 2)) && all(inout$nout == inout$nin)) {
        private$.n_indivs_in = max(inout$nin)
        private$.n_indivs_out = max(inout$nout)
      } else {
        stop("Combining operators with disagreeing n_indivs_in / n_indivs_out not yet supported.")
      }
      super$initialize(operators = operators, groups = groups, strategies = strategies, binary_fct_as_logical = binary_fct_as_logical, on_type_not_present = on_type_not_present, on_name_not_present = on_name_not_present, granularity = self$n_indivs_in)
      class(self) = c("Recombinator", class(self))
    }
  ),
  # --- copy-paste from Recombinator.R
  active = list(
    n_indivs_in = function(val) {
      if (!missing(val)) stop("n_indivs_in is read-only.")
      private$.n_indivs_in
    },
    n_indivs_out = function(val) {
      if (!missing(val)) stop("n_indivs_out is read-only.")
      private$.n_indivs_out
    }
  ),
  private = list(
    .n_indivs_in = NULL,
    .n_indivs_out = NULL,
    .recombine = function(values) stop(".recombine needs to be implemented by inheriting class."),
  # --- copy-paste end
    .operate = function(values) {
      assert_true(nrow(values) == self$n_indivs_in)  # combinator handles granularity
      values = private$.recombine(values)
      assert_data_table(values, nrows = self$n_indivs_out)
    }
  )
)
