#' @title Self-Adaptive Operator Combinations
#'
#' @include ParamSetShadow.R
#' @include MiesOperator.R
#' @include dictionaries.R
#'
#' @description
#' Combines multiple operators and makes operator-hyperparameters self-adaptive.
#'
#' The `OperatorCombination` operators combine operators for different subspaces of individuals by wraping other [`MiesOperator`]s given during construction.
#' Different [`MiesOperator`]s are assigned to different components or sets of components and operate on them independently of the rest of the components
#' or the other operators. An operator can be assigned to a single component by giving it in `operators` with the name of the component, or to multiple components by
#' giving it in `operators` with the name of a *group*. Groups are created by the `groups` argument, but several default groups that catch components by type
#' exist.
#'
#' @details
#'
#' Operators can be made *self-adaptive* by coupling their hyperparameter values to values in individuals. This is done by giving functions in `adaptions`; these
#' functions are executed for each individual before an operator is applied, and the result given to a named operator hyperparameter.
#'
#' `OperatorCombination` is the base class from which `MutatorCombination` and `RecombinatorCombination` inherit. The latter two are to be used for [`Mutator`] and
#' [`Recombinator`] objects, respectively.
#'
#' Besides groups created with the `groups` construction argument, there are special groups that all unnamed operators fall into based on their [`Param`][paradox::Param]
#' class: `"ParamInt"`, `"ParamDbl"`, `"ParamFct"`, and `"ParamLgl"`. A component of an individual that is not named directly in `operators` or made part of a group
#' in `groups` is automatically in one of these special groups. There is furthermore a special catch-all group `"ParamAny"`, which catches all components that are
#' are not operated directly, not in a group, and not in another special group that is itself named directly or in a group. I.e., all components that would otherwise
#' have no assigned operation.
#'
#' `RecombinatorCombination` can only combine operators where `$n_indivs_in` and `$n_indivs_out` can be combined. This is
#' currently supported either when `$n_indivs_in` and `$n_indivs_out` for each operator are the same (but `$n_indivs_in` may be unequal `$n_indivs_out` in
#' eacho of them); or when `$n_indivs_in` is equal to `$n_indivs_out` for each operator and the set of all `$n_indivs_in` that occur contains `1` and one more integer.
#' `$n_indivs_in` and `$n_indivs_out` for the resulting [`RecombinatorCombination`] operator will be set the maximum of occuring `$n_indivs_in` and `$n_indivs_out`,
#' respectively.
#'
#' @section Supported Operand Types:
#'
#' Supported [`Param`][paradox::Param] classes are calculated based on the supported classes of the wrapped operators.
#' They are frequently just the set union of supported classes, unless inference can be drawn from type-specific groups that an operator is assigned to.
#' If e.g. an operator that supports [`ParamDbl`][paradox::ParamDbl] and [`ParamInt`][paradox::ParamInt] is assigned to group `"ParamInt"`, and
#' an operator that supports [`ParamLgl`][paradox::ParamLgl] is assigned to component `"a"`, then the result will support [`ParamLgl`][paradox::ParamLgl] and
#' [`ParamInt`][paradox::ParamInt] only.
#'
#' @section Hyperparameters:
#'
#' The `OperatorCombination` has the hyperparameters of all encapsulated [`MiesOperator`]s, minus the hyperparameters that are named in the `adaptions`.
#' Hyperparameter names are prefixed with the name of the [`MiesOperator`] in the `operators` list.
#'
#' @templateVar id combine
#' @templateVar additional , <operators>, ...
#' @template autoinfo_prepare_mut
#' @template autoinfo_dict
#' @template autoinfo_prepare_rec
#' @template autoinfo_dict
#'
#' @param operators (named `list` of [`MiesOperator`])\cr
#'   List of operators to apply to components of individuals. Names are either names of individual components, or group names which are either as defined
#'   through `groups` or special groups. Individual components can only be member of either a (non-special) group or named in `operators`, so a name
#'   that occurs in `operators` may not be a member of a group as defined in `groups`.\cr
#'   The `$operators` field will reflect this value.
#' @param groups (named `list` of `character`)\cr
#'   List of groups that operators can act on. Names of this list define new groups. The content of each list element contains the names of
#'   components or special groups (a [`Param`][paradox::Param] subclass name or `"ParamAny"`) to subsume under the group.
#'   Individual components can only be member of either a (non-special) group or named in `operators`, so a name
#'   that occurs in `operators` may not be a member of a group as defined in `groups`. The default is the empty list.\cr
#'   The `$groups` field will reflect this value.
#' @param adaptions (named `list` of `function`)\cr
#'   List of functions used for self-adaption of operators. The names of the list must be names of hyperparameters of wrapped operators, prefixed
#'   with the corresponding name in the `operators` list. This is the same name as the hyperparameter would otherwise have if exposed by the
#'   `OperatorCombination` object. The values in the list must be functions that receive a single input, the individual or individuals being operated on,
#'   as a [`data.table`][data.table::data.table]. It must return a value that is then assigned to the hyperparameter of the operator to which it pertains.
#'   Note that [`MutatorCombination`] adaption functions are always called with a [`data.table`][data.table::data.table] containing a single row, while
#'   [`RecombinatorCombination`] adaption functions are called with [`data.table`][data.table::data.table]s with multiple rows according to `$n_indivs_in`.
#'   In both cases, the return value must be a scalar. The default is the empty list.\cr
#'   The `$adaption` field will reflect this value.
#' @param binary_fct_as_logical (`logical(1)`)\cr
#'   Whether to treat binary [`ParamFct`][paradox::ParamFct] components of [`ParamSet`][paradox::ParamSet]s as [`ParamLgl`][paradox::ParamLgl] with respect
#'   to the special groups `"ParamLgl"` and `"ParamFct"`. This does *not* perform any conversion, so a [`MiesOperator`] assigned to the `"ParamLgl"` special
#'   group when `binary_fct_as_logical` is `TRUE` and there are binary [`ParamFct`][paradox::ParamFct]s present will receive
#'   a factorial value and must also support [`ParamFct`][paradox::ParamFct] in this case. This is checked during `$prime()`, but not during construction.
#'   Default is `FALSE`.\cr
#'   The `$binary_fct_as_logical` field will reflect this value.
#' @param on_type_not_present (`character(1)`)\cr
#'   Action to perform during `$prime()` when an operator is assigned to a type special group but there is no component available that falls in this group, either
#'   because no components of the respective type are present, or because all these components are also directly named in `operators` or in `groups`.
#'   One of `"quiet"` (do nothing), `"warn"` (give warning, default), or `"stop"` (generate an error).\cr
#'   The writable `$on_type_not_present` field will reflect this value.
#' @param on_name_not_present (`character(1)`)\cr
#'   Action to perform during `$prime()` when an operator is assigned to a specifically named component, but the component is not present.
#'   One of `"quiet"` (do nothing), `"warn"` (give warning), or `"stop"` (generate an error, default).\cr
#'   The writable `$on_name_not_present` field will reflect this value.
#' @param granularity (`integer(1)`)\cr
#'   At what granularity to query `adaptions` for sets of individuals. Functions in `adaptions` are always called once per `granularity` individuals
#'   in input `values`, and the function argument in these calls will then have `granularity` number of rows. This is used internally, it is set to
#'   1 for `MutatorCombination`, and to `$n_indivs_in` for `RecombinatorCombination`.
#'
#' @family base classes
#' @family mutators
#' @family mutator wrappers
#' @family recombinators
#' @family recombinator wrappers
#'
#' @export
OperatorCombination = R6Class("OperatorCombination",
  inherit = MiesOperator,
  public = list(
    #' @description
    #' Initialize the `OperatorCombination` object.
    initialize = function(operators, groups = list(), adaptions = list(), binary_fct_as_logical = FALSE, on_type_not_present = "warn", on_name_not_present = "stop", granularity = 1) {
      private$.granularity = assert_int(granularity, lower = 1, tol = 1e-100)
      private$.on_name_not_present = assert_choice(on_name_not_present, c("quiet", "warn", "stop"))
      private$.on_type_not_present = assert_choice(on_type_not_present, c("quiet", "warn", "stop"))
      private$.binary_fct_as_logical = assert_flag(binary_fct_as_logical)
      private$.operators = assert_list(operators, types = "MiesOperator", any.missing = FALSE, names = "unique", min.len = 1)
      private$.groups = assert_list(groups, types = "character", names = "unique", any.missing = FALSE)
      private$.adaptions = assert_list(adaptions, types = "function", names = "unique", any.missing = FALSE)

      lapply(adaptions, assert_function, nargs = 1)

      lapply(groups, assertCharacter, min.chars = 1, any.missing = FALSE, unique = TRUE)
      allgroups = unlist(groups, use.names = FALSE)
      if (anyDuplicated(allgroups)) {
        stopf("Dimension referenced in more than one group: %s", str_collapse(unique(allgroups[duplicated(allgroups)])))
      }
      if (any(allgroups %in% names(groups))) {
        stopf("No recursive groups allowed: %s", str_collapse(intersect(allgroups, names(groups))))
      }
      if (any(allgroups %in% names(operators))) {
        stopf("Dimensions %s that have their own operator must not be present in groups.",
          str_collapse(intersect(allgroups, names(operators))))
      }
      if (any(names(groups) %nin% names(operators))) {
        stopf("No operator for group(s) %s", str_collapse(setdiff(names(groups), names(operators))))
      }
      types = c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl", "ParamAny")
      if (any(names(groups) %in% types)) {
        stop('Special group names "ParamInt", "ParamDbl", "ParamFct", "ParamLgl", "ParamAny" may not be used.')
      }

      param_classes_list = list()
      for (on in names(operators)) {
        private$.operators[[on]] = private$.operators[[on]]$clone(deep = TRUE)
        op = private$.operators[[on]]
        op$param_set$set_id = on

        if (on %in% types) {
          if (on == "ParamAny") {
            # we know that the asterisk may apply to any type that is not otherwise referenced.
            # We can therefore narrow pcl down to the supported class of the operator that is
            # not otherwise referenced and could theory determine if this leaves an empty and if so
            # throw an error. But that would mean we could make a valid combination invalid just by
            # *adding* another operator, so we don't do that here.
            on = op$param_classes
          } else if (on %nin% op$param_classes) {
            stopf("Operator for %s does not support the type.", on)
          }
          pcl = on
        } else if (on %in% names(groups)) {
          neededtypes = intersect(types, groups[[on]])
          if (any(setdiff(neededtypes, "ParamAny") %nin% op$param_classes)) {
            stopf("Operator for group %s does not support type(s) %s.",
              on, str_collapse(setdiff(neededtypes, op$param_classes)))
          }
          if (all(groups[[on]] %in% setdiff(types, "ParamAny"))) {
            pcl = groups[[on]]
          } else {
            pcl = op$param_classes
          }
        } else {
          pcl = op$param_classes
        }
        if (binary_fct_as_logical && "ParamLgl" %in% pcl && "ParamFct" %in% op$param_classes) {
          # handle the case where an operator assigned to ParamLgl handles ParamFct when binary_fct_as_logical is TRUE.
          # Ordinarily an operator assigned to ParamX will only add ParamX to the `param_classes`, but for this case
          # ParamLgl *and* ParamFct must be added (if the operator can handle ParamFct).
          pcl = c(pcl, "ParamFct")
        }

        param_classes_list[[length(param_classes_list) + 1]] = pcl
      }
      params = ParamSetCollection$new(map(self$operators, "param_set"))$ids()
      if (any(names(adaptions) %nin% params)) {
        stopf("Adaption for %s which is not an operator parameter.",
          str_collapse(setdiff(names(adaptions), params)))
      }

      super$initialize(
        param_classes = unique(unlist(param_classes_list, use.names = FALSE)),
        param_set = alist(
          ParamSetShadow$new(
            ParamSetCollection$new(map(self$operators, "param_set")),
            names(self$adaptions)
          )
        )
      )
    },
    #' @description
    #' See [`MiesOperator`] method. Primes both this operator, as well as the wrapped operators
    #' given to `operators` during construction. Priming of wrapped operators happens according
    #' to component assignments to wrapped operators.
    #' @param param_set ([`ParamSet`][paradox::ParamSet])\cr
    #'   Passed to [`MiesOperator`]`$prime()`.
    #' @return [invisible] `self`.
    prime = function(param_set) {
      super$prime(param_set)
      types = c("ParamInt", "ParamDbl", "ParamFct", "ParamLgl")  # special types
      specialgroups = c(types, "ParamAny")  # pseudogroups
      groupnames = c(specialgroups, names(self$groups))  # names of groups, including type-pseudogroups

      ids = param_set$ids()
      classes = param_set$class
      if (self$binary_fct_as_logical) {
        classes[param_set$nlevels == 2] = "ParamLgl"
      }

      if (any(groupnames %in% ids)) {
        stop("groupnames / Param class names and ids of param_set must be disjoint")
      }

      capturing = c(setdiff(names(self$operators), names(self$groups)), unlist(self$groups))  # dimensions that are captured either directly or through groups
      if (any(setdiff(capturing, specialgroups) %nin% ids)) {
        switch(self$on_name_not_present,
          "quiet" = function(...) NULL,
          "warn" = warningf,
          "stop" = stopf)(
          "Named operators %s have no corresponding dimension.",
          str_collapse(setdiff(capturing, c(specialgroups, ids))))
      }
      type_captured_ids = ids[ids %nin% capturing]  # dimensions that are captured through types
      captured_types = classes[ids %nin% capturing]  # ... and their types
      if ("ParamAny" %in% capturing) {  # if we have an ParamAny-group, then it gets all the leftovers
        captured_types[captured_types %nin% capturing] = "ParamAny"
      }
      type_mapping = sapply(unique(captured_types), function(x) type_captured_ids[captured_types == x], simplify = FALSE)  # named list: type -> uncaptured dimensions of that type

      if (any(intersect(capturing, specialgroups) %nin% captured_types)) {
        switch(self$on_type_not_present,
          "quiet" = function(...) NULL,
          "warn" = warningf,
          "stop" = stopf)(
          "Operators for types / special groups %s have no corresponding dimensions (or dimensions are overriden).",
          str_collapse(setdiff(intersect(capturing, specialgroups), captured_types)))
      }
      if (any(captured_types %nin% capturing)) {  # uncaptured dimension is not captured through pseudogroup
        badtypes = setdiff(unique(captured_types), capturing)
        stopf("No operators for dimensions %s of types %s",
          str_collapse(type_captured_ids[captured_types %in% badtypes]),
          str_collapse(badtypes))
      }

      # mapping: name of operator in self$operators --> name of dimensions this operator captures
      mapping = c(
        keep(type_mapping[names(self$operators)], function(x) length(x) > 0),  # type (i.e. pseudogroup) capture
        sapply(self$groups, function(g) intersect(c(setdiff(g, types), unlist(type_mapping[g], use.names = FALSE)), ids), simplify = FALSE),  # group capture
        sapply(intersect(names(self$operators), ids), identity, simplify = FALSE)  # direct capture. No groups remain here because ids and groupnames are disjoint (checked above)
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
    #' @field operators (named `list` of [`MiesOperator`])\cr
    #' List of operators to apply to components of individuals, as set during construction. Read-only.
    operators = function(val) {
      if (!missing(val)) stop("operators is read-only.")
      private$.operators
    },
    #' @field groups (named `list` of `character`)\cr
    #' List of groups that operators can act on, as set during construction. Read-only.
    groups = function(val) {
      if (!missing(val)) stop("groups is read-only.")
      private$.groups
    },
    #' @field adaptions (named `list` of `function`)\cr
    #' List of functions used for self-adaption of operators, as set during construction. Read-only.
    adaptions = function(val) {
      if (!missing(val)) stop("adaptions is read-only.")
      private$.adaptions
    },
    #' @field binary_fct_as_logical (`logical(1)`)\cr
    #' Whether to treat binary [`ParamFct`][paradox::ParamFct] components of [`ParamSet`][paradox::ParamSet]s as [`ParamLgl`][paradox::ParamLgl] with respect
    #' to the special groups `"ParamLgl"` and `"ParamFct"`, as set during construction. Read-only.
    binary_fct_as_logical = function(val) {
      if (!missing(val)) stop("binary_fct_as_logical is read-only.")
      private$.binary_fct_as_logical
    },
    #' @field on_type_not_present (`character(1)`)\cr
    #' Action to perform during `$prime()` when an operator is assigned to a type special group but there is no component available that falls in this group.
    #' See the construction argument. Can be changed during the object's lifetime.
    on_type_not_present = function(val) {
      if (!missing(val)) {
        private$.on_type_not_present = assert_choice(val, c("quiet", "warn", "stop"))
      }
      private$.on_type_not_present
    },
    #' @field on_name_not_present (`character(1)`)\cr
    #' Action to perform during `$prime()` when an operator is assigned to a specifically named component, but the component is not present.
    #' See the construction argument. Can be changed during the object's lifetime.
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
    .adaptions = NULL,
    .binary_fct_as_logical = NULL,
    .on_type_not_present = NULL,
    .on_name_not_present = NULL,
    .mapping = NULL,
    .granularity = NULL,
    .operate = function(values, ...) {
      granularity = if (!length(private$.adaptions)) nrow(values) else private$.granularity
      assert_true(nrow(values) %% granularity == 0)
      rbindlist(
        lapply(split(values, rep(seq_len(nrow(values) / granularity), each = granularity)), function(vs) {
          adaption_values = lapply(private$.adaptions, function(f) f(vs))
          self$param_set$origin$values = insert_named(self$param_set$origin$values, adaption_values)
          do.call(cbind, unname(imap(private$.mapping, function(pars, op) {
            self$operators[[op]]$operate(vs[, match(pars, names(vs), 0), with = FALSE])
          })))
        }),
        use.names = TRUE
      )
    }
  )
)

#' @rdname OperatorCombination
#' @aliases dict_mutators_combine
#' @export
MutatorCombination = R6Class("MutatorCombination",
  inherit = OperatorCombination,
  public = list(
    #' @description
    #' Initialize the `MutatorCombination` object.
    #' @param operators see above.
    #' @param groups see above.
    #' @param adaptions see above.
    #' @param binary_fct_as_logical see above.
    #' @param on_type_not_present see above.
    #' @param on_name_not_present see above.
    initialize = function(operators = list(), groups = list(), adaptions = list(), binary_fct_as_logical = FALSE, on_type_not_present = "warn", on_name_not_present = "stop") {
      assert_list(operators, types = "Mutator")
      super$initialize(operators = operators, groups = groups, adaptions = adaptions, binary_fct_as_logical = binary_fct_as_logical, on_type_not_present = on_type_not_present, on_name_not_present = on_name_not_present)
      class(self) = c("Mutator", class(self))
    }
  )
)
dict_mutators$add("combine", MutatorCombination)

#' @rdname OperatorCombination
#' @aliases dict_recombinators_combine
#' @export
RecombinatorCombination = R6Class("RecombinatorCombination",
  inherit = OperatorCombination,
  public = list(
    #' @description
    #' Initialize the `RecombinatorCombination` object.
    #' @param operators see above.
    #' @param groups see above.
    #' @param adaptions see above.
    #' @param binary_fct_as_logical see above.
    #' @param on_type_not_present see above.
    #' @param on_name_not_present see above.
    initialize = function(operators = list(), groups = list(), adaptions = list(), binary_fct_as_logical = FALSE, on_type_not_present = "warn", on_name_not_present = "stop") {
      assert_list(operators, types = "Recombinator")
      inout = map_dtr(operators, function(o) list(nin = o$n_indivs_in, nout = o$n_indivs_out))
      if (nrow(unique(inout)) == 1) {
        private$.n_indivs_in = inout$nin
        private$.n_indivs_out = inout$nout
      } else if (length(setdiff(inout$nin, 1)) == 1 && all(inout$nout == inout$nin)) {
        private$.n_indivs_in = max(inout$nin)
        private$.n_indivs_out = max(inout$nout)
      } else {
        stop("Combining operators with disagreeing n_indivs_in / n_indivs_out where more than one operator has these values not equal 1 is not yet supported.")
      }
      super$initialize(operators = operators, groups = groups, adaptions = adaptions, binary_fct_as_logical = binary_fct_as_logical, on_type_not_present = on_type_not_present, on_name_not_present = on_name_not_present, granularity = self$n_indivs_in)
      class(self) = c("Recombinator", class(self))
    }
  ),
  # --- copy-paste from Recombinator.R
  active = list(
    #' @field n_indivs_in (`integer(1)`)\cr
    #' Number of individuals to consider at the same time. When operating, the number of input individuals must be divisible by this number.
    n_indivs_in = function(val) {
      if (!missing(val)) stop("n_indivs_in is read-only.")
      private$.n_indivs_in
    },
    #' @field n_indivs_out (`integer(1)`)\cr
    #' Number of individuals produced for each group of `$n_indivs_in` individuals.
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
dict_recombinators$add("combine", RecombinatorCombination)