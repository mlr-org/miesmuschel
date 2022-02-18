#' @import mlr3misc
#' @import checkmate
#' @import bbotk
#' @import paradox
#' @import R6
#' @import data.table
#'
#' @description
#'
#' SMASHY description # TODO
"_PACKAGE"

lg = NULL

reg_bbotk = function(...) {  # nocov start
  mlr_optimizers = utils::getFromNamespace("mlr_optimizers", ns = "bbotk")
  mlr_optimizers$add("smashy", OptimizerSmashy)
}  # nocov end

reg_mlr3tuning = function(...) {  # nocov start
  if (requireNamespace("mlr3tuning", quietly = TRUE)) {
    mlr_tuners = utils::getFromNamespace("mlr_tuners", ns = "mlr3tuning")
    mlr_tuners$add("smashy", TunerSmashy)
  }
}  # nocov end

reg_mlr3 = function(...) {  # nocov start
  if (requireNamespace("mlr3", quietly = TRUE)) {
    mlr_reflections = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
    if (!"density" %in% mlr_reflections$task_types$type) {
      mlr_reflections$task_types = setkeyv(rbind(mlr_reflections$task_types, rowwise_table(
        ~type, ~package, ~task, ~learner, ~prediction, ~measure,
        "density", "smashy", "TaskDensity", "LearnerDensity", "PredictionDensity", "MeasureDensity"
      )), "type")
      mlr_reflections$task_col_roles$density = setdiff(mlr_reflections$task_col_roles$regr, "target")
      mlr_reflections$task_properties$density = mlr_reflections$task_properties$regr
      mlr_reflections$learner_properties$density = c(mlr_reflections$learner_properties$regr, "sample")
      mlr_reflections$measure_properties$density = mlr_reflections$measure_properties$regr
      mlr_reflections$learner_predict_types$density = list(prob = "prob")
      mlr_reflections$default_measures$density = "density.logloss"
    }
    mlr3::mlr_learners$add("density.featureless", LearnerDensityFeatureless)
    mlr3::mlr_learners$add("density.np", LearnerDensityNP)
    mlr3::mlr_tasks$add("faithful", load_faithful)
    mlr3::mlr_measures$add("density.logloss", MeasureDensityLogloss)
  }
}  # nocov end

reg_mlr3pipelines = function(...) {  # nocov start
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    mlr3pipelines::mlr_pipeops$add("densityratio", PipeOpDensityRatio)
    mlr3pipelines::mlr_pipeops$add("densitysplit", PipeOpDensitySplit)
    mlr3pipelines::mlr_pipeops$add("stratify", PipeOpStratify)
    mlr3pipelines::mlr_pipeops$add("predictionunion", PipeOpPredictionUnion)
    mlr3pipelines::add_class_hierarchy_cache(c("TaskDensity", "Task"))
    mlr3pipelines::add_class_hierarchy_cache(c("PredictionDensity", "Prediction"))
    mlr3pipelines::add_class_hierarchy_cache(c("LearnerDensity", "Learner"))
    mlr3pipelines::add_class_hierarchy_cache(c("MeasureDensity", "Measure"))
  }
}  # nocov end

# TODO register with miesmuschel

.onLoad = function(libname, pkgname) {  # nocov start
  reg_bbotk()
  reg_mlr3tuning()
  reg_mlr3()
  reg_mlr3pipelines()

  if (is.null(paradox::ps()$context_available) ||  # need paradox context variable
      !isTRUE(tryCatch({SamplerRandomWeights$new() ; TRUE}, error = function(e) FALSE))) {  # need samplers that allow ParamUty
    stop("Need paradox with ContextPV for this version of smashy. Do:
remotes::install_github(\"mlr-org/paradox@expression_params\")
and try again.")
  }

  assign("lg", lgr::get_logger(pkgname), envir = parent.env(environment()))
  setHook(packageEvent("bbotk", "onLoad"), reg_bbotk, action = "append")
  setHook(packageEvent("mlr3tuning", "onLoad"), reg_mlr3tuning, action = "append")
  setHook(packageEvent("mlr3", "onLoad"), reg_mlr3, action = "append")
  setHook(packageEvent("mlr3pipelines", "onLoad"), reg_mlr3pipelines, action = "append")
  options(miesmuschel.testing = getOption("miesmuschel.testing") %??% FALSE)
}  # nocov end

.onUnload = function(libpath) {  # nocov start
  for (pkg in c("bbotk", "mlr3tuning", "mlr3", "mlr3pipelines")) {
    load_event = packageEvent(pkg, "onLoad")
    setHook(load_event,
      keep(getHook(load_event), function(x) environmentName(topenv(environment(x))) != "smashy"),
      action = "replace"
    )
  }
}  # nocov end


# static code checks should not complain about commonly used data.table columns
utils::globalVariables(c("dob", "eol"))

if (!Sys.getenv("DEVTOOLS_LOAD") == "true") {
  leanify_package()
}

