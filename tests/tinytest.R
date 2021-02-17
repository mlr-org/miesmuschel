
if (requireNamespace("tinytest", quietly = TRUE)) {
  library("bbotk")
  library("mlr3tuning")
  library("data.table")
  tinytest::test_package("miesmuschel", at_home = identical(Sys.getenv("NOT_CRAN"), "true"), ncpu = 2)
}
