
if (requireNamespace("tinytest", quietly = TRUE)) {
  tinytest::test_package("miesmuschel", at_home = identical(Sys.getenv("NOT_CRAN"), "true"), ncpu = 2)
  Sys.sleep(5)  # wait for parallel workers to quit
}
