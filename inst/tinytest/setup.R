
using("checkmate")
checkmate::register_test_backend("tinytest")

library("data.table")
loadNamespace("mlr3")
lgr::get_logger("mlr3")$set_threshold("warn")

for (helper in list.files(pattern = "^helper_.*\\.R$")) {
  source(helper, local = TRUE)
}
