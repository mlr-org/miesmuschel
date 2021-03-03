
using("checkmate")
checkmate::register_test_backend("tinytest")

library("data.table")

for (helper in list.files(pattern = "^helper_.*\\.R$")) {
  source(helper, local = TRUE)
}
