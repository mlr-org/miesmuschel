
using("checkmate")

library("data.table")

for (helper in list.files(pattern = "^helper_.*\\.R$")) {
  source(helper, local = TRUE)
}
