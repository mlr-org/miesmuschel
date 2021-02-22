
library("paradox")
library("tinytest")
library("checkmate")
library("data.table")

devtools::document()
devtools::load_all()




pp = ps(a = p_dbl(0, 10), b = p_dbl(-10, 0))

des <- generate_design_random(pp, 1000)$data
des$a <- 1




plot(mut("gauss", sdev = 1, sdev_is_relative = FALSE, truncated_normal = FALSE)$prime(pp)$operate(des)$a)



pp = ps(a = p_lgl(), b = p_fct(letters[1:4]))
des <- generate_design_random(pp, 10)$data
des$a <- TRUE


mut("unif")$prime(pp)$operate(des)
mut("unif", can_mutate_to_same = FALSE, p = 1)$prime(pp)$operate(des)


rec()



rec("null")$prime(pp)$operate(des)

rec("xounif")$prime(pp)$operate(des)


pp = ps(a = p_lgl(), b = p_fct(letters[1:4]), c = p_dbl(0, 10))
des <- generate_design_random(pp, 10)$data
rec("xounif")$prime(pp)$operate(des)


tinytest::test_all()
