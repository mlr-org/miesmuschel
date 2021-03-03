

source("setup.R", local = TRUE)

# flattening ParamSetCollection
ps1 = ps(x = p_dbl(1, 2))
ps1$set_id = "a"
ps1$deps = data.table(id = character(0), on = character(0), cond = list())
ps2 = ps(y = p_dbl(-1, 1))
ps2$set_id = "b"

psboth = ps(a.x = p_dbl(1, 2), b.y = p_dbl(-1, 1))
psboth$values = list()
psboth$deps = data.table(id = character(0), on = character(0), cond = list())

fullps_collection = ParamSetCollection$new(list(ps1, ps2))
fullps_flat = ps_flatten(fullps_collection)
fullps_flat$values = list()
fullps_flat$deps = data.table(id = character(0), on = character(0), cond = list())

expect_equal(fullps_flat, psboth)

# flattening ParamSetShadow

psboth_noprefix = ps(x = p_dbl(1, 2), y = p_dbl(-1, 1))
psboth_noprefix$deps = data.table(id = character(0), on = character(0), cond = list())
ps1_shadow = ParamSetShadow$new(psboth_noprefix, "y")
ps1_shadow$set_id = "a"
ps1_flattened = ps_flatten(ps1_shadow)
ps1_flattened_noclone = ps_flatten(ps1_shadow, clone = FALSE)
ps1_flattened$values = list()

expect_equal(ps1_flattened, ps1)
expect_false(data.table::address(ps1_flattened$deps) == data.table::address(ps1_shadow$deps))

expect_true(identical(ps1, ps_flatten(ps1, clone = FALSE)))
expect_false(identical(ps1, ps_flatten(ps1, clone = TRUE)))



