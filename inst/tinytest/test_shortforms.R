
source("setup.R", local = TRUE)

sp = SelectorProxy$new()
expect_equal(sel("proxy"), sp)
expect_equal(sels(c("proxy", "random")), list(sp, SelectorRandom$new()))
sp$param_set$values$operation  = SelectorRandom$new()
expect_equal(sel("proxy", operation = sel("random")), sp)

expect_equal(rec("null"), RecombinatorNull$new())
expect_equal(rec("null", n_indivs_in = 2), RecombinatorNull$new(n_indivs_in = 2, n_indivs_out = 2))
rp = RecombinatorProxy$new()
expect_equal(rec("proxy"), rp)
expect_equal(recs(c("proxy", "null")), list(rp, RecombinatorNull$new()))
rp$param_set$values$operation = RecombinatorCrossoverUniform()
expect_equal(rec("proxy", operation = rec("xounif")), rp)
rp2 = RecombinatorMaybe$new(RecombinatorCrossoverUniform())
rp2$param_set$values$maybe.p = 0.987
expect_equal(rec("maybe", rec("xounif"), maybe.p = 0.987), rp2)

expect_equal(mut("null"), MutatorNull$new())
mm = MutatorMaybe$new(MutatorGauss$new())
mm$param_set$values$maybe.sdev = 3.141
expect_equal(mut("maybe", mut("gauss"), maybe.sdev = 3.141), mm)
mm$param_set$values$maybe.sdev = 0.5
mg = mut("gauss")
# problem here is that ParamSetCollection reorders $values, so we have to make the
# order explicit here as well.
mg$param_set$values = list(sdev = 0.5, sdev_is_relative = TRUE, truncated_normal = FALSE)
mmcmp = mut("maybe", mg)
mmcmp$param_set  # need to do this to get active bindings in the correct state
expect_equal(mmcmp, mm)
expect_equal(muts(c("null", "gauss")), list(MutatorNull$new(), MutatorGauss$new()))

expect_equal(mlr3tuning::tnr("mies"), TunerMies$new())
expect_equal(bbotk::opt("mies"), OptimizerMies$new())

expect_equal(bbotk::trm("gens"), TerminatorGenerations$new())
expect_equal(bbotk::trm("budget"), TerminatorBudget$new())

expect_equal(ftr("null"), FiltorNull$new())
fp = FiltorMaybe$new(filtor = FiltorNull$new())
fp$param_set$values$p = 0.7
expect_equal(ftr("maybe", p = 0.7, filtor = ftr("null")), fp)
expect_equal(ftrs(c("null", "proxy")), list(FiltorNull$new(), FiltorProxy$new()))
