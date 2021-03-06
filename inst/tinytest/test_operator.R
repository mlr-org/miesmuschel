
source("setup.R", local = TRUE)

op = MiesOperator$new()

expect_read_only(op, c("param_classes", "endomorphism"))
