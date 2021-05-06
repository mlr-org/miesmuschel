library(mlr3viz)
source("attic/irace_omni.R")
lgr::get_logger("bbotk")$set_threshold("warn")

# 05.05.2021
# 52*10*8 smashy budget 
# 300 irace budget
dir.create("./attic/data_05_05")
instance_file = "./attic/data_05_05/irace_instance_05_05.rda"

res = optimize_irace(c("val_cross_entropy", "val_balanced_accuracy"), c("test_cross_entropy", "test_balanced_accuracy"), "OpenML_task_id", cfg, 300, instance_file)
instance = readRDS("attic/data_05_05/irace_instance_05_05.rda")

autoplot(instance, type = "parallel")

# 06.05.2021
# 52*100*8 smashy budget 
# 1000 irace budget
dir.create("./attic/data_06_05")
instance_file = "./attic/data_06_05/irace_instance_06_05.rda"
log_file = "./attic/data_06_05/irace_log.Rdata"

res = optimize_irace(c("val_cross_entropy", "val_balanced_accuracy"), c("test_cross_entropy", "test_balanced_accuracy"), "OpenML_task_id", cfg, 1000, instance_file, log_file)

# 06.05.2021
# 52*100*8 smashy budget 
# 3000 irace budget
dir.create("./attic/data_06_05_b3000")
instance_file = "./attic/data_06_05_b3000/irace_instance_06_05_b3000.rda"
log_file = "./attic/data_06_05_b3000/irace_log.Rdata"

res = optimize_irace(c("val_cross_entropy", "val_balanced_accuracy"), c("test_cross_entropy", "test_balanced_accuracy"), "OpenML_task_id", cfg, 3000, instance_file, log_file)

# 06.05.2021
# test
dir.create("./attic/data_test")
instance_file = "./attic/data_test/irace_instance_test.rda"
log_file = "./attic/data_test/irace_log.Rdata"

future::plan("multicore", workers = 8)

res = optimize_irace(c("val_cross_entropy", "val_balanced_accuracy"), c("test_cross_entropy", "test_balanced_accuracy"), "OpenML_task_id", cfg, 300, instance_file, log_file)
instance = readRDS("attic/data_test/irace_instance_test.rda")
