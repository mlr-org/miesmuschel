library(mlr3viz)
source("attic/irace_omni.R")
lgr::get_logger("bbotk")$set_threshold("warn")

# 05.05.2021
# 52*10*8 smashy budget 
dir.create("./attic/data_05_05")
instance_file = "./attic/data_05_05/irace_instance_05_05.rda"

res = optimize_irace(c("val_cross_entropy", "val_balanced_accuracy"), c("test_cross_entropy", "test_balanced_accuracy"), "OpenML_task_id", cfg, 300, instance_file)
instance = readRDS("attic/data_05_05/irace_instance_05_05.rda")

autoplot(instance, type = "parallel")

# 06.05.2021
# 52*100*8 smashy budget 
dir.create("./attic/data_06_05")
instance_file = "./attic/data_06_05/irace_instance_06_05.rda"
log_file = "./attic/data_06_05/irace_log.Rdata"

res = optimize_irace(c("val_cross_entropy", "val_balanced_accuracy"), c("test_cross_entropy", "test_balanced_accuracy"), "OpenML_task_id", cfg, 1000, instance_file, log_file)


