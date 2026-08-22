setwd(here::here("monarch/tourism/projection"))
# targets::tar_make(reporter = "timestamp", use_crew = TRUE)
targets::tar_make_future(workers = 80, reporter = "timestamp")
source("output.R")

if (FALSE) {
  # if the process is stopped manually, clean up orphan processes
  process <- ps::ps(columns = c("pid", "cmdline"))
  tools::pskill(process$pid[grepl("callr", process$cmdline)])
}
