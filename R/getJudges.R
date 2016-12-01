#'
#' @import data.table
#' @importFrom pipeR "%>>%"
#' @importFrom dplyr ntile
getJudges <- function(iter = 2000) {
  dt.judges <- data.table(read.csv(system.file("extdata", "judges.csv", package="AJL")), key = "judge_id")

  dt <- CJ(iter = seq.int(iter), judge_id = unique(dt.judges$judge_id)) %>>%
    merge(dt.judges, by=c("judge_id"))

  # add bins
  dt[, bin := ntile(release, 9), by=iter]

  # assign each judge in each bin to an experimental group
  grps <- c("No PSA", "PSA", "Half")
  dt[, grp := sample(grps), by='iter,bin']

  # add the psa factors
  dt[grp == "No PSA", psa_flag := 0]
  dt[grp == "PSA", psa_flag := 1]
  # dt[grps == "Half", psa_flag := as.numeric(runif(.N) < .5)]

  return(dt)
}
