#' @title
#' @description In the half experimental group, the treated and control groups failure rates and compared
test_1 <- function(decisions) {
  stopifnot(inherits(decisions, "data.table"))
  stopifnot("grp" %in% colnames(decisions))

  dt <- decisions[grp == "Half", list(failure = sum(misbehave * decision) / .N), by="iter,psa_flag"]
  dcast.data.table(dt, ... ~ psa_flag, value.var = "failure") %>>%
    (ks.test(.[["0"]], .[["1"]]))
}

test_2 <- function(decisions) {
  stopifnot(inherits(decisions, "data.table"))
  dt <- copy(decisions)

  dt[, list()]
}
