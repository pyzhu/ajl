#'
#'
#' @import testthat
getArr <- function(n, num.obs.nonwhite, a, b, mu, sigma, k, theta) {
  # throw an error if num.obs.nonwhite is greater than n
  assert_that(n > num.obs.nonwhite)  
  
  dt <- data.table(arr_id = seq.int(n), key="arr_id")
  # setkey(dt, arr_id)
  dt[, true_risk := rbeta(.N, a, b)]
  
  # Set biases according to race, nonwhite = 1
  # no systematic bias if white
  dt[, nonwhite := c(rep(TRUE, num.obs.nonwhite), rep(FALSE, .N-num.obs.nonwhite))]
  dt[, noise := rnorm(.N, mu * as.numeric(nonwhite), sigma), by=nonwhite] 

  # given the true risk, draw from binomial
  # TODO: would give false if true_risk = 1
  dt[, misbehave := runif(.N) < true_risk]

  return(dt)
}
