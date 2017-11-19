#'
#'
#' @import testthat
getArr <- function(n, num.obs.nonwhite, a, b, mu, sigma, k, theta) {
  # throw an error if num.obs.nonwhite is greater than n
  stopifnot(n > num.obs.nonwhite)  
    
  dt <- data.table(arr_id = seq.int(n), key="arr_id")
  # setkey(dt, arr_id)
  dt[, true_risk := rbeta(.N, a, b)]
  
  # Set biases according to race, nonwhite = 1
  dt[, nonwhite := c(rep(TRUE, num.obs.nonwhite), rep(FALSE, .N-num.obs.nonwhite))]
  dt[, noise := rnorm(.N, mu * as.numeric(nonwhite), sigma), by=nonwhite] # no systematic bias if white

  # given the true risk, draw from binomial
  # TODO: would give false if true_risk = 1
  dt[, misbehave := runif(.N) < true_risk]
  dt[, days_incarc := ceiling(rgamma(.N, shape = k, scale = theta))]

  # cap the days_incarc to 240
  dt[days_incarc > 240, days_incarc := 240]

  dt[, treated := as.numeric(sample(seq.int(.N) <= .N/2))]

  # add days incarcerated according to a gamma distribution
  # using ceiling to get minimum 1 day
  dt[, days_incarc := ceiling(rgamma(.N, shape = k, scale = theta))]

  return(dt)
}
