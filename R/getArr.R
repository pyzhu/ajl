getArr <- function(iter, n, a, b, mu, sigma, k, theta) {
  dt <- CJ(iter = seq.int(iter), arr_id = seq.int(n))
  setkey(dt, iter, arr_id)
  dt[, `:=`(true_risk = rbeta(.N, a, b)
            , noise = rnorm(.N, mu, sigma))]

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
