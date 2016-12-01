getArr <- function(iter = 2000, n = 4000, a = .55, b = .2, mu = 0, sigma = .25) {
  dt <- CJ(iter = seq.int(iter), arr_id = seq.int(n))
  setkey(dt, iter, arr_id)
  dt[, `:=`(true_risk = rbeta(.N, a, b)
            , noise = rnorm(.N, mu, sigma))]

  # given the true risk, draw from binomial
  # TODO: would give false if true_risk = 1
  dt[, misbehave := runif(.N) < true_risk]

  return(dt)
}
