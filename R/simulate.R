#' @title Simulate
#'
#'
simulate <- function(tau = 1, release.rate, iter, n, a, b, mu, sigma, k = .32, theta = 30, plot = TRUE) {
  judges <- getJudges(iter)
  arrestees <- getArr(iter, n, a, b, mu, sigma, k = k, theta = theta)
  # arrestees[, judge_id := sample(judges$judge_id, size = length(arr_id), replace = TRUE)]
  dt <- data.table:::merge.data.table(CJ(arr_id = seq.int(n), tau = tau), arrestees, by="arr_id")
  # dt <- merge(arrestees, judges[, list(iter, judge_id, bin, release, grp, psa_flag)], by=c("iter", "judge_id"))
  # add in the scalars
  dt[, tau := tau]
  dt[, release_rate := release.rate]

  # dt[, treated := as.numeric(sample(seq.int(.N) <= .N/2))]
  # dt[, psa_flag := as.numeric(sample(seq.int(.N) <= .N/2))]
  dt[, obs_risk := true_risk + treated * noise * tau + (1 - treated) * noise]
  dt[, released := ecdf(obs_risk)(obs_risk) <= release_rate, by="iter,treated"]

  # add days incarcerated according to a gamma distribution
  # using ceiling to get minimum 1 day
  dt[, days_incarc := ceiling(rgamma(.N, shape = k, scale = theta))]

  return(dt)
}
