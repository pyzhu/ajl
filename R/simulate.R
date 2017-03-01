#' @title Simulate
#'
#'
simulate <- function(tau = 1, release.rate, iter, n, a, b, mu, sigma, k = .32, theta = 77, plot = TRUE) {
  judges <- getJudges(iter)
  arrestees <- getArr(iter, n, a, b, mu, sigma, k = k, theta = theta)
  # arrestees[, judge_id := sample(judges$judge_id, size = length(arr_id), replace = TRUE)]
  # TODO: cut out the repetitive data
  arr_id_treated <- arrestees[treated == 1, arr_id]
  dt <- rbind(CJ(arr_id = arr_id_treated, tau = tau), arrestees[treated == 0, list(arr_id, tau = 1)], use.names = TRUE)
  dt <- data.table:::merge.data.table(dt, arrestees, by="arr_id")
  # dt <- data.table:::merge.data.table(CJ(arr_id = seq.int(n), tau = tau), arrestees, by="arr_id")
  # add in the scalars
  dt[, tau := tau]
  dt[, release_rate := release.rate]

  # dt[, treated := as.numeric(sample(seq.int(.N) <= .N/2))]
  # dt[, psa_flag := as.numeric(sample(seq.int(.N) <= .N/2))]
  dt[, obs_risk := true_risk + treated * noise * tau + (1 - treated) * noise]
  dt[, released := ecdf(obs_risk)(obs_risk) <= release_rate, by="treated,tau"]

  return(dt)
}
