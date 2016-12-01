#' @title Simulate
#'
#'
simulate <- function(psa = 1, iter, n, a, b, mu, sigma, plot = TRUE) {
  judges <- getJudges(iter)
  arrestees <- getArr(iter, n, a, b, mu, sigma)
  arrestees[, judge_id := sample(judges$judge_id, size = length(arr_id), replace = TRUE)]

  dt <- merge(arrestees, judges[, list(iter, judge_id, bin, release, grp, psa_flag)], by=c("iter", "judge_id"))
  dt[grp == "Half", psa_flag := as.numeric(runif(.N) < .5)]
  dt[, obs_risk := true_risk + psa_flag * noise * psa + (1 - psa_flag) * noise]
  dt[, decision := ecdf(obs_risk)(obs_risk) < release, by="iter,judge_id"]
  dt[, psa := psa]

  return(dt)
}
