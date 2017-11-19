#' @title Simulate
#'
#'
simulate <- function(tau = 1, release.rate, n, num.obs.nonwhite, a, b, mu, sigma
    , k = .32, theta = 77, haphazard = FALSE, plot = TRUE) {
  # judges <- getJudges()
  arrestees <- getArr(n, num.obs.nonwhite, a, b, mu, sigma, k = k, theta = theta)

  # TODO: cut out the repetitive data
  # this should have been deprecated upon adding a tau4 = 0 column in the input
  arr_id_treated <- arrestees[treated == 1, arr_id]
  dt <- rbind(CJ(arr_id = arr_id_treated, tau = tau), arrestees[treated == 0, list(arr_id, tau = -1)], use.names = TRUE)
  dt <- data.table:::merge.data.table(dt, arrestees, by="arr_id")

  # add in the scalars
  dt[, release_rate := release.rate]
  
  if (haphazard) {
    # judges release completely randomly no reference to the obs_risk
    dt[, released := FALSE]
    dt[sample(.N, floor(release.rate * .N)), released := TRUE]
    return(dt)
}

  dt[, obs_risk := true_risk + treated * noise * tau + (1 - treated) * noise]
  dt[, released := ecdf(obs_risk)(obs_risk) <= release_rate, by="treated,tau"]

  return(dt)
}
