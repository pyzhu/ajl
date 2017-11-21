#' @title Simulate
#'
#' @importFrom assertthat assert_that
simulateRelease <- function(tau = 1, release.rate, n, num.obs.nonwhite, a, b, mu, sigma
    , k = .32, theta = 77, haphazard = FALSE, plot = TRUE) {
  arrestees <- getArr(n, num.obs.nonwhite, a, b, mu, sigma, k = k, theta = theta)

  # for each arrestee, there should be a number of tau values to assess
  # where tau = 0, set treatment = 0 (definition of treatment)
  # arr_id_treated <- arrestees[treated == 1, arr_id]
  dt <- arrestees[CJ(arr_id, tau=tau)] # add the tau value and treated
  dt[, treated := (tau > 0)]

  # make sure we have the right number of rows
  assert_that(nrow(dt) == length(unique(arrestees$arr_id)) * length(tau))

  # add in the scalars
  dt[, release_rate := release.rate]
  
  # for the case where judges release completely randomly without looking at
  # the psa score.
  if (haphazard) {
    dt[, released := FALSE]
    dt[sample(.N, floor(release.rate * .N)), released := TRUE]
    return(dt)
  }

  dt[, obs_risk := true_risk + treated * noise * tau + (1 - treated) * noise]
  dt[, released := ecdf(obs_risk)(obs_risk) <= release_rate, by="treated,tau"]

  return(dt)
}
