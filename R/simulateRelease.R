#' @title Simulate
#'
#' @importFrom assertthat assert_that
simulateRelease <- function(tau = 1, release.rate, n, num.obs.nonwhite, a, b, mu, sigma
    , k = .32, theta = 77, haphazard = FALSE, plot = TRUE) {
  arrestees <- getArr(n, num.obs.nonwhite, a, b, mu, sigma, k = k, theta = theta)

  # randomly assign half the arrestees to the control and the other half to the treatment group
  arrestees[, treated := FALSE]
  arrestees[sample(.N, floor(.N/2)), treated := TRUE]
  
  # add tau value to the treated arrestees only and merge treated arrestees with the non-treated arrestees
  arr_id_treated <- arrestees[treated == 1, arr_id]
  dt <- rbind(CJ(arr_id = arr_id_treated, tau = tau), arrestees[treated == 0, list(arr_id, tau = -1)], use.names = TRUE)
  dt <- data.table:::merge.data.table(dt, arrestees, by="arr_id")
    
  # make sure we have the right number of rows
  assert_that(nrow(dt) == length(unique(arrestees$arr_id)) / 2 * (length(tau) + 1)

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
