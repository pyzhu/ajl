library(data.table)
library(readxl)
library(devtools)
library(pipeR)
library(testthat)
library(assertthat)

# change this to where the file is located
load_all("~/hls/research/ajl")
fi.val <- "/Users/pyz/Dropbox/PRAI Simulations (1)/Peter/Dataset_for_Simulation_20171108.xls"

dt.values <- read_excel(fi.val) %>>%
  data.table

# run a few tests to make sure the algo is working the way we want it to
test_that("setting the right number of whites and nonwhites", {
  getArr(n=1000, num.obs.nonwhite=700, a=.5, b=.45, mu=0, sigma=.05, .44, .53) %>>%
    with(expect_equal(sum(nonwhite), 700))
  
  expect_that(getArr(n=1000, num.obs.nonwhite=7000, a=.5, b=.45, mu=0, sigma=.05, .44, .53)
              , throws_error())
})



# create iterations and aggregate into summary data
system.time(
dt.sim <- dt.values[run == 99, {
  # set.seed(seed)
  simulateRelease(
    tau = c(tau1, tau2, tau3, tau4), release.rate = release_rate
  , n = num_obs, num.obs.nonwhite = num_obs_nonwhite
  , a = alpha, b = beta
  , mu = gamma, sigma = sigma
  , haphazard = TRUE) %>>%
    (.[, list(
      num_obs_group = .N,
      num_obs,
      num_obs_white,
      num_obs_non_white,
      nonwhite,
      true_risk,
      alpha = alpha[1],
      beta = beta[1],
      gamma = gamma[1],
      sigma = sigma[1],
      release_rate = release_rate[1],
      released,
      misbehave,
      # failure = sum(released * misbehave),
      failure_rate = sum(released * misbehave) / .N
      # days_incarc = sum(days_incarc[!released]),
      # days_incarc_mean = mean(days_incarc[!released]),
      # days_incarc_var = sd(days_incarc[!released])^2
      ), keyby="treated,tau"])
  }, keyby="run,iteration"])

# Test how true risk is distributed across white and nonwhite
lattice::densityplot(~true_risk, dt.sim[sample(.N, 10000)], group=nonwhite)
dt.sim[, list(sum(released) / .N), by=nonwhite]


# # format data to be written to csv
# # TODO: deprecated, but should be the same idea.
dt.sim[, list(
  control_failure1 = mean(failure_rate[which(treated == 0)]),
#   control_failure2 = failure_rate[which(treated == 0)],
#   control_failure3 = failure_rate[which(treated == 0)],
  treatment_failure1 = mean(failure_rate[which(frank(tau, ties.method = "dense") == 3 & treated > 0)]),
  treatment_failure2 = mean(failure_rate[which(frank(tau, ties.method = "dense") == 4 & treated > 0)]),
  treatment_failure3 = mean(failure_rate[which(frank(tau, ties.method = "dense") == 5 & treated > 0)]),
  treatment_failure4 = mean(failure_rate[which(frank(tau, ties.method = "dense") == 2 & treated > 0)])
#   mean_days_control1 = days_incarc_mean[which(treated == 0)],
#   mean_days_control2 = days_incarc_mean[which(treated == 0)],
#   mean_days_control3 = days_incarc_mean[which(treated == 0)],
#   variance_days_control1 = days_incarc_var[which(treated == 0)],
#   variance_days_control2 = days_incarc_var[which(treated == 0)],
#   variance_days_control3 = days_incarc_var[which(treated == 0)],
#   mean_days_treatment1 = days_incarc_mean[which(frank(tau, ties.method = "first") == 2 & treated > 0)],
#   mean_days_treatment2 = days_incarc_mean[which(frank(tau, ties.method = "first") == 3 & treated > 0)],
#   mean_days_treatment3 = days_incarc_mean[which(frank(tau, ties.method = "first") == 4 & treated > 0)],
#   variance_days_treatment1 = days_incarc_var[which(frank(tau, ties.method = "first") == 2 & treated > 0)],
#   variance_days_treatment2 = days_incarc_var[which(frank(tau, ties.method = "first") == 3 & treated > 0)],
#   variance_days_treatment3 = days_incarc_var[which(frank(tau, ties.method = "first") == 4 & treated > 0)]
), by="run,iteration,num_obs,num_obs_white,num_obs_nonwhite,alpha,beta,gamma,sigma,release_rate"]
