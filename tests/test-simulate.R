library(data.table)
library(readxl)
library(devtools)
library(pipeR)
load_all("~/hls/research/ajl")

fi.val <- "~/hls/research/ajl/inst/extdata/Simulation_Values_to_estimate.xls"
dt.values <- read_excel(fi.val) %>>%
  data.table


system.time(dt.out <- dt.values[1, {
  # set.seed(seed)
  AJL::simulate(tau = c(tau1, tau2, tau3), release.rate = release_rate
  , iter = 1, n = num_arrestees, a = alpha, b = beta
  , mu = gamma, sigma = sigma) %>>%
  (.[, list(
    n = .N,
    released = sum(released),
    failure = sum(released * misbehave),
    failure_rate = sum(released * misbehave) / .N,
    # days_incarc = sum(days_incarc[!released]),
    days_incarc_mean = mean(days_incarc[!released]),
    days_incarc_var = sd(days_incarc[!released])^2
    ), keyby="treated,tau"])
  }, keyby="run,iteration"])
# write.csv(dt.out, paste0(c("~/Downloads/run1iter49seed", seed, ".csv"), collapse=""))
dt.out
