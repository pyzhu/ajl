library(data.table)
library(readxl)
library(devtools)
library(pipeR)
load_all("~/Documents/hls/research/ajl")

fi.val <- "~/Documents/hls/research/Simulation_Values_to_estimate.xls"
dt.values <- read_excel(fi.val) %>>%
  data.table

dt.values[1, {
  set.seed(1)
  AJL::simulate(tau = c(tau1, tau2, tau3), release.rate = release_rate
  , iter = 1, n = num_arrestees, a = alpha, b = beta
  , mu = gamma, sigma = sigma) %>>%
  (.[, list(
    failure = sum(released * misbehave),
    failure_rate = sum(released * misbehave) / .N,
    days_incarc = sum(days_incarc[!released]),
    days_incarc_mean = mean(days_incarc[!released]),
    days_incarc_var = sd(days_incarc[!released])^2
    ), by="treated,tau"])
  }]
