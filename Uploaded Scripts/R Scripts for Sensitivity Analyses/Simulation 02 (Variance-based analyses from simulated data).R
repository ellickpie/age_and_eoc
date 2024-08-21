rm(list = ls())
library(rvest)
library(tidyr)
library("sensobol")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")

load("Simulated Data for Sensitivity Analyses/Simulation 2 sensobol_y.RData")
load("Simulated Data for Sensitivity Analyses/Simulation 2 sensobol_out.RData")
N <- 5000
params <- c("alpha", "Hit.Prob", "Learn")
matrices <- c("A", "B", "AB", "BA")
first <- total <- "azzini"
order <- "second"
R <- 10 ^ 3
type <- "percent"
conf <- 0.95


out = as.data.frame(out)

plot_uncertainty(Y = y, N = N)
ind <- sobol_indices(matrices = matrices, Y = y, N = N, params = params,
                     first = first, total = total, order = order, boot = TRUE, R = R,
                     parallel = "no", type = type, conf = conf)


ind.dummy <- sobol_dummy(Y = y, N = N, params = params, boot = TRUE, R = R)


plot(ind, dummy = ind.dummy)
plot(ind, order = "second")

plot_multiscatter(data = out, N = N, Y = y, params = params)

t.test(y, mu = 0)  ## the average r and its 95% interval

sum(ind$results$original[1],
    ind$results$original[2], 
    ind$results$original[3],
    ind$results$original[7],
    ind$results$original[8],
    ind$results$original[9])  ## the total explained variance by the three parameters

