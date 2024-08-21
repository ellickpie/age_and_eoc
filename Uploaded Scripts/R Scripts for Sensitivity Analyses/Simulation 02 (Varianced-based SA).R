rm(list = ls())
library(rvest)
library(tidyr)
library("sensobol")
if(!require(devtools)) install.packages("devtools")
devtools::install_github("kassambara/ggpubr")
source("sim.corr.R")

N <- 5000
params <- c("alpha", "Hit.Prob", "Learn")
matrices <- c("A", "B", "AB", "BA")
first <- total <- "azzini"
order <- "second"
R <- 10 ^ 3
type <- "percent"
conf <- 0.95

out = sobol_matrices(matrices = matrices, N = N, params = params,
                      order = order, type = "LHS")


out[, "alpha"] <- qnorm(out[, "alpha"], .05, 0.015)%>% 
  ifelse(. > .098, .098, .) %>% ifelse(. < .002, .002, .)
  
out[, "Hit.Prob"] <- qnorm(out[, "Hit.Prob"], .5, .15) %>% 
ifelse(. > .98, .98, .) %>% ifelse(. < .02, .02, .)

out[, "Learn"] <- qnorm(out[, "Learn"], 250, 50) %>% 
ifelse(. > 450, 450, .) %>% ifelse(. < 50, 50, .) %>% round(., 0)

out[, "Learn"] = out[, "Learn"] * 4

Quota1 = 2; Quota2 = 4; Quota3 = 7; Quota4 = 10 
EOC.Strategy.Prob = .5
LOE.Strategy.Prob = .5
contrast = c(-1.5, -0.5, 0.5, 1.5)

y <- nrow(out)
for (i in 1:nrow(out))
{
  y[i] = sim.corr(out[i,1], out[i,3], Quota1, Quota2, Quota3, Quota4,
             out[i, 2],
             EOC.Strategy.Prob, LOE.Strategy.Prob,
             contrast)
  print(i)
  
}
  
save(y, file = "Simulated Data for Sensitivity Analyses/study 2 sensobol_y.RData")
save(out, file = "Simulated Data for Sensitivity Analyses/study 2 sensobol_out.RData")

