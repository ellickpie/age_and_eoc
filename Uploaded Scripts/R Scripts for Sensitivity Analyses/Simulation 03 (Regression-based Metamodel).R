rm(list = ls())
library("sensobol")
library(rvest)
library(tidyr)
library(sjPlot)
library(sjmisc)
library(ggplot2)

outcome3 = matrix(, nrow = 0, ncol = 11)

source("sim.R")
alpha.norm = function ()
{
  a = rnorm(1, .05, .015)
  if (a < .002) {a = .002}
  if (a > .098) {a = .098}
  return(a)
}

Learn.norm = function()
{
  a = rnorm(1, 250, 50)
  if (a < 50) {a = 50}
  if (a > 450) {a = 450}
  a = 4* round(a, 0)
  return(a)
}

Hit.Prob.norm = function()
{
  a = rnorm(1, .5, .15)
  if (a < .02) {a = .02}
  if (a > .98) {a = .98}
  return(a)
}


Quota1 = 2; Quota2 = 10; Quota3 = 7; Quota4 = 2
EOC.Strategy.Prob = .6 
LOE.Strategy.Prob = .4
contrast = c(-1, 1, 1, -1)

alloutcome = replicate (5000, sim.corr(alpha.norm(), Learn.norm(), Quota1, Quota2, Quota3, Quota4,
                                       Hit.Prob.norm(),
                                       EOC.Strategy.Prob, LOE.Strategy.Prob,
                                       contrast))

boutcome = t(alloutcome [1, ,]) %>% as.data.frame() 
save(boutcome, file = "Simulated Data for Sensitivity Analyses/Simulation 3 metamodel.RData")

