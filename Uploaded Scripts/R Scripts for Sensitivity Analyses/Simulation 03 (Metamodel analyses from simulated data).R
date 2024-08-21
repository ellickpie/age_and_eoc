rm(list = ls())
library("sensobol")
library(rvest)
library(tidyr)
library(sjPlot)
library(sjmisc)
library(ggplot2)

load("Simulated Data for Sensitivity Analyses/Simulation 3 metamodel.RData")
data = as.data.frame(boutcome)
data = as.data.frame(mapply(unlist, data))
mean(data$dv.r)
sd(data$dv.r)
y = scale(data$dv.r)
Alpha = c(scale(data$alpha))
x.Hit.Prob = c(scale(data$Hit.Prob))
No_of_Learning = c(scale(data$Learn))

fit = lm(data$dv.r ~  Alpha * x.Hit.Prob * No_of_Learning)
summary(fit)

plot_model(fit, type = "pred", 
           terms = c("No_of_Learning [-2, 0, 2]", "x.Hit.Prob[-2,0,2]", "Alpha [-2, 0, 2]"),
           axis.title = c("No. of Learning Trials","Correlation"),
           axis.labels = "XXXXXXX",
           legend.title = "Hit Probability",
           title = c("Simulation 3", "ddd"))

