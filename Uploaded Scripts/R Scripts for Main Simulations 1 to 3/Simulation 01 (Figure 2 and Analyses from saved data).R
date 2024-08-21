rm(list = ls())
##############Plot the Graph for Figure 2
load("Simulated Data for Main Analyses/Simulatin_01_main_output.RData")
temp = rbind(ES2, ES5, ES8, LOE2, LOE5, LOE8)
write.csv(temp, "simulation 1 table.csv")
ES2 = c("", outcome3[1,1], outcome3[1,3], outcome3[1,5],outcome3[1,7])
LOE2 = c("", outcome3[1,2], outcome3[1,4], outcome3[1,6],outcome3[1,8])

ES5 = c("", outcome3[2,1], outcome3[2,3], outcome3[2,5],outcome3[2,7])
LOE5 = c("", outcome3[2,2], outcome3[2,4], outcome3[2,6],outcome3[2,8])

ES8 = c("", outcome3[3,1], outcome3[3,3], outcome3[3,5],outcome3[3,7])
LOE8 = c("", outcome3[3,2], outcome3[3,4], outcome3[3,6],outcome3[3,8])

#plot(kk, type="l", col="blue", ylim=c(0,1), axes = FALSE)
plot(ES2, pch = 15, type = "b", col="black", 
     ylim=c(0,1), axes = FALSE,
     xlab = "Development Phases", ylab = "Probability of Choosing")
lines(ES5, pch = 19, type="b", col="black")
lines(ES8, pch = 17, type="b", col="black")
lines(LOE2, pch = 22, type="b", lty=2, col="black")
lines(LOE5, pch = 21, type="b", lty=2, col="black")
lines(LOE8, pch = 24, type="b", lty=2, col="black")

axis(1, 1:5, c("", "250 (Quota = 10)", "500 (Quota = 7)", 
               "750 (Quota = 4)", "1000 (Quota = 2)"), col.axis = "blue")
axis(2, col.axis = "blue")
legend(x = "bottomleft",          # Position
       legend = c("Escalation (0.2)", "Law-of-Effect (0.2)",
                  "Escalation (0.5)", "Law-of-Effect (0.5)",
                  "Escalation (0.8)", "Law-of-Effect (0.8)"),  # Legend texts
       lty = c(1, 1, 1, 2, 2, 2),      # Line types
       pch = c(15, 22, 19, 21, 17, 24),
       col = c(1, 1, 1,1,1,1),           # Line colors
       cex = 0.8,
       lwd = 2)  

##############Statistics for Simulation 1
x = c (-1.5, -0.5, 0.5, 1.5)
y1 = c(outcome3[1,1], outcome3[1,3], outcome3[1,5], outcome3[1,7])
y2 = c(outcome3[2,1], outcome3[2,3], outcome3[2,5], outcome3[2,7])
y3 = c(outcome3[3,1], outcome3[3,3], outcome3[3,5], outcome3[3,7])

cor(x, y1)
cor(x, y2)
cor(x, y3)


