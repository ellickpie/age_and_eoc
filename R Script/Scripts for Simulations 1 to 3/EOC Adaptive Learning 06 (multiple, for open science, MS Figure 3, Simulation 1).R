rm(list = ls())
outcome3 = matrix(, nrow = 0, ncol = 8)
for (trial in 1:3) {
  sim <- function () {
    rm(list = ls())
    EOC.Strategy.Prob = vector()
    EOC.Strategy.ProbRange.Min = vector()
    EOC.Strategy.ProbRange.Max = vector()
    LOE.Strategy.Prob = vector()
    LOE.Strategy.ProbRange.Min = vector()
    LOE.Strategy.ProbRange.Max = vector()
    Neu.Strategy.Prob = vector()
    Neu.Strategy.ProbRange.Min = vector()
    Neu.Strategy.ProbRange.Max = vector()
    hit = vector()
    hitcheck = vector()
    draw = vector()
    temp = vector()
    strategy.choice = vector()
    outcome1 = matrix(, nrow = 0, ncol = 7)
    outcome2 = matrix(, nrow = 0, ncol = 12)
    #----- defining the range of probability
    #EOC.Strategy.ProbRange.Min [1] = 0
    #EOC.Strategy.ProbRange.Max [1] = EOC.Strategy.Prob [1]
    #LOE.Strategy.ProbRange.Min [1] = EOC.Strategy.ProbRange.Max [1]
    #LOE.Strategy.ProbRange.Max [1] = EOC.Strategy.ProbRange.Max [1] + LOE.Strategy.ProbRange.Min [1]
    #Neu.Strategy.ProbRange.Min [1] = LOE.Strategy.ProbRange.Max [1]
    #Neu.Strategy.ProbRange.Max [1] = LOE.Strategy.ProbRange.Max [1] + Neu.Strategy.Prob [1]
    
    
    #----- values of all parameters
    EOC.Strategy.Prob = .5
    LOE.Strategy.Prob = .5
    Neu.Strategy.Prob = 1 - EOC.Strategy.Prob  - LOE.Strategy.Prob
    Quota = 10
    if (trial == 1) {
      Hit.Prob = .2
    }
    if (trial == 2) {
      Hit.Prob = .5
    }
    if (trial == 3) {
      Hit.Prob = .8
    }
    alpha = .05
    Learn = 1000
    
    #EOC.Strategy.Function Function
    EOC.Strategy.Function <<- function (Quota,
                                        Hit.Prob,
                                        alpha)
      #,
      #EOC.Strategy.Prob,
      #LOE.Strategy.Prob,
      #Neu.Strategy.Prob)
    {
      for (i in 1:Quota) {
        #STep 1, Do the draw
        hit [i] = runif(1, 0, 1)
        hitcheck [i] = 0
        if (hit [i] < Hit.Prob) {
          hitcheck [i] = 1
          break
        }
      }
      
      #Step 2, change the Prob
      if (sum(hitcheck) > 0) {
        adjustment = alpha * (1 / i)
        EOC.Strategy.Prob = (1 - alpha) * EOC.Strategy.Prob + adjustment
      } else
      {
        adjustment = alpha
        EOC.Strategy.Prob = (1 - alpha) * EOC.Strategy.Prob
      }
      
      LOE.Strategy.Prob  = (1 - EOC.Strategy.Prob) * (LOE.Strategy.Prob / (LOE.Strategy.Prob + Neu.Strategy.Prob))
      Neu.Strategy.Prob  = 1 - EOC.Strategy.Prob - LOE.Strategy.Prob
      if (Neu.Strategy.Prob < .000000001) {
        Neu.Strategy.Prob = 0
      }
      
      newlist = list(
        "hitcheck" = hitcheck,
        "hit" = hit,
        "adjustment" = adjustment,
        "alpha" = alpha,
        "EOC.Strategy.Prob" = EOC.Strategy.Prob,
        "LOE.Strategy.Prob" = LOE.Strategy.Prob,
        "Neu.Strategy.Prob" = Neu.Strategy.Prob
      )
      return(newlist)
    }
    
    
    
    #LOE.Strategy.Function Function
    LOE.Strategy.Function <<- function (Quota,
                                        Hit.Prob,
                                        alpha) {
      for (i in 1:Quota) {
        #STep 1, Do the draw
        hit [i] = runif(1, 0, 1)
        hitcheck [i] = 1
        if (hit [i] > Hit.Prob) {
          hitcheck [i] = 0
          break
        }
      }
      
      
      #Step 2, change the Prob
      if (sum(hitcheck) > 0) {
        adjustment = alpha * ((sum(hitcheck)) / length(hitcheck))
        LOE.Strategy.Prob = (1 - alpha) * LOE.Strategy.Prob + adjustment
      } else
      {
        adjustment = alpha
        LOE.Strategy.Prob = (1 - alpha) * LOE.Strategy.Prob
      }
      EOC.Strategy.Prob  = (1 - LOE.Strategy.Prob) * (EOC.Strategy.Prob / (EOC.Strategy.Prob + Neu.Strategy.Prob))
      Neu.Strategy.Prob  = 1 - EOC.Strategy.Prob - LOE.Strategy.Prob
      if (Neu.Strategy.Prob < .000000001) {
        Neu.Strategy.Prob = 0
      }
      
      newlist = list(
        "hitcheck" = hitcheck,
        "hit" = hit,
        "adjustment" = adjustment,
        "alpha" = alpha,
        "EOC.Strategy.Prob" = EOC.Strategy.Prob,
        "LOE.Strategy.Prob" = LOE.Strategy.Prob,
        "Neu.Strategy.Prob" = Neu.Strategy.Prob
      )
      return(newlist)
      
    }
    
    
    #Neu.Strategy.Function
    Neu.Strategy.Function <<- function (Quota,
                                        Hit.Prob,
                                        alpha) {
      for (i in 1:Quota) {
        #STep 1, Do the draw
        hit [i] = runif(1, 0, 1)
        hitcheck [i] = 1
        if (hit [i] > Hit.Prob) {
          hitcheck [i] = 0
        }
      }
      
      
      #Step 2, change the Prob
      if (sum(hitcheck) > 0) {
        adjustment = alpha * (sum(hitcheck) / length(hitcheck))
        Neu.Strategy.Prob = (1 - alpha) * Neu.Strategy.Prob + adjustment
      } else
      {
        adjustment = alpha
        Neu.Strategy.Prob = (1 - alpha) * Neu.Strategy.Prob
      }
      EOC.Strategy.Prob  = (1 - Neu.Strategy.Prob) * (EOC.Strategy.Prob / (EOC.Strategy.Prob + LOE.Strategy.Prob))
      LOE.Strategy.Prob  = 1 - EOC.Strategy.Prob - Neu.Strategy.Prob
      newlist = list(
        "hitcheck" = hitcheck,
        "hit" = hit,
        "adjustment" = adjustment,
        "alpha" = alpha,
        "EOC.Strategy.Prob" = EOC.Strategy.Prob,
        "LOE.Strategy.Prob" = LOE.Strategy.Prob,
        "Neu.Strategy.Prob" = Neu.Strategy.Prob
      )
      return(newlist)
      
    }
    
    
    for (ii in 1:Learn) {
      EOC.Strategy.ProbRange.Min = 0
      EOC.Strategy.ProbRange.Max = EOC.Strategy.Prob
      LOE.Strategy.ProbRange.Min = EOC.Strategy.ProbRange.Max
      LOE.Strategy.ProbRange.Max = EOC.Strategy.ProbRange.Max + LOE.Strategy.Prob
      Neu.Strategy.ProbRange.Min = LOE.Strategy.ProbRange.Max
      Neu.Strategy.ProbRange.Max = LOE.Strategy.ProbRange.Max + Neu.Strategy.Prob
      if (ii < (Learn/4)) {
        Quota = 10
      }
      if (ii > (Learn/4) & ii < (Learn/2)) {
        Quota = 7
      }
      if (ii > (Learn/2) & ii < (Learn*3/4)) {
        Quota = 4
      }
      if (ii > (Learn*3/4)) {
        Quota = 2
      }
      
      draw = runif(1, 0, 1)
      if (draw < EOC.Strategy.ProbRange.Max) {
        strategy.choice [ii] = 1
      }
      if (LOE.Strategy.ProbRange.Max  > draw &
          draw > LOE.Strategy.ProbRange.Min) {
        strategy.choice [ii] = 2
      }
      #else
      if (draw > LOE.Strategy.ProbRange.Max) {
        strategy.choice [ii] = 3
      }
      if (strategy.choice[ii] == 1) {
        outcome = EOC.Strategy.Function(Quota,
                                        Hit.Prob,
                                        alpha)
      }
      if (strategy.choice[ii] == 2)
      {
        outcome = LOE.Strategy.Function(Quota,
                                        Hit.Prob,
                                        alpha)
      }
      if (strategy.choice[ii] == 3)
      {
        outcome = Neu.Strategy.Function(Quota,
                                        Hit.Prob,
                                        alpha)
      }
      
      EOC.Strategy.Prob = outcome$EOC.Strategy.Prob
      LOE.Strategy.Prob = outcome$LOE.Strategy.Prob
      Neu.Strategy.Prob = outcome$Neu.Strategy.Prob
      outcome1 = rbind(outcome1, t(outcome))
    }
    outcome1 = cbind(outcome1, strategy.choice)
    outcome0250 = c(outcome1[250, 5], outcome1[250, 6], outcome1[250, 7])
    outcome0500 = c(outcome1[500, 5], outcome1[500, 6], outcome1[500, 7])
    outcome0750 = c(outcome1[750, 5], outcome1[750, 6], outcome1[750, 7])
    outcome1000 = c(outcome1[1000, 5], outcome1[1000, 6], outcome1[1000, 7])
    outcometemp = cbind(t(outcome0250),
                        t(outcome0500),
                        t(outcome0750),
                        t(outcome1000))
    outcome2 = rbind(outcome2, outcometemp)
    return(outcome2)
    #return(Hit.Prob)
  }
  
  alloutcome = replicate (5000, sim())
  boutcome = t(alloutcome [1, ,])
  coutcome.2 = c(
    mean(unlist(boutcome[, 1])),
    mean(unlist(boutcome[, 2])),
    mean(unlist(boutcome[, 4])),
    mean(unlist(boutcome[, 5])),
    mean(unlist(boutcome[, 7])),
    mean(unlist(boutcome[, 8])),
    mean(unlist(boutcome[, 10])),
    mean(unlist(boutcome[, 11]))
  )
  outcome3 = rbind(outcome3, coutcome.2)
}
outcome3
rownames(outcome3) = c("0.2", "0.5", "0.8")
colnames(outcome3 )= c("Escalation 250", "Law-of-Effect 250", 
                       "Escalation 500", "Law-of-Effect 500",
                       "Escalation 750", "Law-of-Effect 750",
                       "Escalation 1000", "Law-of-Effect 1000")
output = as.data.frame(outcome3)


##############Plot the Graph for Figure 3
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

