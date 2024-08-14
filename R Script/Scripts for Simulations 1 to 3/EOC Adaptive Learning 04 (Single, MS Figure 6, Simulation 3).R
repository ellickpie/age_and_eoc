

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
#Quota = 10
Hit.Prob = 0.2
alpha = .05

#EOC.Strategy.Function Function
EOC.Strategy.Function = function (Quota,
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
LOE.Strategy.Function = function (Quota,
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
Neu.Strategy.Function = function (Quota,
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

for (ii in 1:1000) {
  EOC.Strategy.ProbRange.Min = 0
  EOC.Strategy.ProbRange.Max = EOC.Strategy.Prob 
  LOE.Strategy.ProbRange.Min = EOC.Strategy.ProbRange.Max 
  LOE.Strategy.ProbRange.Max = EOC.Strategy.ProbRange.Max + LOE.Strategy.ProbRange.Min
  Neu.Strategy.ProbRange.Min = LOE.Strategy.ProbRange.Max 
  Neu.Strategy.ProbRange.Max = LOE.Strategy.ProbRange.Max + Neu.Strategy.Prob
  if(ii < 250) {Quota = 2}
  if(ii > 250 & ii < 500 ) {Quota = 10}
  if(ii > 500 & ii < 750 ) {Quota = 7}
  if(ii > 750) {Quota = 10}
  
  draw = runif(1, 0, 1)
  if (draw < EOC.Strategy.ProbRange.Max ) {
    strategy.choice [ii] = 1}
  #if (LOE.Strategy.ProbRange.Max  > draw & draw > LOE.Strategy.ProbRange.Min ) 
  else {strategy.choice [ii] = 2}
  #if (draw > LOE.Strategy.ProbRange.Max) {
   # strategy.choice [ii] = 3}
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

kk = outcome1[,5]
yy = outcome1[,6]
kk = unlist(kk)
yy = unlist(yy)

plot(yy, type="l", col="red", ylim=c(0,1), xlab = "Development Phases", ylab = "Probability of Choosing")
lines(kk, type="l", col="blue")
legend(x = "bottomleft",          # Position
      legend = c("Escalation Strategy", "Law-of-Effect Strategy"),  # Legend texts
      lty = c(1, 1),      # Line types
      col = c("blue", "red"),           # Line colors
      cex = 0.8,
      lwd = 2)  


