rm(list = ls())
sim <- function (alpha = .05, Learn = 1000, Quota1, Quota2, Quota3, Quota4, Hit.Prob, 
                 EOC.Strategy.Prob = .5, 
                 LOE.Strategy.Prob = .5,
                 contrast = c(-1.5, -0.5, 0.5, 1)) {

  #creating objects for information storing####
  hit = vector()
  hitcheck = vector()
  draw = vector()
  temp = vector()
  strategy.choice = vector()
  outcome1 = matrix(, nrow = 0, ncol = 7)
  outcome2 = matrix(, nrow = 0, ncol = 16)

  Neu.Strategy.Prob = 1 - EOC.Strategy.Prob  - LOE.Strategy.Prob
  
  
  #EOC.Strategy.Function Function####
  EOC.Strategy.Function <<- function (Quota,
                                      Hit.Prob,
                                      alpha)
    
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
  
  
  
  #LOE.Strategy.Function Function####
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
  
  
  #Neu.Strategy.Function####
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
  
  #Simulating the choosing and decision making####
  for (ii in 1:Learn) {
    EOC.Strategy.ProbRange.Min = 0
    EOC.Strategy.ProbRange.Max = EOC.Strategy.Prob
    LOE.Strategy.ProbRange.Min = EOC.Strategy.ProbRange.Max
    LOE.Strategy.ProbRange.Max = EOC.Strategy.ProbRange.Max + LOE.Strategy.Prob
    Neu.Strategy.ProbRange.Min = LOE.Strategy.ProbRange.Max
    Neu.Strategy.ProbRange.Max = LOE.Strategy.ProbRange.Max + Neu.Strategy.Prob
    if (ii < (Learn/4)) {
      Quota = Quota1
    }
    if (ii > (Learn/4) & ii < (Learn/2)) {
      Quota = Quota2
    }
    if (ii > (Learn/2) & ii < (Learn*3/4)) {
      Quota = Quota3
    }
    if (ii > (Learn*3/4)) {
      Quota = Quota4
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
  
  #producing the outcomes####
  outcome1 = cbind(outcome1, strategy.choice)
  outcome0250 = c(outcome1[Learn/4, 5], outcome1[Learn/4, 6], outcome1[Learn/4, 7])
  outcome0500 = c(outcome1[Learn/2, 5], outcome1[Learn/2, 6], outcome1[Learn/4, 7])
  outcome0750 = c(outcome1[Learn*.75, 5], outcome1[Learn*.75, 6], outcome1[Learn*.75, 7])
  outcome1000 = c(outcome1[Learn, 5], outcome1[Learn, 6], outcome1[Learn, 7])
  x1 = c(outcome1[Learn/4, 5],
         outcome1[Learn/2, 5],
         outcome1[Learn*.75, 5],
         outcome1[Learn, 5])
  dv.r = cor(as.numeric(x1), contrast)
  
  outcometemp = cbind(t(outcome0250),
                      t(outcome0500),
                      t(outcome0750),
                      t(outcome1000),
                      alpha,
                      Hit.Prob,
                      Learn, dv.r)
  outcome2 = rbind(outcome2, outcometemp)
  return(outcome2)

}



