# Simulating Age Differences in Escalation Bias via Multi-level Adaptive Learning

This repository provides the code for our open-source of simulating escalation bias under multi-level adaptive learning. It is an R function with the following parameter inputs:

## Usage
sim (alpha = .05, Learn = 1000, Quota1, Quota2, Quota3, Quota4, Hit.Prob, 
                 EOC.Strategy.Prob = .5, 
                 LOE.Strategy.Prob = .5,
                 contrast = c(-1.5, -0.5, 0.5, 1))

## Argument
alpha &nbsp;&nbsp;&nbsp;&nbsp;    The learning parameter. The default is .05. \
Learn  &nbsp;&nbsp;&nbsp;&nbsp;   Total number of learning trials.\
Quota1   &nbsp;&nbsp;&nbsp;&nbsp; The quota in Phase 1.\
Quota2   &nbsp;&nbsp;&nbsp;&nbsp; The quota in Phase 2.\
Quota3   &nbsp;&nbsp;&nbsp;&nbsp; The quota in Phase 3.\
Quota4   &nbsp;&nbsp;&nbsp;&nbsp; The quota in Phase 4.\
Hit.Prob &nbsp;&nbsp;&nbsp;&nbsp; The Probability of getting positive feedback.\
EOC.Strategy.Prob &nbsp;&nbsp;&nbsp;&nbsp; The Probability of choosing escalation strategy. The default is .5.\
LOE.Strategy.Prob &nbsp;&nbsp;&nbsp;&nbsp; The Probability of choosing law-of-effect strategy. The default is .5.\
contrast &nbsp;&nbsp;&nbsp;&nbsp; contrast for testing the linearity of the change in escalation. The default is c(-1.5, -0.5, 0.5, 1.5).

## Examples
### Simulation 1 of the paper
sim(
  alpha = .05, Learn = 1000, Quota1 = 10, Quota2 = 7, Quota3 = 4, Quota4 = 2, 
     Hit.Prob = .2, 
     EOC.Strategy.Prob = .6, 
     LOE.Strategy.Prob = .4,
  contrast = c(1.5, 0.5, -0.5, -1.5)
  )



