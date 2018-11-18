library(graphics)
library(ks)
library(lubridate)
library(tidyverse)
library(graphics)
library(triangle)
library(MASS)
library(rgl)

# Needed Libraries for Analysis #
library(readxl)
# Use for truncated normal
#install.packages('truncnorm')
library(truncnorm)

set.seed(69)
simulation.size <- 10000
hydro.dist <- rtruncnorm(simulation.size,a=0,b=1,mean=0.99,sd=0.05)
reser.dist <- rtruncnorm(simulation.size,a=0,b=1,mean=0.8,sd=0.1)
prop.dist <- rep(0,simulation.size)

for(j in seq(simulation.size)){
  num.wells <- sample(c(10:30),1,replace=T)
  produce <- rep(0, num.wells)
  
  hydro.risk <- rtruncnorm(num.wells, a=0, b=1, mean=0.99, sd=0.05)
  reservoir.risk <- rtruncnorm(num.wells, a=0, b=1, mean=0.8, sd=0.1)

  p.produce <- hydro.risk * reservoir.risk
  v.produce <- rbernoulli(num.wells,p.produce)
  produce <- ifelse(v.produce,1,0)
  
  prop.dist[j] <- mean(produce)
}

hist(prop.dist)  
hist(hydro.dist)
hist(reser.dist)

library(Hmisc)
describe(prop.dist)

VaR <- quantile(prop.dist, probs=0.05)  
CVaR <- mean(prop.dist[prop.dist <= VaR])
