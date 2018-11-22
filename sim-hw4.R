# Needed Libraries for Analysis #
library(graphics)
library(ks)
library(lubridate)
library(tidyverse)
library(triangle)
library(MASS)
library(rgl)
library(readxl)
# Use for truncated normal
#install.packages('truncnorm')
library(truncnorm)
library(ggplot2)
library(scales)
library(Hmisc)

set.seet(69)
simulation.size = 10000

Drilling <- read_excel('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\SimulationRisk\\Drillingcosts.xlsx')

Drilling$Year <- year(Drilling$Date)
Drilling_new <- subset(Drilling, Drilling$Year > 1990 & Drilling$Year <2007)
Drilling_new$avg <- rowMeans(subset(Drilling_new,
                                    select = c(Cost_CrudeOil,Cost_NaturalGas,Cost_DryWell)), na.rm = TRUE)

#Concatenating the 48 obs for change in costs into one vector
Change <- c(t(Drilling_new[5:7]))
Change <- as.numeric(Change)
m <- mean(Change)
sd <- sd(Change)


#Vectors to store the simulated costs for 2019
Cost2019 <- rep(0,simulation.size)

#Initial value of Drilling Costs
C2006 <- 2279.8

for(i in 1:simulation.size){
  x <- C2006 #initial cost 
  
  #Loop calculates the cost from 2007 to 2012
  for(j in 1:6){
    r <- rnorm(n=1, mean=m, sd=sd) #normal distribution b/w 2006-2012
    x <- x*(1+r)
  }
  y <- x
  
  #Loop calculates the cost from 2013 to 2015
  for(j in 1:3){
    r <- rtriangle(1, -0.22, -0.07, -0.0917)  #triangle distribution b/w 2012-2015
    y <- y*(1+r)
  }
  z <- y
  
  #Loop calculates the cost from 2016 to 2019
  for(j in 1:4){
    r <- rtriangle(1, 0.02, 0.06, 0.05) #triangle distribution b/w 2015-2019
    z <- z*(1+r)
  }
  
  #Stores the simulated cost for 2019 in the vector
  Cost2019[i] <- z
}