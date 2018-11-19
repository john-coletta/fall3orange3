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

# Set the seed and simulation size
set.seed(69)
simulation.size <- 100000

ptm <- proc.time()
# Initialize the empty vectors to hold simulated values
hydro.dist <- numeric()
reser.dist <- numeric()
prop.dist <- rep(0,simulation.size)

for(j in 1:simulation.size){
  
  if(j %% 10000 == 0){
    print(j)
    print(proc.time() - ptm)
  }
  
  # Pull the number of wells
  num.wells <- sample(c(10:30),1,replace=T)
  produce <- rep(0, num.wells)
  # Calculate the risk for each well
  hydro.risk <- rtruncnorm(num.wells, a=0, b=1, mean=0.99, sd=0.05)
  reservoir.risk <- rtruncnorm(num.wells, a=0, b=1, mean=0.8, sd=0.1)
  
  # Calculate the probability each well produces
  p.produce <- hydro.risk * reservoir.risk
  # Calculate whether each well produces or not
  v.produce <- rbernoulli(num.wells,p.produce)
  produce <- ifelse(v.produce,1,0)
  
  # Get proportion of wet wells and store simulated values
  prop.dist[j] <- mean(produce)
  hydro.dist <- c(hydro.dist,hydro.risk)
  reser.dist <- c(reser.dist,reservoir.risk)
}

proc.time() - ptm

library(Hmisc)
describe(prop.dist)

# Get the 5% VaR and CVaR
VaR <- quantile(prop.dist, probs=0.05)
VaR
CVaR <- mean(prop.dist[prop.dist < VaR])
CVaR

mean(hydro.dist)
mean(reser.dist)
median(prop.dist)

# Make into dataframe for plotting
prop.df <- as.data.frame(prop.dist)
hydro.df <- as.data.frame(hydro.dist)
reser.df <- as.data.frame(reser.dist)

# Pretty plots
ggplot(prop.df) +
  geom_histogram(mapping = aes(prop.dist), bins = 50, colour = "black", fill = "lightblue") +
  xlab("Projected Proportion of Wet Wells") +
  ylab("Count") +
  ggtitle("Simulated Distribution of Wet Well Proportion") +
  geom_vline(xintercept  = VaR, colour = "red", lwd = 1.25) +
  #scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(0,1,0.1), limits = c(0,1)) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(hydro.df) +
  geom_histogram(mapping = aes(hydro.dist), bins = 50, colour = "black", fill = "lightblue") +
  xlab("Projected Hydocarbon Risk") +
  ylab("Count") +
  ggtitle("Simulated Distribution for Hydrocarbon Risk") +
  geom_vline(xintercept  = mean(hydro.dist), colour = "red", lwd = 1.25) +
  #scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(0.8,1,0.1), limits = c(0.8,1)) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(reser.df) +
  geom_histogram(mapping = aes(reser.dist), bins = 50, colour = "black", fill = "lightblue") +
  xlab("Projected Reservoir Risk") +
  ylab("Count") +
  ggtitle("Simulated Distribution of Reservoir Risk") +
  geom_vline(xintercept  = mean(reser.dist), colour = "red", lwd = 1.25) +
  #scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(0.3,1,0.1), limits = c(0.3,1)) +
  theme(axis.text.x = element_text(angle = 45))

ggplot(prop.df %>% filter(prop.dist <= VaR)) +
  geom_histogram(mapping = aes(prop.dist), bins = 50, colour = "black", fill = "lightblue") +
  xlab("Projected Proportion of Wet Well (Bottom 5th Percentile)") +
  ylab("Count") +
  ggtitle("Simulated Distribution for Bottom 5th Percentile of Wet Well Proportion") +
  geom_vline(xintercept  = CVaR, colour = "red", lwd = 1.25) +
  #scale_y_continuous(labels = percent_format()) +
  scale_x_continuous(breaks = seq(min(prop.dist),VaR + 0.05,0.05), limits = c(min(prop.dist),VaR + 0.05)) +
  theme(axis.text.x = element_text(angle = 45))
