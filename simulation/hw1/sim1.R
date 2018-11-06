library(graphics)
library(ks)
library(dplyr)
library(triangle)
library(nortest)

# loading in the data
actual_oil <- read.csv("./actual_oil.csv")
predicted_oil <- read.csv("./predicted_oil.csv")

colnames(actual_oil)[4] = "natural"
colnames(actual_oil)[6] = "natural_change"


# exploring each of the individual arithmetic changes in cost
hist(actual_oil$crude_change)
hist(actual_oil$well_change)
hist(actual_oil$natural_change)

# exploring distribution of all arithmetic changes of costs
hist(c(actual_oil$crude_change, actual_oil$well_change, actual_oil$natural_change))

# filtering the data to only include years between 1990 and 2006
# 2007 is taken out because it was considered an outlier
filtered_data <- filter(actual_oil, year > 1990, year < 2007)

# combining all of the arithmetic changes in order to have more observations
observations <- c(filtered_data$well_change, filtered_data$natural_change, filtered_data$crude_change)

# calculating the bandwidth to use in the kernel density function
density.oil <- density(observations, bw = "SJ-ste")
density.oil

# creating 10000 random draws from our kde that was created from historical data
est.oil <- rkde( fhat = kde( observations, H = density.oil$bw), n = 10000)

# exploring distribution of our random draws from out kde
hist(est.oil)

# setting seed to be able to replicate results
set.seed(112358)

#---------------------------------------------------------------------
# simulation using kernel density estimation
#---------------------------------------------------------------------
# creating a data frame of all of the simulated returns
sim_data_kde <- data.frame(
  sim_2006 = rkde( fhat = kde( observations, H = density.oil$bw), n = 10000),
  sim_2007 = rkde( fhat = kde( observations, H = density.oil$bw), n = 10000),
  sim_2008 = rkde( fhat = kde( observations, H = density.oil$bw), n = 10000),
  sim_2009 = rkde( fhat = kde( observations, H = density.oil$bw), n = 10000),
  sim_2010 = rkde( fhat = kde( observations, H = density.oil$bw), n = 10000),
  sim_2011 = rkde( fhat = kde( observations, H = density.oil$bw), n = 10000),
  sim_2012 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2013 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2014 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2015 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2016 = rtriangle(10000, a = .02, b = .06, c = .05),
  sim_2017 = rtriangle(10000, a = .02, b = .06, c = .05),
  sim_2018 = rtriangle(10000, a = .02, b = .06, c = .05)
)

# creating a starting point for the oil price
# using 2006's average of crude, natural, and well price
starting_point <- mean( as.numeric(actual_oil[actual_oil$year == 2006, c("crude", "natural", "well") ] ) )

# creating vector of starting point 10000 times so we can do 10000 simulations
sim_2019_kde <- rep(starting_point, 10000)

# loop that goes from starting point until 2019 and continously mutiplies the rate from the next year
# in order to get a 2019 price estimate
for(i in 2:ncol(sim_data)){
  sim_2019_kde <- sim_2019_kde * (1 + sim_data_kde[, i])
}

# exploring distribution of 2019 simulated costs
hist(sim_2019_kde)
mean(sim_2019_kde)
median(sim_2019_kde)

#-------------------------------------------------------------------------
# simulation using assumption of normal distribution for years 2006 - 2012
#-------------------------------------------------------------------------

mean2006_2012 <- mean(observations)
sd2006_2012 <- sd(observations)

# creating a data frame of all of the simulated returns
sim_data_norm <- data.frame(
  sim_2006 = rnorm(n = 10000, mean = mean2006_2012, sd = sd2006_2012),
  sim_2007 = rnorm(n = 10000, mean = mean2006_2012, sd = sd2006_2012),
  sim_2008 = rnorm(n = 10000, mean = mean2006_2012, sd = sd2006_2012),
  sim_2009 = rnorm(n = 10000, mean = mean2006_2012, sd = sd2006_2012),
  sim_2010 = rnorm(n = 10000, mean = mean2006_2012, sd = sd2006_2012),
  sim_2011 = rnorm(n = 10000, mean = mean2006_2012, sd = sd2006_2012),
  sim_2012 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2013 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2014 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2015 = rtriangle(10000, a = .07, b = .22, c = .0917),
  sim_2016 = rtriangle(10000, a = .02, b = .06, c = .05),
  sim_2017 = rtriangle(10000, a = .02, b = .06, c = .05),
  sim_2018 = rtriangle(10000, a = .02, b = .06, c = .05)
)

# creating a starting point for the oil price
# using 2006's average of crude, natural, and well price
starting_point <- mean( as.numeric(actual_oil[actual_oil$year == 2006, c("crude", "natural", "well") ] ) )

# creating vector of starting point 10000 times so we can do 10000 simulations
sim_2019_norm <- rep(starting_point, 10000)

# loop that goes from starting point until 2019 and continously mutiplies the rate from the next year
# in order to get a 2019 price estimate
for(i in 2:ncol(sim_data_norm)){
  sim_2019_norm <- sim_2019_norm * (1 + sim_data_norm[, i])
}

# exploring distribution of 2019 simulated costs
hist(sim_2019_norm)
mean(sim_2019_norm)
median(sim_2019_norm)

# testing 2006 - 2012 distribution for normality

obs_2006_2012 <- c(sim_data_kde$sim_2006,
                   sim_data_kde$sim_2007,
                   sim_data_kde$sim_2008,
                   sim_data_kde$sim_2009,
                   sim_data_kde$sim_2010,
                   sim_data_kde$sim_2011,
                   sim_data_kde$sim_2012)

hist(obs_2006_2012)

# using a qqplot
qqnorm(obs_2006_2012, pch = 1, frame = FALSE)
qqline(obs_2006_2012, col = "steelblue", lwd = 2)

# formal test for normality
shapiro.test(sample(obs_2006_2012, 5000))
ad.test(sample(obs_2006_2012, 5000))


