library(tidyverse)
library(survival)
library(survminer)
#install.packages("flexsurv")
library(flexsurv)
library(dplyr)
library(RColorBrewer)

#Sage
pumps <- read_csv('C:\\Users\\parma\\Documents\\Survival\\survivalcsv\\katrina.csv')

#trying different distributions
#only looking at flood fail condition: reason==1
#weibull - maybe - is the best, log-logistic is a bit worse
#makes sense because its used to model things that wear out over time
fit_wb <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
                      + slope + age, data = pumps, dist = "weibull")
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution")
#looking at first 15 points
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution", xmax=15)
#exponential - NOPE
# fit_exp <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
#                       + slope + age, data = pumps, dist = "exponential")
# plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
#      xlab = "hour", ylab = "cumulative hazard", main = "exponential distribution")
#lognormal - Maybe - look into first 15 points - NOPE
# fit_lnorm <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
#                       + slope + age, data = pumps, dist = "lognormal")
# plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
#      xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution")
# #looking at first 15 points
# plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
#      xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution",xmax=15)
#log-logistic - maybe - look into first 15 points - NOPE
# fit_llogis <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
#                       + slope + age, data = pumps, dist = "llogis")
# plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
#      xlab = "hour", ylab = "cumulative hazard", main = "log-logistic distribution distribution")
# plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
#      xlab = "hour", ylab = "cumulative hazard", main = "log-logistic distribution distribution",xmax=15)

fit <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
               + slope + age, data = pumps, dist = "weibull")
summary(fit)
exp(coef(fit))

# predicted quantiles
survprob_75_50_25 <- predict(fit, type = "quantile", se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(survprob_75_50_25$fit)

# #predicted time to failure - not sure how to interpret this though...
# pred_time <- predict(fit, type = "response", se.fit = TRUE)
# with(pred_time, head(cbind(fit, se.fit)))
# 
# #see predicted survival probabilities at the time where we actually observed the event
# survprob_actual <- 1 - psurvreg(pumps$hour,
#                                 mean = predict(fit, type = "lp"),
#                                 scale = fit$scale,
#                                 distribution = fit$dist)
# head(survprob_actual)
