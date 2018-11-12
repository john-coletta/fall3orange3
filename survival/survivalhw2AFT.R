library(tidyverse)
library(survival)
library(survminer)
#install.packages("survminer")
#install.packages("flexsurv")
library(flexsurv)
library(dplyr)
library(RColorBrewer)

pumps <- read_csv('C:\\Users\\Nupur Baviskar\\Desktop\\Nupur IAA Material\\FALL 3\\SURVIVAL ANALYSIS\\survivalcsv\\katrina.csv')

pumps = read_csv("C:\\Users\\parma\\Documents\\Survival\\survivalcsv\\katrina.csv")
#trying different distributions
#only looking at flood fail condition: reason==1
#weibull - maybe - is the best, log-logistic is a bit worse
#makes sense because its used to model things that wear out over time
fit_wb <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
                      + slope + age, data = pumps, dist = "weibull")
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, xaxp =c(0,48,8) ,
     xlab = "Hour", ylab = "Cumulative Hazard", main = "Weibull Distribution")
#looking at first 15 points
plot(fit_wb, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
     xlab = "hour", ylab = "cumulative hazard", main = "weibull distribution", xmax=15)
#exponential - NOPE
fit_exp <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
                  + slope + age, data = pumps, dist = "exponential")
plot(fit_exp, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, xaxp =c(0,48,8) ,
      xlab = "hour", ylab = "cumulative hazard", main = "exponential distribution")
#lognormal - Maybe - look into first 15 points - NOPE
fit_lnorm <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
                       + slope + age, data = pumps, dist = "lognormal")
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, xaxp =c(0,48,8),
      xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution")
# #looking at first 15 points
plot(fit_lnorm, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
      xlab = "hour", ylab = "cumulative hazard", main = "lognormal distribution",xmax=15)
#log-logistic - maybe - look into first 15 points - NOPE
fit_llogis <- flexsurvreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
                       + slope + age, data = pumps, dist = "llogis")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, xaxp =c(0,48,8),
      xlab = "hour", ylab = "cumulative hazard", main = "log-logistic distribution distribution")
plot(fit_llogis, type = "cumhaz", ci = TRUE, conf.int = FALSE, las = 1, bty = "n",
      xlab = "hour", ylab = "cumulative hazard", main = "log-logistic distribution distribution",xmax=15)

fit <- survreg(Surv(hour, reason == 1) ~ backup + bridgecrane + servo + trashrack + elevation
               + slope + age, data = pumps, dist = "weibull")
summary(fit)
exp(coef(fit))

# predicted quantiles
survprob_75_50_25 <- predict(fit, type = "quantile", se.fit = TRUE,
                             p = c(0.25, 0.5, 0.75))
head(survprob_75_50_25$fit)

# #predicted time to failure.
pred_time <- predict(fit, type = "response", se.fit = TRUE)
#with(pred_time, head(cbind(fit, se.fit)))

#see predicted survival probabilities at the time where we actually observed the event - STILL WORKING ON
#do we need to specify we're only looking at flooding?
#actually gives you the info for hour 48
survprob_actual <- 1 - psurvreg(pumps$hour,
                                mean = predict(fit, type = "lp"),
                                scale = fit$scale,
                                distribution = fit$dist)
head(survprob_actual)

#look at survival probability at 48 hour mark
survprob_48hr <- 1 - psurvreg(48, mean = predict(fit, type = "lp"), 
                              scale = fit$scale,
                              distribution = fit$dist)
head(survprob_48hr)

#looking into backup, servo, elevation, age - all that have betas that increase the survial time 
#don't look into elevation and age - we're only looking into upgrades - you can't do upgrades with
#elevation and age

#servo
pumps_noservo <- pumps %>%
  mutate(old_lp = predict(fit, type = "lp"),
         ID = row_number()) %>%
  dplyr::filter(reason == 1, servo == 0) %>%
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_servo = servo,
         servo = old_servo + 1)

results_servo = pumps_noservo %>%
  mutate(new_lp = predict(fit, newdata = pumps_noservo, type = "lp"),
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
head(results_servo)

#backup
pumps_nobup <- pumps %>%
  mutate(old_lp = predict(fit, type = "lp"),
         ID = row_number()) %>%
  dplyr::filter(reason == 1, backup == 0) %>%
  mutate(old_time = hour,
         surv_prob = 1 - psurvreg(old_time,
                                  mean = old_lp,
                                  scale = fit$scale,
                                  distribution = fit$dist),
         old_backup = backup,
         backup = old_backup + 1)

results_backup = pumps_nobup %>%
  mutate(new_lp = predict(fit, newdata = pumps_nobup, type = "lp"),
         new_time = qsurvreg(1 - surv_prob,
                             mean = new_lp,
                             scale = fit$scale,
                             distribution = fit$dist),
         pred_time_diff = new_time - old_time) %>%
  select(ID, surv_prob, old_time, new_time, pred_time_diff)
head(results_backup)

write.csv(results_backup,file = 'C:\\Users\\Nupur Baviskar\\Desktop\\Nupur IAA Material\\FALL 3\\SURVIVAL ANALYSIS\\survivalcsv\\backup.csv')
write.csv(results_servo,file = 'C:\\Users\\Nupur Baviskar\\Desktop\\Nupur IAA Material\\FALL 3\\SURVIVAL ANALYSIS\\survivalcsv\\servo.csv')

write.csv(results_backup,file = 'C:\Users\parma\Documents\\Survival\\backup.csv')
write.csv(results_servo,file = 'C:\\Users\\parma\\Documents\\Survival\\servo.csv')

