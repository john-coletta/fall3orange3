library(tidyverse)
library(survival)
library(survminer)
library(muhaz)

# Read in the data file
pumps <- read_csv('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Survival Analysis\\survivalcsv\\katrina.csv')

head(pumps)

# Percent of pumps that survived
(1 - mean(pumps$survive))

# Percent for each failure
# Flood (reason = 1)
count(pumps[pumps$reason==1,])/count(pumps)
# Motor (reason = 2)
count(pumps[pumps$reason==2,])/count(pumps)
# Surge (reason = 3)
count(pumps[pumps$reason==3,])/count(pumps)
# Jammed (reason = 4)
count(pumps[pumps$reason==4,])/count(pumps)

# Now, of the failures, what are the percent breakdowns
# Flood (reason = 1)
count(pumps[pumps$reason==1,])/count(pumps[pumps$survive==0,])
# Motor (reason = 2)
count(pumps[pumps$reason==2,])/count(pumps[pumps$survive==0,])
# Surge (reason = 3)
count(pumps[pumps$reason==3,])/count(pumps[pumps$survive==0,])
# Jammed (reason = 4)
count(pumps[pumps$reason==4,])/count(pumps[pumps$survive==0,])

# Set up the survival analysis
pumps$fail <- ifelse(pumps$survive==0,1,0)
with(pumps, Surv(time=hour, event = fail == 1))

pumps_fit <- survfit(Surv(hour, fail==1) ~ 1, data=pumps)
pumps_fit
summary(pumps_fit)

# Plotting below
ggsurvplot(pumps_fit, data=pumps, conf.int=F, palette='grey')

# By failure reason
pumps_reason <- survfit(Surv(hour, fail==1) ~ reason, data=pumps)

# Plot
ggsurvplot(pumps_reason, data=pumps, conf.int=F,palette='grey')

# Log-rank test
pairwise_survdiff(Surv(time=hour, event=fail) ~ reason, data=pumps[pumps$reason != 0,])

# Hazard plots
pumps$hour2 <- ifelse(pumps$hour==48 & pumps$fail==0, 49, pumps$hour)

pumps_haz <- with(pumps, kphaz.fit(hour2,fail))

kphaz.plot(pumps_haz, main = "hazard function")

# Cumulative hazard plot
ggsurvplot(pumps_fit, fun = "cumhaz", palette = "grey")
# Cumulative hazard plot stratifid
ggsurvplot(pumps_reason, fun = 'cumhaz', palette = 'grey')
