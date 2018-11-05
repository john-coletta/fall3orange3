library(tidyverse)
library(survival)
library(survminer)
library(muhaz)
library(dplyr)

# Read in the data file
#John
#pumps <- read_csv('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Survival Analysis\\survivalcsv\\katrina.csv')
#Sage
pumps <- read_csv('C:\\Users\\parma\\Documents\\Survival\\survivalcsv\\katrina.csv')

head(pumps)

# Percent of pumps that failed/did not survive: 58.96%
(1 - mean(pumps$survive))

#percent of pumps that did survive: 41.04%
mean(pumps$survive)

# Percent for each failure
# Flood (reason = 1): 14.94%
count(pumps[pumps$reason==1,])/count(pumps)
# Motor (reason = 2): 14.55
count(pumps[pumps$reason==2,])/count(pumps)
# Surge (reason = 3): 14.42
count(pumps[pumps$reason==3,])/count(pumps)
# Jammed (reason = 4): 15.06%
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
#shows you censored with +s
with(pumps, Surv(time=hour, event = fail == 1))

pumps_fit <- survfit(Surv(hour, fail==1) ~ 1, data=pumps)
pumps_fit
summary(pumps_fit)

# Plotting below
ggsurvplot(pumps_fit, data=pumps, conf.int=F, palette='#377EB8',
           surv.median.line = "hv",
           ggtheme = theme_minimal(), 
           title="Survival Probability by Hour: All Pumps",
           break.x.by=5, xlim=c(0,50),
           xlab="Hour")

# By failure reason
pumps_reason <- survfit(Surv(hour, fail==1) ~ reason, data=pumps)

pumps_reason_no_survive <- survfit(Surv(hour, fail==1) ~ reason, data=pumps[pumps$reason != 0,])

# Plot
# ggsurvplot(pumps_reason, data=pumps, conf.int=F,palette='hue',
#            title="Survival Plot for New Orleans' Wells",
#            ggtheme=theme_minimal(),
#            xlab='Hour',
#            ylab='Survival Probability',
#            legend.labs=c('Survived','Flooded','Motor Failure','Surge','Jammed'))
library("RColorBrewer")
display.brewer.all()
ggsurvplot(pumps_reason_no_survive, data=pumps[pumps$reason != 0,], conf.int=F, 
           palette=brewer.pal(4, "Set1"),
           legend.labs=c('Flooded','Motor Failure','Surge','Jammed'),
           surv.median.line = "h",
           ggtheme = theme_minimal(), 
           title="Survival Probability of Failed Pumps by Failure Condition",
           break.x.by=5, xlim=c(0,50),
           xlab="Hour")

# Log-rank test
pairwise_survdiff(Surv(time=hour, event=fail) ~ reason, data=pumps[pumps$reason != 0,])

#added to check the difference in p-values between including surviavl and not, p-values are still insanely low
#survdiff(Surv(hour,fail)~reason, data=pumps)
#survdiff(Surv(hour,fail)~reason, data=pumps[pumps$reason != 0,])

# Hazard plots
pumps$hour2 <- ifelse(pumps$hour==48 & pumps$fail==0, 49, pumps$hour)

pumps_haz <- with(pumps, kphaz.fit(hour2,fail))
pumps_haz2 <- with(pumps, kphaz.fit(hour,fail))

kphaz.plot(pumps_haz, main = "Hazard Function for All Pumps")
kphaz.plot(pumps_haz2, main= 'hazard function')
# Cumulative hazard plot
ggsurvplot(pumps_fit, fun = "cumhaz", palette = "#377EB8",
           ggtheme=theme_minimal(),
           title="Cumulative Hazard Plot for All Pumps",
           xlab='Hour',ylab='Cumulative Hazard',
           legend.labs='All Wells',
           break.x.by=5, xlim=c(0,50))
# Cumulative hazard plot stratifid
# ggsurvplot(pumps_reason, fun = 'cumhaz', palette = 'hue',
#            title="Stratified Cumulative Hazard Plot for New Orleans' Wells",
#            xlab='Hour',
#            ylab='Cumulative Hazard',
#            legend.labs=c('Survived','Flooded','Motor Failure','Surge','Jammed'),
#            ggtheme=theme_minimal())
ggsurvplot(pumps_reason_no_survive, fun='cumhaz', palette=brewer.pal(4, "Set1"),
           ggtheme=theme_minimal(),
           title="Cumulative Hazard Plot by Failure Condition for Pumps",
           xlab='Hour',ylab='Cumulative Hazard',
           legend.labs=c('Flooded','Motor Failure','Surge','Jammed'),
           break.x.by=5, xlim=c(0,50))



ggsurvplot(pumps_reason_no_survive, data=pumps[pumps$reason != 0,], conf.int=F, 
           palette=brewer.pal(4, "Set1"),
           legend.labs=c('Flooded','Motor Failure','Surge','Jammed'),
           surv.median.line = "h",
           ggtheme = theme_minimal(), 
           title="Survival Probability of Failed Pumps by Failure Condition",
           break.x.by=5, xlim=c(0,50),
           xlab="Hour")

