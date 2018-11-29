load('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\final_data.RData')

library(splines)
library(tidyverse)
times <- seq(1,295)/100 # Observations in 1/100th of a second
X <- bs(times,intercept=TRUE,df=60) #create a spline to 
#model the data
betas <- matrix(0,ncol=60,nrow = 6792)
###########################################################
# run a linear regression on each data set
# here I am manipulating my data you I can cluster
###########################################################
for (ii in 1:6792){
  temp <- lm(as.numeric(final_data[ii,6:300])~X-1) #-1 removes the natural intercept
  betas[ii,]  <- coefficients(temp)
}
cdata <- cbind(final_data[,1:5],betas)

#CONVERT EVERTYING TO 'numbers'
cdata$AGE <- as.numeric(cdata$AGE)
cdata$EVER_SMOKE <- as.numeric(cdata$EVER_SMOKE)
cdata$ASTHMA <- as.numeric(cdata$ASTHMA)
cdata$POVERTY_RATIO <- as.numeric(cdata$POVERTY_RATIO)

# Run before every cluster
set.seed(12345)
library(mclust)

clustBIC <- mclustBIC(cdata[,10:20], modelNames='VVV', G=1:20)
plot(clustBIC)
abline(v=13, lty=2)
clustBIC
set.seed(12345)
clustBIC6 <- Mclust(cdata[,10:20], modelNames='VVV', G=6)
clustBIC6

df <- cdata
df$clust <- as.factor(clustBIC6$classification)

clus_means <- df %>% group_by(clust) %>% summarise_all(funs(mean))

b1 <- as.numeric((clus_means %>% filter(clust==1) %>% select(-c('clust','SEQN','AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))))
b2 <- as.numeric((clus_means %>% filter(clust==2) %>% select(-c('clust','SEQN','AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))))
b3 <- as.numeric((clus_means %>% filter(clust==3) %>% select(-c('clust','SEQN','AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))))
b4 <- as.numeric((clus_means %>% filter(clust==4) %>% select(-c('clust','SEQN','AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))))
b5 <- as.numeric((clus_means %>% filter(clust==5) %>% select(-c('clust','SEQN','AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))))
b6 <- as.numeric((clus_means %>% filter(clust==6) %>% select(-c('clust','SEQN','AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))))

clus_df <- as.data.frame(cbind(times,X%*%b1,X%*%b2,X%*%b3,X%*%b4,X%*%b5,X%*%b6))
names(clus_df) <- c('time','clus1','clus2','clus3','clus4','clus5','clus6')
clus_df <- clus_df %>% gather(key=cluster, value=mL, -time)

ggplot(clus_df, aes(x=time, y=mL, color=cluster)) +
  geom_line(lwd=1.25) +
  xlab('Time (s)') +
  ylab('Milliliters') +
  guides(color=guide_legend(title='')) +
  scale_color_brewer(type='qual',palette='Set2',labels=c('Cluster 1','Cluster 2','Cluster 3','Cluster 4','Cluster 5','Cluster 6'))

plot(times, X%*%b1, ylab="mL",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%b2,lwd=2,col=2)
lines(times,X%*%b3,lwd=2,col=3)
lines(times,X%*%b4,lwd=2,col=4)
lines(times,X%*%b5,lwd=2,col=5)
lines(times,X%*%b6,lwd=2,col=6)

clustBIC6$parameters$mean
b1[5:15]

sfun1 <- splinefun(times,X%*%b1)
sfun2 <- splinefun(times,X%*%b2)
sfun3 <- splinefun(times,X%*%b3)
sfun4 <- splinefun(times,X%*%b4)
sfun5 <- splinefun(times,X%*%b5)
sfun6 <- splinefun(times,X%*%b6) #this creates an interpolant of the curve from min(times) to max(times)

integrate(sfun1,min(times),max(times))
integrate(sfun2,min(times),max(times))
integrate(sfun3,min(times),max(times))
integrate(sfun4,min(times),max(times))
integrate(sfun5,min(times),max(times))
integrate(sfun6,min(times),max(times)) #this will find the area under the curve

clus_means %>% filter(clust==4) %>% select(c('AGE','EVER_SMOKE','ASTHMA','POVERTY_RATIO'))
