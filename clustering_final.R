load('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\final_data.RData')

library(splines)
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

clustBIC6 <- Mclust(cdata[,8:65], modelNames='VVV', G=6)
clustBIC6

df <- cdata
df$clust <- as.factor(clustBIC6$classification)

b1 <- matrix(clustBIC6$parameters$mean[,1],60,1)
b2 <- matrix(clustBIC6$parameters$mean[,2],60,1)
b3 <- matrix(clustBIC6$parameters$mean[,3],60,1)
b4 <- matrix(clustBIC6$parameters$mean[,4],60,1)
b5 <- matrix(clustBIC6$parameters$mean[,5],60,1)
b6 <- matrix(clustBIC6$parameters$mean[,6],60,1)
plot(times, X%*%b1, ylab="ML",xlab = "Time",type = 'l',lwd=2,col=1,ylim=c(0,100))
lines(times,X%*%b2,lwd=2,col=2)
lines(times,X%*%b3,lwd=2,col=3)
lines(times,X%*%b4,lwd=2,col=4)
lines(times,X%*%b5,lwd=2,col=5)
lines(times,X%*%b6,lwd=2,col=6)


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
