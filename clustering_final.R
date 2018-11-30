load('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\final_data.RData')

names(final_data)

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

new_cdata <- cdata[2:65]
cov_X <- cov(new_cdata)
eig_cov <- eigen(cov_X)

means <- colMeans(new_cdata)
means_matrix <- matrix( means, nrow = nrow(new_cdata), ncol = ncol(new_cdata), byrow = TRUE)
mean_centered_X <- new_cdata - means_matrix

X_scores <- as.matrix(mean_centered_X) %*% eig_cov$vectors

eig_matrix <- matrix( (1 / eig_cov$values), nrow = nrow(X_scores), ncol = ncol(X_scores), byrow = TRUE)

standard_X <- X_scores * eig_matrix

# using principal components function

princ_comps <- princomp(new_cdata, cor = FALSE)

princ_scores <- princ_comps$scores

# listing first 5 standard deviations

first5_stdevs <- princ_comps$sdev[1:5]

first5_stdevs


# computing optimal number of clusters using kmeans on pca scores

if(!require("factoextra"))install.packages("factoextra")
library(factoextra)

fviz_nbclust(princ_scores, kmeans, method = "wss")
# optimal number of clusters using kmeans and wss appers to be 4

fviz_nbclust(princ_scores, kmeans, method = "silhouette")
# optimal number of clusters using kmeans and silhouette appears to be 2

#-----------------------------------------------------------------------------
# The wss method give us 4 clusters and the silhouhette method give us 2.
# The wss and silhouette can produce different results
# this is due to the fact that wss seeks to minimize the distance cluster
# distance, while the silhouette statistic aims to maximize the difference
# between the between cluster distance and within cluster distance for each
# observation, normalized by the maximum distance between that observation
# and a observation in another group.
#-----------------------------------------------------------------------------

# kmeans clustering using 4 clusters

set.seed(12345)

# running k-means with 4 clusters
kmeans4 <- kmeans(princ_scores, centers = 4)
# adding cluster label to dataset
new_cdata$clusts <- kmeans4$cluster
# getting mean spirometry of clusters
summarized_og <- new_cdata %>% group_by(clusts) %>% summarise_all("mean")
# ignoring first 5 columns for plotting the spirometry
summarized <- summarized_og[, 6: ncol(summarized_og)]
# getting means to graph spriometry, uncentered
means <- colMeans(new_cdata[, 5: (ncol(new_cdata) - 1 )])
means_matrix <- matrix(means, nrow = nrow(summarized), ncol = ncol(summarized), byrow = TRUE)
# adding means back into pca scores to uncenter
uncentered_summary <- summarized + means_matrix
# multiplying the mean coeficients by base spline variables to create the y
spiro_means <- as.matrix(uncentered_summary) %*% t(X)
spiro_means <- as.data.frame(spiro_means)
# adding cluster labels to spirometry scores
spiro_means$clusts <- summarized_og$clusts
# transforming matrix so theres a column for each cluster
spiro_meansT <- t(spiro_means)
spiro_meansT <- spiro_meansT[1:( nrow(spiro_meansT) - 1 ), ]
# naming the columns
colnames(spiro_meansT) <- c("cluster1", "cluster2", "cluster3", "cluster4")
# adding time to the dataframe
spiro_meansT <- as.data.frame(spiro_meansT)
spiro_meansT$time <- times

# plotting the unscaled spirometry scores over time
ggplot(spiro_meansT) +
  geom_line(mapping = aes(x = time, y = cluster1, colour = "cluster1")) +
  geom_line(mapping = aes(x = time, y = cluster2, colour = "cluster2")) +
  geom_line(mapping = aes(x = time, y = cluster3, colour = "cluster3")) + 
  geom_line(mapping = aes(x = time, y = cluster4, colour = "cluster4")) +
  scale_colour_manual("", 
                      breaks = c("cluster1", "cluster2", "cluster3", "cluster4"),
                      values = c("blue", "red", "green", "purple")) +
  ylab("Mean Spirometry") +
  xlab("Time (seconds)") +
  ggtitle("Mean Spirometry by Cluster Across Time")


# plotting the scaled spirometry times 
spiro_means1 <- as.matrix(summarized) %*% t(X)
spiro_means1 <- as.data.frame(spiro_means1)

spiro_means1$clusts <- summarized_og$clusts
spiro_meansT1 <- t(spiro_means1)
spiro_meansT1 <- spiro_meansT1[1:( nrow(spiro_meansT1) - 1 ), ]
colnames(spiro_meansT1) <- c("cluster1", "cluster2", "cluster3", "cluster4")
spiro_meansT1 <- as.data.frame(spiro_meansT1)
spiro_meansT1$time <- times

ggplot(spiro_meansT1) +
  geom_line(mapping = aes(x = time, y = cluster1, colour = "cluster1")) +
  geom_line(mapping = aes(x = time, y = cluster2, colour = "cluster2")) +
  geom_line(mapping = aes(x = time, y = cluster3, colour = "cluster3")) + 
  geom_line(mapping = aes(x = time, y = cluster4, colour = "cluster4")) +
  scale_colour_manual("", 
                      breaks = c("cluster1", "cluster2", "cluster3", "cluster4"),
                      values = c("blue", "red", "green", "purple")) +
  ylab("Mean Spirometry") +
  xlab("Time (seconds)") +
  ggtitle("Mean Spirometry(scaled) by Cluster Across Time")

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
