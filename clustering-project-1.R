
#####################################################
#Geocode Location
#####################################################
# Need experimental version of ggmap
# also need to download and install rtools off of the internet
#install.packages('devtools')
library(devtools)
#devtools::install_github("dkahle/ggmap", ref = "tidyup")
#####################################################
#TO DO THIS YOU NEED TO REGISTER TO A GOOGLE DEV ACCOUNT
#AND PROVIDE YOUR OWN API KEY!!!!!
#THEN YOU CAN GET A PRETTY MAP!
# https://www.wpgmaps.com/documentation/creating-a-google-maps-api-key/
# You will also have to get the geocode api enabled
#####################################################
library(lubridate)
library(ggplot2)
library(ggmap)
library(data.table)
library(ggrepel)
library(tidytext)
library(stringr)
library(dplyr)
library(text2vec)
library(readr)
listings <- read_csv("C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\boston-airbnb-open-data\\listings.csv")
set.seed(42)
#register_google("[YOUR API KEY HERE]")

nrc_total <- get_sentiments('afinn')

reviews <- read_csv('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\boston-airbnb-open-data\\reviews.csv')

# Below we calculate the sentiment score for each review
rv <- reviews %>% group_by(listing_id) %>% count(listing_id,sort=T) %>%
  filter(n>=4) %>% select(-'n')

new_reviews <- reviews %>% group_by(listing_id) %>%
    unnest_tokens(word, comments) %>%
    right_join(rv, by='listing_id') %>% filter(!is.na(word)) %>%
    left_join(nrc_total, by='word') %>% filter(!is.na(score))
  
score <- new_reviews %>% group_by(listing_id) %>% mutate(sscore=sum(score)) %>% distinct(listing_id,sscore)
nwords <- new_reviews %>% group_by(listing_id) %>% count(listing_id)
  
complete <- nwords %>% left_join(score, 'listing_id') %>% mutate(avg=sscore/n)
complete$avgscale <- scale(complete$avg)


# Here we calculate the price per bed for each propety
listings$pricenum <- as.numeric(gsub('\\$','',listings$price))
listings$priceperbed <- (listings$pricenum / listings$beds)
listings$listing_id <- listings$id

num_reviews <- select(listings, id, reviews_per_month)

# Some values are NA or Inf so drop those
complete <- complete %>% left_join(listings[,c('listing_id','priceperbed')],by='listing_id') %>%
  filter(!is.na(priceperbed)) %>% filter(!is.infinite(priceperbed))

complete$pricescale <- scale(complete$priceperbed)

# Now let us look at distance to popular attractions
places = c('fenway','oldnorth','tdcenter','faneuil','airport','bunkerhill','mofa')
lats = c(42.346676,42.366326,42.366136,42.360191,
         42.366209,42.376310,42.339479)
lons = c(-71.097221, -71.054485,-71.061865,-71.056198,
         -71.019709,-71.060775,-71.093892)

places.df <- data.frame(place=places,lat=lats,lon=lons)
# uncomment to run
# retrieves the lat and lon of each listing

#strtAddress <- listings$street
#lon<- matrix(0,nrow=length(strtAddress))
#lat<- matrix(0,nrow=length(strtAddress))
#for (ii in 1:length(strtAddress)){
#    latLon <- geocode(strtAddress[ii],output="latlon")
#    lon[ii] <- as.numeric(latLon[1])
#    lat[ii] <- as.numeric(latLon[2])
#}

# Get the coords for each property
listing_k <-data.frame(listing_id = listings$id, lat = listings$latitude, lon = listings$longitude)

combined <- complete %>% left_join(listing_k,by='listing_id')
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

combined <- left_join(combined, num_reviews, by = c('listing_id'='id'))
combined$reviews_scale <- scale(combined$reviews_per_month)

# Using haversine formula to calculate lat long distance
deg2rad <- function(deg){
  (deg*pi)/(180)
}

R <- 6371e3
for(loc in places){
  phi1 <- deg2rad(combined$lat)

  phi2 <- deg2rad((places.df %>% filter(place==loc))$lat)
  
  delphi <- deg2rad((combined$lat)-(places.df %>% filter(place==loc))$lat)
  dellam <- deg2rad((combined$lon)-(places.df %>% filter(place==loc))$lon)

  a <- sin(delphi/2) * sin(delphi/2) + cos(phi1) * cos(phi2) * sin(dellam/2) * sin(dellam/2)

  c <- 2 * atan2(sqrt(a),sqrt(1-a))
  
  
  d <- R * c
  
  combined$dist <- d
  combined$distscale <- scale(combined$dist)
  names(combined)[names(combined) == 'dist'] <- paste('dist',loc,sep='_')
  names(combined)[names(combined) == 'distscale'] <- paste('distscale',loc,sep='_')
    
  
}

# Below are the various attributes to cluster on
toc <- cbind(combined$std.lat,combined$std.lon,combined$avgscale,combined$pricescale)
toc2 <- combined %>% ungroup() %>% select(avgscale,pricescale,distscale_fenway,distscale_airport,distscale_bunkerhill,
              distscale_faneuil,distscale_mofa,distscale_oldnorth,distscale_tdcenter,reviews_scale)

clusters.c <- hclust(dist(toc),method='complete')
clusters.a <- hclust(dist(toc),method='average')
clusters.s <- hclust(dist(toc),method='single')

# The dendograms looked pretty garbage so let's use kmeans
library(factoextra)
library(mclust)
fviz_nbclust(toc, kmeans, method='wss')
fviz_nbclust(toc, kmeans, method='gap')
fviz_nbclust(toc, kmeans, method='silhouette')

fviz_nbclust(toc2, kmeans, method='wss',k.max=20)
fviz_nbclust(toc2, kmeans, method='gap',k.max=20)
fviz_nbclust(toc2, kmeans, method='silhouette',k.max=20)

# Four looks decent?
kmeans_4 <- kmeans(toc,4)
kmeans_6 <- kmeans(toc,6)
set.seed(42)
kmeans_9 <- kmeans(toc2, 9, iter.max=50)



kmeans_9$centers

cmeans <- colMeans(combined %>% ungroup() %>% select(avg,priceperbed,dist_fenway,dist_airport,dist_bunkerhill,dist_faneuil,dist_mofa,dist_oldnorth,dist_tdcenter,reviews_per_month))
csd <- apply(combined %>% ungroup() %>% select(avg,priceperbed,dist_fenway,dist_airport,dist_bunkerhill,dist_faneuil,dist_mofa,dist_oldnorth,dist_tdcenter,reviews_per_month),
             2,sd)


clusts <- kmeans_9$centers*matrix(csd,byrow=TRUE,nrow=9,ncol=10) + matrix(cmeans,byrow=TRUE,nrow=9,ncol=10)


# Let's try PCA cause why not
pca <- princomp(toc2)
pca$loadings
pca$center

plot(pca, type='l')
# 5 looks like a good elbow
pca$scores[,1:6]


# kmeans time
fviz_nbclust(pca$scores[,1:6], kmeans, method='wss',k.max=20)
fviz_nbclust(pca$scores[,1:6], kmeans, method='gap',k.max=20)
fviz_nbclust(pca$scores[,1:6], kmeans, method='silhouette',k.max=20)

kmeans_10 <- kmeans(pca$scores[,1:6], 10, nstart=25)

kmeans_10$centers


combined$clus_k4 <- as.factor(kmeans_4$cluster)
combined$clus_k6 <- as.factor(kmeans_6$cluster)
combined$clus_k9 <- as.factor(kmeans_9$cluster)
combined$clus_k10 <- as.factor(kmeans_10$cluster)
#### Not sure what the below does, should 'name' the clusters, NOT WORKING #######
# clusters <- list()
# for( i in 1:8){
#   clusters[[i]] <-  combined %>% ungroup() %>% select(avgscale,pricescale,distscale_airport,distscale_bunkerhill,distscale_fenway,
#                                         distscale_faneuil,distscale_mofa,distscale_oldnorth,distscale_tdcenter,clus_k8) %>% filter(clus_k8 == i) %>%
#                               mutate(avg=avgscale) %>% select(-c('avgscale','clus_k8'))
# }
# 
# 
# # Find the means of each cluster to "Name them"
# x <- cbind(colMeans(combined %>% ungroup() %>% select(avg,priceperbed,dist_airport,dist_bunkerhill,dist_fenway,
#                                         dist_faneuil,dist_mofa,dist_oldnorth,dist_tdcenter)))
# 
# X <- cbind(x,t(clusters))
#################################################################
#########################################
# Get a Map of Boston (uses stamenmap instead of google)
#########################################
boston <- get_stamenmap(bbox=c(left=-71.201596,bottom=42.221039,right=-70.941457,top=42.398561),zoom=12)
ggmap(boston)

# Filter the properites to their clusters
clu1 <- combined %>% filter(clus_k4==1)
clu2 <- combined %>% filter(clus_k4==2)
clu3 <- combined %>% filter(clus_k4==3)
clu4 <- combined %>% filter(clus_k4==4)



ggmap(boston, fullpage = TRUE,alpha=0.7) +
  geom_point(data=combined, aes(x=lon,y=lat,color=clus_k9),size=2) +
  scale_color_discrete()


### OUTPUT DATASET ###
library(openxlsx)
wb <- createWorkbook()
for(i in seq(1,9)){
  
  addWorksheet(wb, paste('Segment_',i,sep=''))
  writeData(wb, sheet = i, (combined %>% filter(clus_k9==i) %>% select(listing_id,clus_k9)))
}

saveWorkbook(wb, 'airbnb_property_segments.xlsx', overwrite = T)
### BELOW CODE IS FOR WORD CLOUD ###

# First get the words for each cluster
words1 <- new_reviews %>% ungroup() %>% right_join(clu1,by='listing_id') %>%
  select(word) %>% count(word, sort=T) %>% filter(n<150)

words2 <- new_reviews %>% ungroup() %>% right_join(clu2,by='listing_id') %>%
  select(word) %>% count(word, sort=T) %>% filter(n<150)

words3 <- new_reviews %>% ungroup() %>% right_join(clu3,by='listing_id') %>%
  select(word) %>% count(word, sort=T) %>% filter(n<150)

words4 <- new_reviews %>% ungroup() %>% right_join(clu4,by='listing_id') %>%
  select(word) %>% count(word, sort=T) %>% filter(n<150)

# Now plot the words
library(wordcloud)
wordcloud(words = words1$word, freq=words1$n, min.freq = 150,
          max.words=100, random.order=F, rot.per=0.35,
          colors=brewer.pal(8, 'Dark2'))

wordcloud(words = words2$word, freq=words2$n, min.freq = 150,
          max.words=100, random.order=F, rot.per=0.35,
          colors=brewer.pal(8, 'Dark2'))

wordcloud(words = words3$word, freq=words3$n, min.freq = 150,
          max.words=100, random.order=F, rot.per=0.35,
          colors=brewer.pal(8, 'Dark2'))

wordcloud(words = words4$word, freq=words4$n, min.freq = 150,
          max.words=100, random.order=F, rot.per=0.35,
          colors=brewer.pal(8, 'Dark2'))

### BELOW IS THE MCLUST CODE ###

# Add back in the distributions
toc_dist <- combined[,c('sscore','lat','lon')]
mm_bic <- mclustBIC(toc_dist)
summary(mm_bic)
mclustF <- Mclust(toc_dist,G=9)
plot(mclustF)

combined$mclust <- as.factor(mclustF$classification)

mclu1 <- combined %>% filter(mclust==1)
mclu2 <- combined %>% filter(mclust==2)
mclu3 <- combined %>% filter(mclust==3)
mclu4 <- combined %>% filter(mclust==4)
mclu5 <- combined %>% filter(mclust==5)
mclu6 <- combined %>% filter(mclust==6)
mclu7 <- combined %>% filter(mclust==7)
mclu8 <- combined %>% filter(mclust==8)
mclu9 <- combined %>% filter(mclust==9)

ggmap(boston, fullpage = TRUE,alpha=0.7) +
  geom_point(data=combined, aes(x=lon,y=lat,color=mclust),size=2)

