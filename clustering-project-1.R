
#####################################################
#Geocode Location
#####################################################
# Need experimental version of ggmap
# also need to download and install rtools off of the internet
#install.packages('devtools')
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
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

#register_google("[YOUR API KEY HERE]")

nrc_total <- get_sentiments('afinn')

reviews <- read_csv('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\boston-airbnb-open-data\\reviews.csv')

rv <- reviews %>% group_by(listing_id) %>% count(listing_id,sort=T) %>%
  filter(n>=4) %>% select(-'n')

new_reviews <- reviews %>% group_by(listing_id) %>%
    unnest_tokens(word, comments) %>%
    right_join(rv, by='listing_id') %>% filter(!is.na(word)) %>%
    left_join(nrc_total, by='word') %>% filter(!is.na(score))
  
score <- new_reviews %>% group_by(listing_id) %>% mutate(sscore=sum(score)) %>% distinct(listing_id,sscore)
nwords <- new_reviews %>% group_by(listing_id) %>% count(listing_id)
  
complete <- nwords %>% left_join(score, 'listing_id') %>% mutate(avg=sscore/n)
complete$avg <- scale(complete$avg)


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
listing_k <-data.frame(listing_id = listings$id, lat = listings$latitude, lon = listings$longitude)

combined <- complete %>% left_join(listing_k,by='listing_id')
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

toc <- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toc),method='complete')
clusters.a <- hclust(dist(toc),method='average')
clusters.s <- hclust(dist(toc),method='single')

# The dendograms looked pretty garbage so let's use kmeans
library(factoextra)
library(mclust)
fviz_nbclust(toc, kmeans, method='wss')
fviz_nbclust(toc, kmeans, method='gap')
fviz_nbclust(toc, kmeans, method='silhouette')

# Four looks decent?
kmeans_4 <- kmeans(toc,4)


combined$clus <- as.factor(kmeans_4$cluster)
#########################################
# Get a Map of Boston (uses stamenmap instead of google)
#########################################
boston <- get_stamenmap(bbox=c(left=-71.201596,bottom=42.221039,right=-70.941457,top=42.398561),zoom=12)
ggmap(boston)

# Filter the properites to their clusters
clu1 <- combined %>% filter(clus==1)
clu2 <- combined %>% filter(clus==2)
clu3 <- combined %>% filter(clus==3)
clu4 <- combined %>% filter(clus==4)



ggmap(boston, fullpage = TRUE,alpha=0.7) +
  geom_point(data=combined, aes(x=lon,y=lat,color=clus),size=2) +
  scale_color_discrete()



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
