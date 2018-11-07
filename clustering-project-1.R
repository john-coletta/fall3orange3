
#####################################################
#Geocode Location
#####################################################
# Need experimental version of ggmap
# also need to download and install rtools off of the internet
library(devtools)
devtools::install_github("dkahle/ggmap", ref = "tidyup")
#####################################################
#TO DO THIS YOU NEED TO REGISTER TO A GOOGLE DEV ACCOUNT
#AND PROVIDE YOUR OWN API KEY!!!!!
#THEN YOU CAN GET A PRETTY MAP!
# https://www.wpgmaps.com/documentation/creating-a-google-maps-api-key/
# You will also have to get the geocode api enabled
#####################################################
library(readr)
library(lubridate)
library(ggplot2)
library(ggmap)
library(dplyr)
library(data.table)
library(ggrepel)
listings <- read_csv("C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\boston-airbnb-open-data\\listings.csv")

register_google("[YOUR API KEY HERE]")

nrc_total <- get_sentiments('afinn')

reviews <- read_csv('C:\\Users\\johnb\\OneDrive\\Documents\\MSA\\Fall 3\\Clustering\\boston-airbnb-open-data\\reviews.csv')

rv <- reviews %>% group_by(listing_id) %>% count(listing_id, sort=T) %>%
  filter(n>4) %>% select(-'n')

new_reviews <- rv %>% group_by(listing_id) %>%
  unnest_tokens(word, comments) %>%
  right_join(rv, by='listing_id') %>% filter(!is.na(word)) %>%
  left_join(nrc_total, by='word') %>% filter(!is.na(score))

score <- new_reviews %>% group_by(listing_id) %>% mutate(sscore=sum(score)) %>% distinct(listing_id,score)
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
listing_k <-data.frame(listing_id = listings$id, lat = lat, lon = lon)

combined <- complete %>% left_join(listing_k,by='listing_id')
combined$std.lat <- scale(combined$lat)
combined$std.lon <- scale(combined$lon)

toc <- cbind(combined$avg,combined$std.lat,combined$std.lon)

clusters.c <- hclust(dist(toc),method='complete')
clusters.a <- hclust(dist(toc),method='average')
clusters.s <- hclust(dist(toc),method='single')

plot(clusters.c)
#########################################
# Get a Map of Seattle
#########################################
#map <- get_map(location = "Seattle", zoom = 12)
#map2 <- get_map(location = "Seattle", zoom = 11)

#I want to find the Geo-location of these listings :-)

cluster <- words_and_clusters %>% filter(group == 9) %>%
  left_join(listing_k,"listing_id") %>%
  distinct(listing_id,.keep_all = TRUE)

#notice U of Washington Right in the middle
ggmap(map, fullpage = TRUE) +
  geom_point(data = cluster, aes(x = lon, y = lat), color = 'red', size = 2)

ggmap(map2, fullpage = TRUE) +
  geom_point(data = cluster, aes(x = lon, y = lat), color = 'red', size = 2)


### BELOW CODE IS FOR WORD CLOUD ###
words <- new_reviews %>% ungroup() %>% right_join(,by='listing_id') %>%
  select(word) %>% count(word, sort=T) %>% filter(n<150)

library(wordcloud)
wordcloud(words = words$word, freq=words$n, min.freq = 150,
          max.words=100, random.order=F, rot.per=0.35,
          colors=brewer.pal(8, 'Dark2'))


