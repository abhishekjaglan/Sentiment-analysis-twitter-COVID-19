consumer_key <- 'dKwf8z95QrUFWjllrKyuakyse'
consumer_secret <- 'xGgTFqeYkLSoC0MSmQAtkk3SPvMwkdV1hTFjMj4d55F2aqXx8J'
access_key <- '1023429890085937153-Xi9xKoPcwY4WVdF9tuH9kXDz3TIMaY'
access_secret <- 'QWRMwvDhDqEYGeuSGnRIMv5uPMLHKiFWq2DJV8qBQcWGu'
install.packages("twitteR")
library(twitteR)
setup_twitter_oauth(consumer_key = consumer_key,consumer_secret = consumer_secret,access_token = access_key,access_secret = access_secret)

# loading the tweets
tweets_italy <- searchTwitter('coronavirus+italy',n=1000, lang = 'en')
tweets_france <- searchTwitter('coronavirus+france',n=1000, lang = 'en')
tweets_spain <- searchTwitter('coronavirus+spain', n=1000, lang = 'en')
tweets_uk <- searchTwitter('coronavirus+uk',n=1000, lang = 'en')
tweets_germany <- searchTwitter('coronavirus+germany',n=1000, lang = 'en')

# making a csv file from the tweets
tweetsdf <- twListToDF(tweets_italy)
write.csv(tweetsdf, file = '~/Desktop/italy_tweets.csv',row.names = F)
#import the csv file after this step using import tab to decrease the number of variables
tweetsdf <- twListToDF(tweets_france)
write.csv(tweetsdf, file = '~/Desktop/france_tweets.csv',row.names = F)
tweetsdf <- twListToDF(tweets_spain)
write.csv(tweetsdf, file = '~/Desktop/spain_tweets.csv',row.names = F)#import
tweetsdf <- twListToDF(tweets_uk)
write.csv(tweetsdf, file = '~/Desktop/uk_tweets.csv',row.names = F)#import
tweetsdf <- twListToDF(tweets_germany)
write.csv(tweetsdf, file = '~/Desktop/germany_tweets.csv',row.names = F)#import

italy_tweets <- read.csv("~/Desktop/italy_tweets.csv", comment.char="#")
spain_tweets <- read.csv("~/Desktop/spain_tweets.csv", comment.char="#")
france_tweets <- read.csv("~/Desktop/france_tweets.csv", comment.char="#")
uk_tweets <- read.csv("~/Desktop/uk_tweets.csv", comment.char="#")
germany_tweets <- read.csv("~/Desktop/germany_tweets.csv", comment.char="#")

head(italy_tweets)
trend <- availableTrendLocations()
head(trend)
world <- getTrends(1)
world

#reading the text form csv files
install.packages('syuzhet')
library(syuzhet)
install.packages('lubridate')
install.packages('scales')
install.packages('reshape2')
library(lubridate)
library(scales)
library(reshape2)
library(dplyr)
italy_tweet <- iconv(italy_tweets$text, to = 'utf-8-mac')
france_tweet <- iconv(france_tweets$text, to = 'utf-8-mac')
spain_tweet <- iconv(spain_tweets$text, to = 'utf-8-mac')
uk_tweet <- iconv(uk_tweets$text, to = 'utf-8-mac')
germany_tweet <- iconv(germany_tweets$text, to = 'utf-8-mac')

#sentiment analysis
sentiment_italy <- get_nrc_sentiment(italy_tweet)
sentiment_france <- get_nrc_sentiment(france_tweet)
sentiment_spain <- get_nrc_sentiment(spain_tweet)
sentiment_uk <- get_nrc_sentiment(uk_tweet)
sentiment_germany <- get_nrc_sentiment(germany_tweet)

#barplot
par(mfrow = c(2,2))
barplot(colSums(sentiment_italy),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Italy during COVID-19 ')
barplot(colSums(sentiment_spain),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of spain during COVID-19 ')
barplot(colSums(sentiment_france),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of France during COVID-19 ')
barplot(colSums(sentiment_uk),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of UK during COVID-19 ')
barplot(colSums(sentiment_germany),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Germany during COVID-19 ')

europe_score <- vector('list',10)
europe_score[[1]] <- sum(sentiment_france$anger)+sum(sentiment_germany$anger)+sum(sentiment_italy$anger)+sum(sentiment_spain$anger)+sum(sentiment_uk$anger)
europe_score[[2]] <- sum(sentiment_france$anticipation)+sum(sentiment_germany$anticipation)+sum(sentiment_italy$anticipation)+sum(sentiment_spain$anticipation)+sum(sentiment_uk$anticipation)
europe_score[[3]] <- sum(sentiment_france$disgust)+sum(sentiment_germany$disgust)+sum(sentiment_italy$disgust)+sum(sentiment_spain$disgust)+sum(sentiment_uk$disgust)
europe_score[[4]] <- sum(sentiment_france$fear)+sum(sentiment_germany$fear)+sum(sentiment_italy$fear)+sum(sentiment_spain$fear)+sum(sentiment_uk$fear)
europe_score[[5]] <- sum(sentiment_france$joy)+sum(sentiment_germany$joy)+sum(sentiment_italy$joy)+sum(sentiment_spain$joy)+sum(sentiment_uk$joy)
europe_score[[6]] <- sum(sentiment_france$sadness)+sum(sentiment_germany$sadness)+sum(sentiment_italy$sadness)+sum(sentiment_spain$sadness)+sum(sentiment_uk$sadness)
europe_score[[7]] <- sum(sentiment_france$surprise)+sum(sentiment_germany$surprise)+sum(sentiment_italy$surprise)+sum(sentiment_spain$surprise)+sum(sentiment_uk$surprise)
europe_score[[8]] <- sum(sentiment_france$trust)+sum(sentiment_germany$trust)+sum(sentiment_italy$trust)+sum(sentiment_spain$trust)+sum(sentiment_uk$trust)
europe_score[[9]] <- sum(sentiment_france$negative)+sum(sentiment_germany$negative)+sum(sentiment_italy$negative)+sum(sentiment_spain$negative)+sum(sentiment_uk$negative)
europe_score[[10]] <- sum(sentiment_france$positive)+sum(sentiment_germany$positive)+sum(sentiment_italy$positive)+sum(sentiment_spain$positive)+sum(sentiment_uk$positive)

names <- c("anger","anticipation","disgust","fear","joy","sadness","surprise","trust","negative","positive")
for (i in 1:10) {
  print(names[[i]])
  print(europe_score[[i]])
}
barplot(unlist(europe_score),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Europes worst hit countries')


#North America
tweets_usa <- searchTwitter('coronavirus+usa',n=1000, lang = 'en')
tweets_canada <- searchTwitter('coronavirus+canada',n=1000, lang = 'en')
tweets_mexico <- searchTwitter('coronavirus+mexico',n=1000, lang = 'en')

tweetsdf <- twListToDF(tweets_usa)
write.csv(tweetsdf, file = '~/Desktop/usa_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_canada)
write.csv(tweetsdf, file = '~/Desktop/canada_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_mexico)
write.csv(tweetsdf, file = '~/Desktop/mexico_tweets.csv',row.names = F) #import

usa_tweets <- read.csv("~/Desktop/usa_tweets.csv", comment.char="#")
canada_tweets <- read.csv("~/Desktop/canada_tweets.csv", comment.char="#")
mexico_tweets <- read.csv("~/Desktop/mexico_tweets.csv", comment.char="#")

north_america <- c(usa_tweet,canada_tweet,mexico_tweet)
usa_tweet <- iconv(usa_tweets$text, to = 'utf-8-mac')
canada_tweet <- iconv(canada_tweets$text, to = 'utf-8-mac')
mexico_tweet <- iconv(mexico_tweets$text, to = 'utf-8-mac')

sentiment_usa <- get_nrc_sentiment(usa_tweet)
sentiment_canada <- get_nrc_sentiment(canada_tweet)
sentiment_mexico <- get_nrc_sentiment(mexico_tweet)

barplot(colSums(sentiment_usa),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Italy during COVID-19 ')
barplot(colSums(sentiment_canada),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Italy during COVID-19 ')
barplot(colSums(sentiment_mexico),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Italy during COVID-19 ')


namerica_score <- vector('list',10)
namerica_score[[1]] <- sum(sentiment_canada$anger) + sum(sentiment_mexico$anger) + sum(sentiment_usa$anger)
namerica_score[[2]] <- sum(sentiment_canada$anticipation) + sum(sentiment_mexico$anticipation) + sum(sentiment_usa$anticipation)
namerica_score[[3]] <- sum(sentiment_canada$disgust) + sum(sentiment_mexico$disgust) + sum(sentiment_usa$disgust)
namerica_score[[4]] <- sum(sentiment_canada$fear) + sum(sentiment_mexico$fear) + sum(sentiment_usa$fear)
namerica_score[[5]] <- sum(sentiment_canada$joy) + sum(sentiment_mexico$joy) + sum(sentiment_usa$joy)
namerica_score[[6]] <- sum(sentiment_canada$sadness) + sum(sentiment_mexico$sadness) + sum(sentiment_usa$sadness)
namerica_score[[7]] <- sum(sentiment_canada$surprise) + sum(sentiment_mexico$surprise) + sum(sentiment_usa$surprise)
namerica_score[[8]] <- sum(sentiment_canada$trust) + sum(sentiment_mexico$trust) + sum(sentiment_usa$trust)
namerica_score[[9]] <- sum(sentiment_canada$negative) + sum(sentiment_mexico$negative) + sum(sentiment_usa$negative)
namerica_score[[10]] <- sum(sentiment_canada$positive) + sum(sentiment_mexico$positive) + sum(sentiment_usa$positive)

for (i in 1:10) {
  print(names[[i]])
  print(namerica_score[[i]])
}

barplot(unlist(namerica_score),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of North Americas worst hit countries')

#south america
tweets_brazil <- searchTwitter('coronavirus+brazil',n=1000, lang = 'en')
tweets_chile <- searchTwitter('coronavirus+chile',n=1000, lang = 'en')
tweets_ecuador <- searchTwitter('coronavirus+ecuador',n=1000, lang = 'en')

tweetsdf <- twListToDF(tweets_brazil)
write.csv(tweetsdf, file = '~/Desktop/brazil_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_chile)
write.csv(tweetsdf, file = '~/Desktop/chile_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_ecuador)
write.csv(tweetsdf, file = '~/Desktop/ecuador_tweets.csv',row.names = F) #import

brazil_tweets <- read.csv("~/Desktop/brazil_tweets.csv", comment.char="#")
chile_tweets <- read.csv("~/Desktop/chile_tweets.csv", comment.char="#")
ecuador_tweets <- read.csv("~/Desktop/ecuador_tweets.csv", comment.char="#")

brazil_tweet <- iconv(brazil_tweets$text, to = 'utf-8-mac')
chile_tweet <- iconv(chile_tweets$text, to = 'utf-8-mac')
ecuador_tweet <- iconv(ecuador_tweets$text, to = 'utf-8-mac')

sentiment_brazil <- get_nrc_sentiment(brazil_tweet)
sentiment_chile <- get_nrc_sentiment(chile_tweet)
sentiment_ecuador <- get_nrc_sentiment(ecuador_tweet)

barplot(colSums(sentiment_brazil),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Brazil during COVID-19 ')
barplot(colSums(sentiment_chile),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Chile during COVID-19 ')
barplot(colSums(sentiment_ecuador),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Ecuador during COVID-19 ')

samerica_score <- vector('list',10)
samerica_score[[1]] <- sum(sentiment_brazil$anger) + sum(sentiment_chile$anger) + sum(sentiment_ecuador$anger)
samerica_score[[2]] <- sum(sentiment_brazil$anticipation) + sum(sentiment_chile$anticipation) + sum(sentiment_ecuador$anticipation)
samerica_score[[3]] <- sum(sentiment_brazil$disgust) + sum(sentiment_chile$disgust) + sum(sentiment_ecuador$disgust)
samerica_score[[4]] <- sum(sentiment_brazil$fear) + sum(sentiment_chile$fear) + sum(sentiment_ecuador$fear)
samerica_score[[5]] <- sum(sentiment_brazil$joy) + sum(sentiment_chile$joy) + sum(sentiment_ecuador$joy)
samerica_score[[6]] <- sum(sentiment_brazil$sadness) + sum(sentiment_chile$sadness) + sum(sentiment_ecuador$sadness)
samerica_score[[7]] <- sum(sentiment_brazil$surprise) + sum(sentiment_chile$surprise) + sum(sentiment_ecuador$surprise)
samerica_score[[8]] <- sum(sentiment_brazil$trust) + sum(sentiment_chile$trust) + sum(sentiment_ecuador$trust)
samerica_score[[9]] <- sum(sentiment_brazil$negative) + sum(sentiment_chile$negative) + sum(sentiment_ecuador$negative)
samerica_score[[10]] <- sum(sentiment_brazil$positive) + sum(sentiment_chile$positive) + sum(sentiment_ecuador$positive)

for (i in 1:10) {
  print(names[[i]])
  print(samerica_score[[i]])
}

barplot(unlist(samerica_score),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of South Americas worst hit countries')

#Asia
tweets_iran <- searchTwitter('coronavirus+iran',n=1000, lang = 'en')
tweets_china <- searchTwitter('coronavirus+china',n=1000, lang = 'en')
tweets_korea <- searchTwitter('coronavirus+southkorea',n=1000, lang = 'en')
tweets_india <- searchTwitter('coronavirus+india',n=1000, lang = 'en')

tweetsdf <- twListToDF(tweets_iran)
write.csv(tweetsdf, file = '~/Desktop/iran_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_china)
write.csv(tweetsdf, file = '~/Desktop/china_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_korea)
write.csv(tweetsdf, file = '~/Desktop/skorea_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_india)
write.csv(tweetsdf, file = '~/Desktop/india_tweets.csv',row.names = F) #import

iran_tweets <- read.csv("~/Desktop/iran_tweets.csv", comment.char="#")
china_tweets <- read.csv("~/Desktop/china_tweets.csv", comment.char="#")
korea_tweets <- read.csv("~/Desktop/skorea_tweets.csv", comment.char="#")
india_tweets <- read.csv("~/Desktop/india_tweets.csv", comment.char="#")

iran_tweet <- iconv(iran_tweets$text, to = 'utf-8-mac')
china_tweet <- iconv(china_tweets$text, to = 'utf-8-mac')
korea_tweet <- iconv(korea_tweets$text, to = 'utf-8-mac')
india_tweet <- iconv(india_tweets$text, to = 'utf-8-mac')

sentiment_iran <- get_nrc_sentiment(iran_tweet)
sentiment_china <- get_nrc_sentiment(china_tweet)
sentiment_korea <- get_nrc_sentiment(korea_tweet)
sentiment_india <- get_nrc_sentiment(india_tweet)

barplot(colSums(sentiment_iran),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Iran during COVID-19 ')
barplot(colSums(sentiment_china),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of China during COVID-19 ')
barplot(colSums(sentiment_korea),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Korea during COVID-19 ')
barplot(colSums(sentiment_india),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of India during COVID-19 ')

asia_score <- vector('list',10)
asia_score[[1]] <- sum(sentiment_china$anger) + sum(sentiment_india$anger) + sum(sentiment_korea$anger) + sum(sentiment_iran$anger)
asia_score[[2]] <- sum(sentiment_china$anticipation) + sum(sentiment_india$anticipation) + sum(sentiment_korea$anticipation) + sum(sentiment_iran$anticipation)
asia_score[[3]] <- sum(sentiment_china$disgust) + sum(sentiment_india$disgust) + sum(sentiment_korea$disgust) + sum(sentiment_iran$disgust)
asia_score[[4]] <- sum(sentiment_china$fear) + sum(sentiment_india$fear) + sum(sentiment_korea$fear) + sum(sentiment_iran$fear)
asia_score[[5]] <- sum(sentiment_china$joy) + sum(sentiment_india$joy) + sum(sentiment_korea$joy) + sum(sentiment_iran$joy)
asia_score[[6]] <- sum(sentiment_china$sadness) + sum(sentiment_india$sadness) + sum(sentiment_korea$sadness) + sum(sentiment_iran$sadness)
asia_score[[7]] <- sum(sentiment_china$surprise) + sum(sentiment_india$surprise) + sum(sentiment_korea$surprise) + sum(sentiment_iran$surprise)
asia_score[[8]] <- sum(sentiment_china$trust) + sum(sentiment_india$trust) + sum(sentiment_korea$trust) + sum(sentiment_iran$trust)
asia_score[[9]] <- sum(sentiment_china$negative) + sum(sentiment_india$negative) + sum(sentiment_korea$negative) + sum(sentiment_iran$negative)
asia_score[[10]] <- sum(sentiment_china$positive) + sum(sentiment_india$positive) + sum(sentiment_korea$positive) + sum(sentiment_iran$positive)

for (i in 1:10) {
  print(names[[i]])
  print(asia_score[[i]])
}

barplot(unlist(asia_score),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of Asias worst hit countries')

#Australia
tweets_aus <- searchTwitter('coronavirus+australia',n=1000, lang = 'en')
tweets_nz <- searchTwitter('coronavirus+newzealand',n=1000, lang = 'en')

tweetsdf <- twListToDF(tweets_aus)
write.csv(tweetsdf, file = '~/Desktop/aus_tweets.csv',row.names = F) #import
tweetsdf <- twListToDF(tweets_nz)
write.csv(tweetsdf, file = '~/Desktop/nz_tweets.csv',row.names = F) #import

aus_tweets <- read.csv("~/Desktop/aus_tweets.csv", comment.char="#")
nz_tweets <- read.csv("~/Desktop/nz_tweets.csv", comment.char="#")

aus_tweet <- iconv(aus_tweets$text, to = 'utf-8-mac')
nz_tweet <- iconv(nz_tweets$text, to = 'utf-8-mac')

sentiment_aus <- get_nrc_sentiment(aus_tweet)
sentiment_nz <- get_nrc_sentiment(nz_tweet)

barplot(colSums(sentiment_aus),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of AUS during COVID-19 ')
barplot(colSums(sentiment_nz),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of NZ during COVID-19 ')

aus_score <- vector('list',10)
aus_score[[1]] <- sum(sentiment_aus$anger) + sum(sentiment_nz$anger)
aus_score[[2]] <- sum(sentiment_aus$anticipation) + sum(sentiment_nz$anticipation)
aus_score[[3]] <- sum(sentiment_aus$disgust) + sum(sentiment_nz$disgust)
aus_score[[4]] <- sum(sentiment_aus$fear) + sum(sentiment_nz$fear)
aus_score[[5]] <- sum(sentiment_aus$joy) + sum(sentiment_nz$joy)
aus_score[[6]] <- sum(sentiment_aus$sadness) + sum(sentiment_nz$sadness)
aus_score[[7]] <- sum(sentiment_aus$surprise) + sum(sentiment_nz$surprise)
aus_score[[8]] <- sum(sentiment_aus$trust) + sum(sentiment_nz$trust)
aus_score[[9]] <- sum(sentiment_aus$negative) + sum(sentiment_nz$negative)
aus_score[[10]] <- sum(sentiment_aus$positive) + sum(sentiment_nz$positive)

for (i in 1:10) {
  print(names[[i]])
  print(aus_score[[i]])
}

barplot(unlist(aus_score),las=2,col = rainbow(10),ylab = 'count',main = 'Sentiment of AUS/NZ worst hit countries')

######
library(dplyr)
world_score <- data.frame(nrow(5),ncol(10))
for(i in 1:10){
  world_score[1,i]<-europe_score[[i]]
  world_score[2,i]<-namerica_score[[i]]
  world_score[3,i]<-samerica_score[[i]]
  world_score[4,i]<-asia_score[[i]]
  world_score[5,i]<-aus_score[[i]]
}
colnames(world_score)<-c('anger','anticipation','disgust','fear','joy','sadness','surprise','trust','negative','positive')
rownames(world_score)<-c('EUROPE','NORTH AMERICA','SOUTH AMERICA','ASIA','AUS')
world_score
class(world_score)
plot(world_score)
hist(unlist(world_score))
barplot(unlist(world_score),col = rainbow(10))
world_score_sum<-data.frame(nrow(1),ncol(10))
for(i in 1:10){
  world_score_sum[1,i]<-sum(b[,i])
}
colnames(world_score_sum)<-c('anger','anticipation','disgust','fear','joy','sadness','surprise','trust','negative','positive')
sort(unlist(world_score_sum),decreasing = TRUE)
plot(world_score_sum)
barplot(unlist(world_score_sum),col = rainbow(10))
boxplot(world_score_sum)

score_mean <-data.frame(nrow(1),ncol(10))
for(i in 1:10){
  score_mean[1,i]<-mean(world_score[[i]])
}
colnames(score_mean)<-c('anger','anticipation','disgust','fear','joy','sadness','surprise','trust','negative','positive')
plot(score_mean)
barplot(unlist(score_mean),col = rainbow(10))
boxplot(score_mean)
library(ggplot2)
ggplot(world_score,aes(x=positive,y=fear))+
  geom_line()+stat_smooth(method = lm)
ggplot(world_score,aes(x=positive,y=sadness))+
  geom_line()+stat_smooth(method = lm)
ggplot(world_score,aes(x=positive,y=trust))+
  geom_line()+stat_smooth(method = lm)
ggplot(world_score,aes(x=positive,y=joy))+
  geom_line()+stat_smooth(method = lm)
ggplot(world_score,aes(x=positive,y=anger))+
  geom_line()+stat_smooth(method = lm)

