#----------------------
# Packages installation
#----------------------

# install.packages("twitteR") # provides access to the Twitter API 
# install.packages("rtweet") # to extract data from Twitter 
library(twitteR) 
library(rtweet) 

# install.packages("ggplot2") # for plotting 
library(ggplot2) 

# install.packages("dplyr") # for data manipulation 
library(dplyr) 

# install.packages("tidyr") # for storing data 
library(tidyr) 

# install.packages("tidytext") # for text mining 
library(tidytext) 

# install.packages("maps") # creating graphs and maps 
library(maps) 
 
# install.packages("ggmap") # spatial graphic 
library(ggmap) 

# install.packages(c('ROAuth','RCurl')) # authentication 
library(ROAuth) 
library(RCurl) 

# install.packages("syuzhet") # sentiment analysis package 
library(syuzhet) 

# install.packages("tm") # a text mining package 
# install.packages("SnowballC") # text stemming (words are reduced to their root forms) 
# install.packages("wordcloud") # word cloud generator 
# install.packages("RColorBrewer") # color palettes 
library(tm) 
library(SnowballC) 
library(wordcloud) 
library(RColorBrewer) 


#------------
# Twitter API
#------------

my.key<-"080X2ef1ZQ97dVFkLuoqodnZW"
my.secret<-"OZgfcXDAj6lCnSx7CwHykm7ISZdu0T4pjkEeOAkYwBaxzEAzuR"
aces_token<-"1075704418346123267-LQIMGKenrF6IsfdK07mTaapAX8QFfP"
access_secret<-"TmyXYyWW3nOqsDM4NsCreaHjqhONSuX0LvUn2hWxSKwPs"

twitter_token <- create_token(
  consumer_key = my.key,
  consumer_secret = my.secret,
  access_token = aces_token,
  access_secret = access_secret)

setup_twitter_oauth(my.key,my.secret,aces_token,access_secret)

#-----------------
# Data acquisition
#-----------------

# From twitter search with keywords and taking 10000 tweets

tweets <- search_tweets("PlayStation OR Playstation OR playstation", n=10000,lang = "fr")
df = do.call("rbind", lapply(tweets, as.data.frame))
write.csv(df, "PlayStation10000Tweets.csv", row.names=FALSE)

# From twitter account @playstationfr

PlayStationTwitter <- get_timeline("playstationfr", n= 3200)
df2 = do.call("rbind", lapply(PlayStationTwitter, as.data.frame))
write.csv(df2, "PlayStationFR3200Tweets.csv", row.names=FALSE)

# Converting tweets into vector in order to process effectively the data

PlayStationSearchText <- as.vector(tweets$text)
PlayStationAccountText <- as.vector(PlayStationTwitter$text)

head(PlayStationSearchText,5)
head(PlayStationAccountText,5)

#--------------------------------------
# 10,000 searched tweets of PlayStation
#--------------------------------------

# Using gsub to remove various unwanted terms

cleanTweet <- gsub("rt|RT", "", PlayStationSearchText) # remove Retweet
cleanTweet <- gsub("http\\w+", "", cleanTweet)  # remove links http
cleanTweet <- gsub("<.*?>", "", cleanTweet) # remove html tags
cleanTweet <- gsub("@\\w+", "", cleanTweet) # remove at(@)
cleanTweet <- gsub("[[:punct:]]", "", cleanTweet) # remove punctuation
cleanTweet <- gsub("\r?\n|\r", " ", cleanTweet) # remove /n
cleanTweet <- gsub("[[:digit:]]", "", cleanTweet) # remove numbers/Digits
cleanTweet <- gsub("㠼|㸵|㤼|㸲|㸱|㸳|㸴|㸶|攼|㹤", "", cleanTweet) #  asian letters
cleanTweet <- gsub("[ |\t]{2,}", "", cleanTweet) # remove tabs
cleanTweet <- gsub("^ ", "", cleanTweet)  # remove blank spaces at the beginning
cleanTweet <- gsub(" $", "", cleanTweet) # remove blank spaces at the end 

head(cleanTweet,5)

# Applying the sentiment algorithm and merge the results into original data

PlayStationSearchSentiment <- get_nrc_sentiment(cleanTweet, cl = NULL, language = "french", lowercase = TRUE)
head(PlayStationSearchSentiment,5)

PlayStationSearchFinalData <- cbind(tweets, PlayStationSearchSentiment)
head(PlayStationSearchFinalData,1)

View(PlayStationSearchFinalData)
ncol(PlayStationSearchFinalData)

# Plotting the data
# Using dplyr and tidyr to transform and plot the data using ggplot2

# Emotion for search term PlayStation

plotData1 <- gather(PlayStationSearchFinalData,"sentiment","values",91:98)  %>% 
  group_by( sentiment) %>%
  summarise(Total = sum(values))

ggplot(data = plotData1, aes(x = plotData1$sentiment, y = plotData1$Total)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Emotions") + ylab("Total") + ggtitle("Emotion for Search Term PlayStation")+
  geom_text(aes(label =   plotData1$Total), position = position_dodge(width=0.75), vjust = -0.25)

# Sentiment for search PlayStation

plotData2 =gather(PlayStationSearchFinalData,"Polarity","values",99:100)  %>% 
  group_by( Polarity) %>%
  summarise(Total = sum(values))

ggplot(data = plotData2, aes(x = plotData2$Polarity, y = plotData2$Total)) +
  geom_bar(aes(fill = plotData2$Polarity), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total") + ggtitle("Sentiment for Search PlayStation")+
  geom_text(aes(label =   plotData2$Total), position = position_dodge(width=0.75), vjust = -0.25)


#---------------------------------
# Personal account @playstationfr
#---------------------------------

head(PlayStationAccountText,5)

cleanTweetp <- gsub("rt|RT", "", PlayStationAccountText) # remove Retweet
cleanTweetp <- gsub("http\\w+", "", cleanTweetp)  # remove links http
cleanTweetp <- gsub("<.*?>", "", cleanTweetp) # remove html tags
cleanTweetp <- gsub("@\\w+", "", cleanTweetp) # remove at(@)
cleanTweetp <- gsub("[[:punct:]]", "", cleanTweetp) # remove punctuation
cleanTweetp <- gsub("\r?\n|\r", " ", cleanTweetp) # remove /n
cleanTweetp <- gsub("[[:digit:]]", "", cleanTweetp) # remove numbers/Digits
cleanTweetp <- gsub("???|???|???|???|???|???|???|???|???|???", "", cleanTweetp) #  asian letters
cleanTweetp <- gsub("[ |\t]{2,}", "", cleanTweetp) # remove tabs
cleanTweetp <- gsub("^ ", "", cleanTweetp)  # remove blank spaces at the beginning
cleanTweetp <- gsub(" $", "", cleanTweetp) # remove blank spaces at the end 

PlayStationAccountSentiment <- get_nrc_sentiment(cleanTweetp, cl = NULL, language = "french", lowercase = TRUE)
head(PlayStationAccountSentiment,5)

PlayStationFinalData <- cbind(PlayStationTwitter, PlayStationAccountSentiment)
head(PlayStationFinalData,5)

# Plot of Emotions for @playstationfr

plotData3 =gather(PlayStationFinalData,"sentiment","values",91:98)  %>% 
  group_by( sentiment) %>%
  summarise(Total = sum(values))

ggplot(data = plotData3, aes(x = plotData3$sentiment, y = plotData3$Total)) +
  geom_bar(aes(fill = sentiment), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total") + ggtitle("Emotions for @playstationfr")+
  geom_text(aes(label =   plotData3$Total), position = position_dodge(width=0.75), vjust = -0.25)

# Plot of Sentiment for @playstationfr

plotData4 =gather(PlayStationFinalData,"Polarity","values",99:100)  %>% 
  group_by( Polarity) %>%
  summarise(Total = sum(values))

ggplot(data = plotData4, aes(x = plotData4$Polarity, y = plotData4$Total)) +
  geom_bar(aes(fill = plotData4$Polarity), stat = "identity") +
  theme(legend.position = "none") +
  xlab("Sentiment") + ylab("Total") + ggtitle("Sentiment for @playstationfr")+
  geom_text(aes(label =   plotData4$Total), position = position_dodge(width=0.75), vjust = -0.25)

# Emotions of @playstationfr

plotData5 = select(PlayStationFinalData,created_at,91:98)
plotData5 = separate(plotData5,created_at,c("date","Time")," ") %>%
  group_by(date)%>%
  summarise(Anger=sum(anger), Anticipation=sum(anticipation), Disgust=sum(disgust), Fear=sum(fear), Joy=sum(joy), Sadness=sum(sadness), Surprise=sum(surprise), Trust=sum(trust))

plotData5$date = as.Date(plotData5$date,"%Y-%m-%d") 

plotData5$date <- as.Date(cut(plotData5$date, breaks = "month"))

plotData5 = gather(plotData5,"sentiment","values",2:9)%>%
  group_by(date,sentiment)%>%
  summarise(Total=sum(values))

ggplot(data = plotData5, aes(x = plotData5$date, y = plotData5$Total, group = plotData5$sentiment)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = sentiment,stat = "identity")) +
  geom_point(size = 0.5) +
  #ylim(0, 0.6) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Total") + 
  ggtitle("Emotions of @playstationfr 2020-2022")+
  scale_y_continuous(limits=c(0,75)) 

# Sentiments of @playstationfr 

plotData6 =gather(PlayStationFinalData,"Polarity","values",99:100)  %>% 
  group_by( created_at,Polarity) %>%
  summarise(Total = sum(values))
plotData6 = separate(plotData6,created_at,c("date","Time")," ")
plotData6$date = as.Date(plotData6$date,"%Y-%m-%d") 
plotData6$date <- as.Date(cut(plotData6$date, breaks = "month"))

plotData6 = select(plotData6,date,Polarity,Total)%>%
  group_by(date,Polarity)%>%
  summarise(Total = sum(Total))

ggplot(data = plotData6, aes(x = plotData6$date, y = plotData6$Total, group = plotData6$Polarity)) +
  geom_line(size = 2.5, alpha = 0.7, aes(color = plotData6$Polarity,stat = "identity")) +
  geom_point(size = 0.5) +
  #ylim(0, 0.6) +
  theme(legend.title=element_blank(), axis.title.x = element_blank()) +
  ylab("Total") + 
  ggtitle("Sentiment of @PlayStation 2020-2022")+
  scale_y_continuous(limits=c(0,200)) 

# WordCloud of @playstationfr

vector = tweets$text
docs <- Corpus(VectorSource(vector))

inspect(docs)

toSpace <- content_transformer(function (x, pattern ) gsub(pattern, " ", x))

docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removeWords, stopwords("french"))
docs <- tm_map(docs, removeWords, c("https")) 
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
m[1:30,1:20]

# Most frequent terms in the corpus

v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 30)

# Wordcloud

wordcloud(words = d$word, freq = d$freq, min.freq = 5, max.words=200, random.order=FALSE, colors=brewer.pal(8, "Dark2"))
