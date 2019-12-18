library(twitteR)
library(RCurl)
library(httr)
library(tm)
library(wordcloud)
library(syuzhet)

#twitter api keys
api_key <- "aAtA9Tnaj3dTiyzeYHiI0nH1l"
api_secret <- "ky9N9w3Vvz9z1jN4Qgak7LRJggIm1s16PxcS2PqcuMEMvtygch"
#access tokens
access_token <- "1206855925727977472-wZSPcHyr4ccs6yLnqADMxjWR7z1DJ9"
access_token_secret <- "M6QCm4WBPnvZqxi3jP18yitojuQGMPoXu6vGARtRMFtp1"

#authorize api
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

#grab tweets with the specified text
tweets = searchTwitter('Impeachment', n = 3500)

#create data frame
tweets.df <- twListToDF(tweets)
#delete all columns except for the first one
tweets.df = tweets.df[,1]
#delete ampersands
tweets.df = gsub("&amp", "", tweets.df)
tweets.df = gsub("&amp", "", tweets.df)
#remove RT and via in the case of retweets
tweets.df = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", tweets.df)
#remove @ signs
tweets.df = gsub("@\\w+", "", tweets.df)
#remove puncuation
tweets.df = gsub("[[:punct:]]", "", tweets.df)
#remove numbers
tweets.df = gsub("[[:digit:]]", "", tweets.df)
#remove links
tweets.df = gsub("http\\w+", "", tweets.df)
tweets.df = gsub("[ \t]{2,}", "", tweets.df)
tweets.df = gsub("^\\s+|\\s+$", "", tweets.df)
tweets.df = gsub("\\Trump\\b", "",tweets.df)

sentiment <- get_nrc_sentiment(tweets.df)
sentBar = colSums(sentiment)
sentSum = data.frame(count=sentBar, sentiment=names(sentBar))
sentSum$sentiment = factor(sentSum$sentiment, levels=sentSum$sentiment[order(sentSum$count, decreasing = TRUE)])

# Visualize the sentiments from NRC sentiments
library(plotly)
p <- plot_ly(sentSum, x=~sentiment, y=~count, type="bar", color=~sentiment) %>%
  layout(xaxis=list(title=""), showlegend=FALSE,
         title="Sentiment on Impeachment Eve")
p

# Create comparison word cloud data

wordcloud_tweet = c(
  paste(tweets.df[sentiment$anger > 0], collapse=" "),
  paste(tweets.df[sentiment$anticipation > 0], collapse=" "),
  paste(tweets.df[sentiment$disgust > 0], collapse=" "),
  paste(tweets.df[sentiment$fear > 0], collapse=" "),
  paste(tweets.df[sentiment$joy > 0], collapse=" "),
  paste(tweets.df[sentiment$sadness > 0], collapse=" "),
  paste(tweets.df[sentiment$surprise > 0], collapse=" "),
  paste(tweets.df[sentiment$trust > 0], collapse=" ")
)

# create corpus
tweetCorpus = Corpus(VectorSource(wordcloud_tweet))

# remove punctuation, convert every word in lower case and remove stop words

tweetCorpus = tm_map(tweetCorpus, removePunctuation)
tweetcorpus = tm_map(tweetCorpus, removeWords, c(stopwords("english")))
corpus = tm_map(tweetCorpus, stemDocument)

# create document term matrix

tdmatrix = TermDocumentMatrix(corpus)

# convert as matrix
tdmatrix = as.matrix(tdmatrix)
tdmatrixnew <- tdmatrix[nchar(rownames(tdmatrix)) < 11,]

# column name binding
colnames(tdmatrix) = c('anger', 'anticipation', 'disgust', 'fear', 'joy', 'sadness', 'surprise', 'trust')
colnames(tdmatrixnew) <- colnames(tdmatrix)
comparison.cloud(tdmatrixnew, random.order=FALSE,
                 colors = c("#00B2FF", "red", "#FF0099", "#6600CC", "green", "orange", "blue", "brown"),
                 title.size=1, max.words=250, scale=c(2.5, 0.4),rot.per=0.4)

