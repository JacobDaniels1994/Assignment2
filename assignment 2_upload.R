library(twitteR)
library(wordcloud)
library(tm)
library(rtweet)
library(SnowballC)
library(stringr)
library(qdap)

#Getting the keys
CK <-'XXXXXXXXXXX'
CS <-'XXXXXXXXXXX'
AT <-'XXXXXXXXXXX'
AS <-'XXXXXXXXXXX'

#-----------------------------
#setting up the authentication connection and the working directories.
#-----------------------------
auth<- setup_twitter_oauth(CK,CS,AT,AS)

#Getwd will tell you where files that you write to csv will save
#setwd will let you set your working directory, so pick where on your computer you want to save your files to.
getwd()
setwd(dir = "D:/Gradschool/PK Class/Assignment2")

#-----------------------------
#Search Criterion
#-----------------------------
#i wanted to avoid pulling retweets because it would 
tweetsearch <- searchTwitter("#420day exclude:retweets", n=3000)
#turning tweetsearch variable to list
dftweet = twitteR::twListToDF(tweetsearch)

dftweet

#Saving this to csv so i can run from here and work on it later without having to use twitter API again
write.csv(dftweet,file = "420day_tweets2.csv", row.names = FALSE)
#dftweet=read_csv

###### READ CSV AT THIS POINT
dftweet=read.csv("420day_tweets2.csv")
#viewing the data frame
dftweet
#cleaning the tweets before turning into corpus
dftweet$text <- iconv(dftweet$text, from = "UTF-8", to = "ASCII", sub = "")
dftweet$text <- removePunctuation(dftweet$text)
#viewing dftweet$text
dftweet$text

#changing the df into a corpus
review_corpus <- VCorpus(VectorSource(dftweet$text))
print(review_corpus)


#Cleaning corpus - pre_processing and cleaning
clean_corpus <- function(cleaned_corpus){
  removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))
  cleaned_corpus <- tm_map(cleaned_corpus, removeURL)
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(replace_abbreviation))
  cleaned_corpus <- tm_map(cleaned_corpus, content_transformer(tolower))
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  custom_stop_words <- c("420day","day")
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  # cleaned_corpus <- tm_map(cleaned_corpus, stemDocument,language = "english")
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
  }


#running the function
clean_tweet_corpus <- clean_corpus(review_corpus)
#comparing cleaned and not cleaned
print(review_corpus[[19]][1])
print(clean_tweet_corpus[[19]][1])

########### TDM/DTM########
#creating the TDM
TDM_tweets <- TermDocumentMatrix(clean_tweet_corpus)
TDM_tweets_m <- as.matrix(TDM_tweets)

# Term Frequency
term_frequency <- rowSums(TDM_tweets_m)
term_frequency

# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)
term_frequency

#######UNIGRAM CODE##############
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
unigramWC <- wordcloud(word_freqs$term, word_freqs$num,min.freq=2,scale=c(3,0.5),max.words=150,colors=brewer.pal(8, "Dark2"))
unigramWC

######BiGRAM CODE###############
library(RWeka)
tokenizer <- function(x)NGramTokenizer(x,Weka_control(min=2,max=2))

bigram_tdm <- TermDocumentMatrix(clean_tweet_corpus,control = list(tokenize=tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)

# Term Frequency for bigram
term_frequency <- rowSums(bigram_tdm_m)

# Sort term_frequency in descending order for bigram
term_frequency <- sort(term_frequency,dec=TRUE)

############Word Cloud for bigram#############
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
bigramWC <- wordcloud(word_freqs$term, word_freqs$num,min.freq=2,scale=c(3,.5),max.words=150,colors=brewer.pal(8, "Dark2"))

####### Trigram code############

tokenizer <- function(x)NGramTokenizer(x,Weka_control(min=3,max=3))

trigram_tdm <- TermDocumentMatrix(clean_tweet_corpus, control = list(tokenize=tokenizer))
trigram_tdm_m <- as.matrix(trigram_tdm)

# Term Frequency for trigram
term_frequency <- rowSums(trigram_tdm_m)
# Sort term_frequency in descending order for trigram
term_frequency <- sort(term_frequency,dec=TRUE)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,scale=c(3,.5),max.words=150,colors=brewer.pal(8, "Dark2"))

####TF-IDF#####

tfidf_tdm <- TermDocumentMatrix(clean_tweet_corpus,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)

# Term Frequency tfidf
term_frequency <- rowSums(tfidf_tdm_m)

# Sort term_frequency in descending order tfidf
term_frequency <- sort(term_frequency,dec=TRUE)

# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)

# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=6,scale=c(5,.5),max.words=150,colors=brewer.pal(8, "Dark2"))

########SENTIMENT ANALYSIS
#rerunning librarys needed for this section
library(tm)
library(qdap)
library(tibble)
library(ggplot2)
library(RWeka)
library(wordcloud)
library(lubridate)
library(lexicon)
library(tidytext)
library(lubridate)
library(gutenbergr)
library(stringr)
library(dplyr)
library(radarchart)
library(tidyr)

#reading csv from this part of the hw
setwd(dir = "D:/Gradschool/PK Class/Assignment2")
dftweet <- read.csv("420day_tweets2.csv")

dftweet$text <- iconv(dftweet$text, from = "UTF-8", to = "ASCII", sub = "")
dftweet$text <- removePunctuation(dftweet$text)
#getting the polarity
(sentiment_text<- polarity(dftweet$text))

#adding the polarity to the DF
dftweet$sent <- sentiment_text$all$polarity

text_counts <- counts(sentiment_text)
(n_good <- length(text_counts$pos.words[[1]]))
(n_words <- text_counts$wc)
n_good / sqrt(n_words)


#creating the tidy corpus

clean_tweet_corpus <- clean_corpus(review_corpus)

TDM_mytext <- TermDocumentMatrix(clean_tweet_corpus)

mytext_tidy <- tidy(TDM_mytext)

#using the bing lexicon
bing_lex <- get_sentiments("bing")

#Joining sentiments (lesson 3.r)
tweets_bing_lex <- inner_join(mytext_tidy, bing_lex, by = c("term" = "word"))

tweets_bing_lex$sentiment_n <- ifelse(tweets_bing_lex$sentiment=="negative", -1, 1)

tweets_bing_lex$sentiment_value <- tweets_bing_lex$sentiment_n * tweets_bing_lex$count

#aggregating the sentiments
tweets_aggdata <- aggregate(tweets_bing_lex$sentiment_value, list(index = tweets_bing_lex$document), sum)
sapply(tweets_aggdata,typeof)

#prepping the tweets for the graphing
tweets_aggdata$index <- as.numeric(tweets_aggdata$index)
colnames(tweets_aggdata) <- c("index","bing_score")

tidy_sentiment_data <- gather(tweets_aggdata, sentiment_dict, sentiment_score, -index)
tidy_sentiment_data[is.na(tidy_sentiment_data)] <- 0


### Plotting the sentiments
ggplot(data = tidy_sentiment_data,
       aes(x=index,y=sentiment_score,fill=sentiment_dict))+
  geom_bar(stat="identity") + facet_grid(sentiment_dict~.)+theme_bw() + 
  theme(legend.position = "none")+ggtitle("Tweet sentiments")

#####Comminality and comparison clouds
dftweet <- read.csv("420day_tweets2.csv")


dftweet$text <- iconv(dftweet$text, from = "UTF-8", to = "ASCII", sub = "")
dftweet$text <- removePunctuation(dftweet$text)

(sentiment_text<- polarity(dftweet$text))

#adding the polarity to the DF
dftweet$sent <- sentiment_text$all$polarity

text_counts <- counts(sentiment_text)

#tweet_text = sapply(dftweet, function(x) x$getText())
positive_tweets <- dftweet[dftweet[ , 17] >0.0, ]
negative_tweets <- dftweet[dftweet[ , 17] <0.0, ]

positive_tweets$text

#puts the positive and negitive text into one long string
#the text needs to be turned into one long string for positive and one long string
#for negitive or else you will get errors when running the cloud
stringpos<- paste(unlist(positive_tweets$text), collapse =" ")
stringneg<- paste(unlist(negative_tweets$text), collapse =" ")

posnegstring <- c(stringpos,stringneg)

#writing this to a corpus and cleaning it

tweet_corpus <- VCorpus(VectorSource(posnegstring))
tweet_corpus <- clean_corpus(tweet_corpus)

#creating the TDM and then turning it to a matrix
TDM_tweet <- TermDocumentMatrix(tweet_corpus)
TDM_tweet_matrix <- as.matrix(TDM_tweet)

#creating commonality cloud
commonality.cloud(TDM_tweet_matrix, scale=c(5,1), max.words = 300,colors=brewer.pal(8, "Dark2"))



#Doing the comparison cloud
#lesson1.r
#need column names
TDM_tweet <- TermDocumentMatrix(tweet_corpus)
colnames(TDM_tweet) <- c("good","bad")
TDM_tweet_matrix <- as.matrix(TDM_tweet)
#making the cloud
comparison.cloud(TDM_tweet_matrix,colors=brewer.pal(8, "Dark2"),max.words = 200)

#Radar chart time
library(radarchart)

#using nrc lexicon for emotions
#####Comminality and comparison clouds
#not needed to reread csv for every part but for me its useful so i can stop and
#pick up the assignment from where i left off rather than running everything again
dftweet <- read.csv("420day_tweets2.csv")

dftweet$text <- iconv(dftweet$text, from = "UTF-8", to = "ASCII", sub = "")

#simpler function for cleaning
clean_corpus <- function(cleaned_corpus){
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

#make the corpus
tweet_corpus <- VCorpus(VectorSource(dftweet$text))
cleaned_tweet_corpus <- clean_corpus(tweet_corpus)

#Using tidy and nrc lexicon
tidy_mytext <- tidy(TermDocumentMatrix(cleaned_tweet_corpus))
nrc_lex <- get_sentiments("nrc")

#joining
mytext_nrc <- inner_join(tidy_mytext, nrc_lex, by = c("term" = "word"))
mytext_nrc_noposneg <- mytext_nrc[!(mytext_nrc$sentiment %in% c("positive","negative")),]

#aggregating the data
aggdata <- aggregate(mytext_nrc_noposneg$count, list(index = mytext_nrc_noposneg$sentiment), sum)

#making the radar chart
chartJSRadar(aggdata)

dftweet
