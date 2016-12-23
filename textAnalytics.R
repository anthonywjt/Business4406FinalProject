#This is a check to see make sure all packages are installed
list.of.packages <- c("twitteR", "ROAuth","ggplot2","ggmap","RColorBrewer","stringr","wordcloud","tm","plyr","dplyr","tm","SnowballC","wordcloud")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
print("All Required Packages Installed")


library(twitteR)
library(ROAuth)
require(RCurl)
library(stringr)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(ggmap)
library(ggplot2)
library(plyr)
library(tm)

closeAllConnections()
rm(list=ls())

requestURL='https://api.twitter.com/oauth/request_token'
accessURL='https://api.twitter.com/oauth/access_token'
authURL='https://api.twitter.com/oauth/authorize'
apiKey='HIDDEN'
apiSecret='HIDDEN'
access_token = 'HIDDEN'
access_token_secret = 'HIDDEN'

setup_twitter_oauth(apiKey,apiSecret,access_token,
                    access_token_secret)

print("-----Authentication Finished-----")
print("-----Now Extracting Tweets-----")

# Grab latest tweets
tweets<- searchTwitter("#election2016 OR #Election2016 OR #Trump OR #Donald OR #Republican", n=2000, lang="en")
tweets.df = ldply(tweets, function(t) t$toDataFrame())
#write whatever was pulled into a Spreadsheet
write.csv(tweets.df[,c("text")], file = "/Users/Joe Carrigan/Documents/BUSI4406/TAKE HOME EXAM/TRUMPtweets.csv")

clintonTweets<- searchTwitter("#election2016 OR #Election2016 OR #Clinton OR #Hillary OR #Democratic", n=2000, lang="en")
tweets.df = ldply(tweets, function(t) t$toDataFrame())

#write whatever was pulled into a Spreadsheet
write.csv(tweets.df[,c("text")], file = "/Users/Joe Carrigan/Documents/BUSI4406/TAKE HOME EXAM/CLINTONtweets.csv")

# Loop over tweets and extract text
print("Extracting Donald Trump Tweets")
feed_tweets <- laply(tweets, function(t) t$getText(), .progress='text')

print("Extracting Hillary Clinton Tweets")
feed_tweetsClinton<- laply(clintonTweets, function(t) t$getText(), .progress='text')

print("Tweets Extraction Finished")

#scan wordbank for positive and negative words
pos = scan('/Users/Joe Carrigan/Documents/BUSI4406/TAKE HOME EXAM/wordbank/positive-words.txt', what='character', comment.char=';')
neg = scan('/Users/Joe Carrigan/Documents/BUSI4406/TAKE HOME EXAM/wordbank/negative-words.txt', what='character', comment.char=';')



#This is where our sentiment Analysis begins
#Function created by a great community of developers
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we want a simple array of scores back
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}



###########################################################################################################
# Seperate Bar Graphs based on Sentiment Scores between the two Candidates #

clintonAnalysis = score.sentiment(feed_tweetsClinton, pos, neg)
write.csv(clintonAnalysis, "/Users/Joe Carrigan/Documents/BUSI4406/TAKE HOME EXAM/CLINTONAnalysis.csv")
c<-qplot(clintonAnalysis$score)
print(c)
View(clintonAnalysis)

trumpAnalysis = score.sentiment(feed_tweets, pos, neg)
write.csv(trumpAnalysis, "/Users/Joe Carrigan/Documents/BUSI4406/TAKE HOME EXAM/TRUMPAnalysis.csv")
t<-qplot(trumpAnalysis$score)
print(t)
View(trumpAnalysis)

###########################################################################################################
# CREATING A BOXPLOT FOR CANDIDATES BASED ON SCORES #

tweet = rbind(feed_tweets,feed_tweetsClinton)
#tweet = score.sentiment(feed_tweetsClinton, pos, neg, .progress='text')
scores = score.sentiment(tweet, pos, neg, .progress='text')
# add variables to data frame
scores$candidate = factor(rep(c("Donald Trump", "Hilliary Clinton")))
scores$pos = as.numeric(scores$score > 0)
scores$neg = as.numeric(scores$score < 0)
scores$neu = as.numeric(scores$score == 0)

# how many very positives and very negatives
numpos = sum(scores$pos)
numneg = sum(scores$neg)
numneu = sum(scores$neu)

# colors
cols = c("#ff0000", "#0000ff")
names(cols) = c("Donald Trump", "Hillary Clinton")
# boxplot
boxplot<-ggplot(scores, aes(x=candidate, y=score, group=candidate)) +
  geom_boxplot(aes(fill=candidate)) +
  scale_fill_manual(values=cols) +
  geom_jitter(colour="gray40",
              position=position_jitter(width=0.2), alpha=0.3) +
  ggtitle("Boxplot - Presidental Candidate Sentiment Scores")
print(boxplot)
###########################################################################################################
