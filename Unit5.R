library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)
library(stringr)


tweets = read_csv('tweets.csv')

tweets = tweets %>% mutate(Negative = as.factor(Avg <= -1))

tweets %>% group_by(Negative) %>% summarise(NumofObs = n())

corpus = Corpus(VectorSource(tweets$Tweet))

#All texts are lowercase
corpus = tm_map(corpus, tolower)

#Remove punctuations
corpus = tm_map(corpus, removePunctuation)

#Remove stop words
corpus = tm_map(corpus, removeWords, c(stopwords(), 'apple'))

#Create a stem
corpus = tm_map(corpus, stemDocument)

#Create Document Term Matrix
frequencies = DocumentTermMatrix(corpus)

#Inspec the matrix above, for example from doc1 - doc 10, term 1 - 9
inspect(frequencies[1:10, 1:9])

#To find words that appears at least n = 20 times
findFreqTerms(frequencies, lowfreq = 20)

#Remove sparse terms, just only keeps the terms that >= 1 - 0.995 proportion of text data
parse = removeSparseTerms(frequencies, sparse = 0.995)

#Convert to dataframe
tweetsSparse = as.data.frame(as.matrix(parse))

#To make sure that variable name is corect
colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

#Add Negative variable from tweet to tweetSparse
tweetsSparse$Negative = tweets$Negative

#Now, start to analytic this dataset
set.seed(123)

split = sample.split(tweetsSparse$Negative, SplitRatio = 0.7)
trainSparse = tweetsSparse[split,]
testSparse = tweetsSparse[!split,]

#Create first model with rpart
tweetCART = rpart(Negative ~ ., data = trainSparse, method = 'class')
prp(tweetCART)
testSparse$PredCART = predict(tweetCART, newdata = testSparse, type = 'class')

testSparse %>% group_by(Negative) %>%
               summarise(PredictFalse = sum(Negative == PredCART),
                         PredictTrue = sum(Negative != PredCART))

#Calculate CART accuracy
mean(testSparse$Negative == testSparse$PredCART)

#Create second model with randomForest
tweetRF = randomForest(Negative ~ ., data = trainSparse)
testSparse$PredRF = predict(tweetRF, newdata = testSparse)

testSparse %>% group_by(Negative) %>%
               summarise(PredictFalse = sum(Negative == PredRF),
                         PredictTrue = sum(Negative != PredRF))
#Calculate RF accuracy
mean(testSparse$Negative == testSparse$PredRF)

#Let make the third model using logistic regression
tweetLR = glm(Negative ~ ., data = trainSparse, family = 'binomial')
testSparse$ProbLR = predict(tweetLR, newdata = testSparse, type = 'response')
testSparse$PredLR = ifelse(testSparse$ProbLR < 0.5, FALSE, TRUE)

testSparse %>% group_by(Negative) %>%
               summarise(PredictFalse = sum(Negative == PredLR),
                         PredictTrue = sum(Negative != PredLR))

#Calculate LR accuracy
mean(testSparse$Negative == testSparse$PredLR)

#Assignment 1:
wiki = read_csv('wiki.csv')
wiki = wiki[,- c(1, 2)]

corpusAdded = Corpus(VectorSource(wiki$Added))
corpusAdded = tm_map(corpusAdded, removeWords, stopwords())
corpusAdded = tm_map(corpusAdded, stemDocument)
dtmAdded = DocumentTermMatrix(corpusAdded)
str(dtmAdded)

sparseAdded = removeSparseTerms(dtmAdded, 0.997)
str(sparseAdded)

wordsAdded = as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste('A', colnames(wordsAdded))

corpusRemoved = Corpus(VectorSource(wiki$Removed))
corpusRemoved = tm_map(corpusRemoved, removeWords, stopwords())
corpusRemoved = tm_map(corpusRemoved, stemDocument)
dtmRemoved = DocumentTermMatrix(corpusRemoved)

sparseRemoved = removeSparseTerms(dtmRemoved, 0.997)
str(sparseRemoved)

wordsRemoved = as.data.frame(as.matrix(dtmRemoved))
colnames(wordsRemoved) = paste('R', colnames(wordsRemoved))

wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal = wiki$Vandal

set.seed(123)
split = sample.split(wikiWords$Vandal, SplitRatio = 0.7)

trainWiki = wikiWords[split,]
testWiki = wikiWords[!split,]

table(testWiki$Vandal)

wikiCART = rpart(Vandal ~ ., data = trainWiki, method = 'class')
testWiki$PredCART = predict(wikiCART, newdata = testWiki, type = 'class')

mean(testWiki$Vandal == testWiki$PredCART)
prp(wikiCART)

wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(str_detect(wiki$Added, 'http'), 1, 0)
sum(wikiWords2$HTTP)
str(wikiWords2$HTTP)
table(wikiWords2$HTTP)

trainWiki2 = wikiWords2[split,]
testWiki2 = wikiWords2[!split,]

wikiCART2 = rpart(Vandal ~ ., data = trainWiki2, method = 'class')
testWiki2$PredCART = predict(wikiCART2, newdata = testWiki2, type = 'class')
mean(testWiki2$Vandal == testWiki2$PredCART)

wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))

mean(wikiWords2$NumWordsAdded)

trainWiki3 = wikiWords2[split,]
testWiki3 = wikiWords2[!split,]

wikiCART3 = rpart(Vandal ~ ., data = trainWiki3, method = 'class')
testWiki3$PredCART = predict(wikiCART3, newdata = testWiki3, type = 'class')
mean(testWiki3$Vandal == testWiki3$PredCART)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin

trainWiki4 = wikiWords3[split,]
testWiki4 = wikiWords3[!split,]

wikiCART4 = rpart(Vandal ~ ., data = trainWiki4, method = 'class')

testWiki4$PredCART = predict(wikiCART4, newdata = testWiki4, type = 'class')
mean(testWiki4$Vandal == testWiki4$PredCART)
prp(wikiCART4)
