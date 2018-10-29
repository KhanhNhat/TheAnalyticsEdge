library(tidyverse)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(randomForest)


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





