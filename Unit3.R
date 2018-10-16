library(tidyverse)
library(caTools)

#Unit 3: Quick question about Poor Care
quality = read.csv('quality.csv')

set.seed(88)

split = sample.split(quality$PoorCare, SplitRatio = 0.75)

qualityTrain = subset(quality, split)
qualityTest = subset(quality, !split)

qualityLogReg = glm(PoorCare ~ StartedOnCombination + ProviderCount, data = qualityTrain, family = 'binomial')
summary(qualityLogReg)

qualityLogReg2 = glm(PoorCare ~ Narcotics + OfficeVisits, data = qualityTrain, family = 'binomial')
summary(qualityLogReg2)

table(qualityTrain$StartedOnCombination, qualityTrain$PoorCare)

qualityTrain$Pred = predict(qualityLogReg, type = 'response')

qualityTrain$Pred2 = predict(qualityLogReg2, type = 'response')

#Let see, if it is poor care, what is average of probability
qualityTrain %>%
  group_by(PoorCare) %>%
  summarise(AverageProb = mean(Pred))

#Draw ROC 
#With colAUC() from caTools
#1 model
colAUC(qualityTrain$Pred, qualityTrain$PoorCare, plotROC = TRUE)

#2 models at the same time to compare
colAUC(qualityTrain[c(15,16)], qualityTrain$PoorCare, plotROC = TRUE)

#With ROCR package
library(ROCR)
ROC_pred = prediction(qualityTrain$Pred, qualityTrain$PoorCare)
ROC_perf = performance(ROC_pred, 'tpr', 'fpr')
plot(ROC_perf, colorize = TRUE)

qualityTest$Pred = predict(qualityLogReg, newdata = qualityTest, type = 'response')
ROC_pred_test = prediction(qualityTest$Pred, qualityTest$PoorCare)

#To calculate AUC
ROC_perf_test = performance(ROC_pred_test, 'auc')
#Use @ to extract a slot inside ROC_perf_test
as.numeric(ROC_perf_test@y.values)

#Unit 3: Quick question about Framingham Study
framingham = read.csv('framingham.csv')

set.seed(1000)
split = sample.split(framingham$TenYearCHD, SplitRatio = 0.65)
train = framingham[split,]
test = framingham[!split,]

framinghamLogReg = glm(TenYearCHD ~ ., data = train, family = 'binomial')

test$Pred = predict(framinghamLogReg, newdata = test, type = 'response')

colAUC(test$Pred, test$TenYearCHD, plotROC = TRUE)

test_pred = prediction(test$Pred, test$TenYearCHD)
as.numeric(performance(test_pred, 'auc')@y.values)

#Assignment 3.1

songs = read.csv('songs.csv')

songs %>% filter(year == 2010) %>% summarise(songIn2010 = n())

songs %>% filter(artistname == 'Michael Jackson') %>% summarise(numOfMJ = n())

songs %>% filter(artistname == 'Michael Jackson' & Top10 == 1) %>% select(songtitle)

songs %>% group_by(timesignature) %>% summarise(numOfObservations = n())

songs %>% filter(tempo == max(tempo)) %>% select(songtitle)

#Make training and testing data
#Only keep variable that has meaning to build model

songsTrain = songs %>% filter(year <= 2009)
nrow(songsTrain)
songsTrain = songsTrain[,-c(1,2,3,4,5)]

songsTest = songs %>% filter(year > 2009)
songsTest = songsTest[,-c(1,2,3,4,5)]

#Create a first model
Model1 = glm(Top10 ~ ., data = songsTrain, family = 'binomial')
summary(Model1)

songLogReg2 = glm(Top10 ~ . - loudness, data = songsTrain, family = 'binomial')
summary(songLogReg2)

songLogReg3 = glm(Top10 ~ . - energy, data = songsTrain, family = 'binomial')
summary(songLogReg3)

songsTest$Prob = predict(songLogReg3, newdata = songsTest, type = 'response')
songsTest$Pred = ifelse(songsTest$Prob < 0.45, 0, 1)

#Create a refusion matrix
songsTest %>% group_by(Top10) %>% summarise(predictNo = sum(Pred == 0),
                                            predictYes = sum(Pred == 1))

#Assignment 3.2
parole = read.csv('parole.csv')

sum(parole$violator == 1)

#Convert state and crime variables to factor
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)

summary(parole)

#Now split data to training and testing, then build a logistic regression model
set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)

paroleTrain = parole[split,]
paroleTest = parole[!split,]

paroleLogReg = glm(violator ~ ., data = paroleTrain, family = 'binomial')

summary(paroleLogReg)

specificTest = parole[1,]
specificTest = add_row(specificTest, male = 1, race = 1, age = 50, state = 1, time.served = 3,
                      max.sentence = 12, multiple.offenses = 0, crime = 2, violator = 0)

#We have formula to calculate Odds from probability p
#Odds = p/(1 - p)

paroleTest$Prob = predict(paroleLogReg, newdata = paroleTest, type = 'response')

max(paroleTest$Prob)

paroleTest$Pred = ifelse(paroleTest$Prob < 0.5, 0, 1)

#Create a refusion matrix
paroleTest %>% group_by(violator) %>% summarise(predictNo = sum(Pred == 0), predictYes = sum(Pred == 1))

parole_test_pred = prediction(paroleTest$Prob, paroleTest$violator)
as.numeric(performance(parole_test_pred, 'auc')@y.values)

#Assignment 3.3
loans = read.csv('loans.csv')

mean(loans$not.fully.paid)

#Find out which variable has NA value
#has_na return TRUE if vector x has NA value
has_na = function(x){sum(is.na(x)) > 0}

#First just select only the variables that have NA value
#Convert this dataset to a dataset with only TRUE/FALSE value
#TRUE at NA position, FALSE at other case.
#Make a column sum to find how many NA in this variable.
select_if(loans, has_na) %>%
  is.na.data.frame() %>% 
  colSums()

loans_imputed = read.csv('loans_imputed.csv')

set.seed(144)

split = sample.split(loans_imputed$not.fully.paid, SplitRatio = 0.7)

loanImputedTrain = loans_imputed[split, ]
loanImputedTest = loans_imputed[!split, ]

loanImputedLogReg = glm(not.fully.paid ~ ., data = loanImputedTrain, family = 'binomial')

summary(loanImputedLogReg)

loanImputedTest$Prob = predict(loanImputedLogReg, newdata = loanImputedTest, type = 'response')
loanImputedTest$Pred = ifelse(loanImputedTest$Prob < 0.5, 0, 1)

loanImputedTest %>% group_by(not.fully.paid) %>%
                    summarise(predictNo = sum(Pred == 0), predictYes = sum(Pred == 1))

loan_test_pred = prediction(loanImputedTest$Prob, loanImputedTest$not.fully.paid)
as.numeric(performance(loan_test_pred, 'auc')@y.values)

#Create a bivariate model
loan_biLogReg = glm(not.fully.paid ~ int.rate, data = loanImputedTrain, family = 'binomial')

loanImputedTest$Prob2 = predict(loan_biLogReg, newdata = loanImputedTest, type = 'response')
loanImputedTest$Pred2 = ifelse(loanImputedTest$Prob2 < 0.5, 0, 1)

#Find the highest probability of not full payment with the second model
max(loanImputedTest$Prob2)

#Create a refusion matrix for the second model
loanImputedTest %>% group_by(not.fully.paid) %>%
  summarise(predictNo = sum(Pred2 == 0), predictYes = sum(Pred2 == 1))

loan_bi_pred = prediction(loanImputedTest$Prob2, loanImputedTest$not.fully.paid)
as.numeric(performance(loan_bi_pred, 'auc')@y.values)

#Create a profit variable
loanImputedTest$profit = exp(loanImputedTest$int.rate * 3) - 1
loanImputedTest$profit[loanImputedTest$not.fully.paid == 1] = -1

#Find the highest profit
max(loanImputedTest$profit)

#Create a dataset with only high interest rate, equal or large than 15%
hightRateLoans = loanImputedTest %>% filter(int.rate >= 0.15)

#Average profit with this high interest rate
mean(hightRateLoans$profit)

#Proportion of not full payment
mean(hightRateLoans$not.fully.paid == 1)

#Sort by predicted probability of unpayment from the first model
hightRateLoans = hightRateLoans %>% arrange(Prob)

#Get 100 of smallest probability of unpayment
selectedLoan = hightRateLoans[seq(1, 100, 1),]

#Find the total of proit
sum(selectedLoan$profit)

#Find how many case of unpayment
sum(selectedLoan$not.fully.paid == 1)

min(hightRateLoans$Prob2)
