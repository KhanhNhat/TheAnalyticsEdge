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





