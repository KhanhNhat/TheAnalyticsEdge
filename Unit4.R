library(tidyverse)
library(caTools)
library(caret)
library(rpart)
library(rpart.plot)
library(ROCR)
library(randomForest)

steven = read_csv('stevens.csv',
                  col_types = cols(
                    Docket = col_character(),
                    Term = col_integer(),
                    Circuit = col_factor(NULL),
                    Issue = col_factor(NULL),
                    Petitioner = col_factor(NULL),
                    Respondent = col_factor(NULL),
                    LowerCourt = col_factor(NULL),
                    Unconst = col_integer(),
                    Reverse = col_integer()))

#First, we use rpart to build a tree classification
set.seed(3000)
split_ca = sample.split(steven$Reverse, SplitRatio = 0.7)
stevenTrain = steven[split_ca,]
stevenTest = steven[!split_ca,]

stevenTree = stevenTrain %>% rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                                   data = ., method = 'class', minbucket = 25)

prp(stevenTree)

stevenTest$Tree_Pred = predict(stevenTree, newdata = stevenTest, type = 'class')

#Confusion matrix
stevenTest %>% group_by(Reverse) %>%
               summarise(ReversePred = sum(Tree_Pred == 1),
                         AffirmPred = sum(Tree_Pred == 0))
#Accuracy
stevenTest %>% summarise(Accuracy = sum(Reverse == Tree_Pred)/nrow(stevenTest))


stevenROC_Pred = prediction(stevenTest_Pred[,2], stevenTest$Reverse)
stevenROC_Perf = performance(stevenROC_Pred, 'tpr', 'fpr')
plot(stevenROC_Perf, colorize = TRUE)

as.numeric(performance(stevenROC_Pred, 'auc')@y.values)

#Now we use randomForest
stevenTrain$Reverse = as.factor(stevenTrain$Reverse)
stevenTest$Reverse = as.factor(stevenTest$Reverse)

set.seed(200)
stevenRForest = stevenTrain %>% 
                  randomForest(Reverse ~  Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                               data = ., nodesize = 25, ntree = 200)

stevenTest$RF_Pred = predict(stevenRForest, newdata = stevenTest)

#Confusion matrix
stevenTest %>% group_by(Reverse) %>%
               summarise(ReversePred = sum(RF_Pred == 1),
                         AffirmPred = sum(RF_Pred == 0))
#Accuracy
stevenTest %>% summarise(Accuracy = sum(Reverse == RF_Pred)/nrow(stevenTest))


#Cross Validation for CART
numFold = trainControl(method = 'cv', number = 10)
cpGrid = expand.grid(.cp = seq(0.1, 0.5, 0.01))

#To use this cross validation we also need e1071 package
library(e1071)
stevenTrain %>% train(Reverse ~  Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                      data = ., method = 'rpart', trControl = numFold, tuneGrid = cpGrid)

stevenCV = stevenTrain %>% rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt + Unconst,
                                 data = ., method = 'class', cp = 0.18)

stevenTest$CV_Pred = predict(stevenCV, newdata = stevenTest, type = 'class')

#Cross Validation Confusion matrix
stevenTest %>% group_by(Reverse) %>%
               summarise(ReversePred = sum(CV_Pred == 1),
                         AffirmPred = sum(CV_Pred == 0))
#Cross validation accuracy
stevenTest %>% summarise(Accuracy = sum(Reverse == CV_Pred)/nrow(stevenTest))

#Plot cross validation tree
prp(stevenCV)
