library(tidyverse)
library(ggthemes)
library(corrplot)
library(lubridate)

#Quick question Unit2: Wine
x = c(0, 1, 1)
yPredict = 3*x + 2
yTrue = c(2, 2, 8)

yAvg = mean(yTrue)

SSE = sum((yTrue - yPredict)^2)

SST = sum((yTrue - yAvg)^2)

R_2 = 1 - SSE/SST

wine = read.csv('wine.csv')

#To check correlation of variable in the dataframe
#Simple way
pairs(wine[-1])

#Morve visible way
corrplot(cor(wine[-1]), method = 'ellipse', type = 'lower')

#Find correlation value
cor(wine[-1])

wine_scale = data.frame(scale(wine[-1]))
wine_scale$Year = wine$Year

model1 = lm(Price ~ HarvestRain + WinterRain, data = wine)

model2 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine_scale)

#Quick question Unit2: Money ball
baseBallDF = read_csv('baseball.csv')
baseBallDF$RD = baseBallDF$RS - baseBallDF$RA

moneyBall = filter(baseBallDF, Year < 2002)

ggplot(moneyBall, aes(x = RD, y = W)) + 
  geom_point(col = 'coral', alpha = 0.8) +
  geom_smooth(method = 'lm', se = FALSE) +
  ggtitle('Relationship between Run Different and Win', 
          subtitle = 'Win = 80.8814 + 0.1058*RD') +
  theme(plot.title = element_text(color = 'grey23', hjust = 0.5, size = 15),
        plot.subtitle = element_text(color = 'grey23', hjust = 0.5, size = 12))

winReg = lm(W ~ RD, data = moneyBall)

runScoreReg = lm(RS ~ OBP + SLG, data = moneyBall)

runAllowReg = lm(RA ~ OOBP + OSLG, data = moneyBall)

players = data.frame(OBP = c(0.338, 0.391, 0.369, 0.313, 0.369),
                     SLG = c(0.54, 0.45, 0.374, 0.447, 0.5))
rownames(players) = c('Eric', 'Jeremy', 'Frank', 'Greg', 'Carlos')

predict(runScoreReg, newdata = players)

teamRank = c(1,2,3,3,4,4,4,4,5,5)
win2002 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
win2003 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(teamRank, win2002)
cor(teamRank, win2003)

#Assignment 2.1: Climate change
climate = read.csv('climate_change.csv')

climateTrain = climate %>% filter(Year <= 2006)
climateTest = climate %>% filter(Year > 2006)

#Check correlation between variables
corrplot(cor(climateTrain[-c(1,2)]), method = 'ellipse', type = 'lower')

#Create linear regression model for temperature
tempLReg = lm(data = climateTrain, Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols)

summary(tempLReg)

tempLReg2 = lm(data = climateTrain, Temp ~ MEI + TSI + Aerosols + N2O)

summary(tempLReg2)

tempLRegNull = lm(data = climateTrain, Temp ~ 1)

#Choose best linear model by going backward from full model
stepTempRegBackward = step(tempLReg)

summary(stepTempRegBackward)

#Choose best linear model by going forward from null model
stepTempRegForward = step(tempLRegNull,
                          scope = list(lower = formula(tempLRegNull), upper = formula(tempLReg)),
                          direction = 'forward')

summary(stepTempRegForward)

#Now, predict with test data
climateTest$TempPre = predict(stepTempRegBackward, newdata = climateTest)
head(climateTest)
SSE = sum((climateTest$Temp - climateTest$TempPre)^2)
SST = sum((climateTest$Temp - mean(climateTrain$Temp))^2)
(R2_TestData = 1 - SSE/SST)
add_predictions(data = climateTest, model = stepTempRegBackward, var = 'modelrPred')


#Assignment 2.2

pisaTrain = read.csv('pisa2009train.csv')
pisaTest = read.csv('pisa2009test.csv')

pisaTrain %>%
  group_by(male) %>%
  summarise(AverageReading = mean(readingScore))

#We use has_na function from Unit1.R to find which variable has NA value
select_if(pisaTrain, has_na) %>%
  is.na.data.frame() %>%
  colSums()

#Remove all observations have NA values
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)

#Relevel the factor variable raceeth, make 'White' become the first and use it as reference level
pisaTrain$raceeth = relevel(pisaTrain$raceeth, 'White')
pisaTest$raceeth = relevel(pisaTest$raceeth, 'White')

lmScore = lm(data = pisaTrain, readingScore ~ .)

summary(lmScore)

predTest = predict(lmScore, newdata = pisaTest)

#Assignment 2.3
fluTrain = read.csv('FluTrain.csv')
fluTrain$Year = year(fluTrain$Week)
fluTrain$Month = month(fluTrain$Week)

fluTrain %>% filter(ILI == max(ILI))
fluTrain %>% filter(Queries == max(Queries))

ggplot(data = fluTrain, aes(x = ILI)) + geom_histogram(col = 'white')

ggplot(data = fluTrain, aes(x = log(ILI), y = Queries)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

fluTrend1 = lm(data = fluTrain, log(ILI) ~ Queries)
summary(fluTrend1)

fluTest = read.csv('FluTest.csv')

fluTest$Predict = exp(predict(fluTrend1, newdata = fluTest))
fluTest$Month = month(fluTest$Week)

fluTest %>% filter(Month == 3)

#We cannot use directly this code: rmse(data = fluTest, model = fluTrend1)
#Because we made a mode with log(ILI) not with ILI
sqrt(mean((fluTest$ILI - fluTest$Predict)^2))

#We can use information from 2 weeks ago to predict this week
#In training dataset, we create a column that store information of two weeks delay
fluTrain$ILILag2 = c(NA, NA, fluTrain$ILI[-c(416, 417)])
fluTest$ILILag2 = c(fluTrain$ILI[416], fluTrain$ILI[417], fluTest$ILI[-c(51, 52)])

ggplot(data = fluTrain, aes(x = ILILag2, y = ILI)) + 
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)

fluTrend2 = lm(data = fluTrain, log(ILI) ~ log(ILILag2) + Queries)

summary(fluTrend2)

fluTest$Predict2 = exp(predict(fluTrend2, newdata = fluTest))

sqrt(mean((fluTest$ILI - fluTest$Predict2)^2))
