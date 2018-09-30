library(tidyverse)

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
library(corrplot)
corrplot(cor(wine[-1]), method = 'ellipse', type = 'lower')

#Find correlation value
cor(wine[-1])

wine_scale = data.frame(scale(wine[-1]))
wine_scale$Year = wine$Year

model1 = lm(Price ~ HarvestRain + WinterRain, data = wine)

model2 = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data = wine_scale)
