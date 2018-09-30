library(tidyverse)
library(ggthemes)

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






