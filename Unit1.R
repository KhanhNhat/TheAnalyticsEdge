library(tidyverse)
library(lubridate)

WHODataFrame = read.csv('WHO.csv')

#Find average of percent of Over 60 years old
mean(WHODataFrame$Over60)

#Which country has smallest percent of Over 60 years old
WHODataFrame %>% filter(Over60 == min(WHODataFrame$Over60)) %>% '$'(Country)

#Which country has biggest percent of Literacry Rate
WHODataFrame %>% filter(LiteracyRate == max(WHODataFrame$LiteracyRate, na.rm = TRUE)) %>% '$'(Country)

#Find average of Child Mortality of each region 
WHODataFrame %>% group_by(Region) %>% summarise(Average = mean(ChildMortality))


#Now do Unit 1 Exercise

#Part 1: An analytical detective

mvt = read.csv('mvtWeek1.csv')

str(mvt)

max(mvt$ID)

min(mvt$Beat)

table(mvt$Arrest)

mvt %>% filter(LocationDescription == 'ALLEY') %>% nrow()

DateConvert = mdy_hm(mvt$Date)

median(DateConvert)

mvt$Month = month(DateConvert)
mvt$Weekday = wday(DateConvert)
mvt$Date = DateConvert

mvt %>% group_by(Month) %>% summarise(Observation = n()) %>% arrange(Observation)

mvt %>% group_by(Weekday) %>% summarise(Observation = n()) %>% arrange(Observation)

mvt %>% group_by(Month) %>% 
  filter(Arrest == TRUE) %>% 
  summarise(Observation = n()) %>% 
  arrange(Observation)

ggplot(mvt, aes(x = Date)) + geom_histogram(bins = 100, col = 'white')

mvt %>% filter(Date > ymd('2005-01-01') & Date < ymd('2008-12-31')) %>% 
    ggplot(aes(x = Date)) + geom_histogram(col = 'white', bins = 100)

mvt %>% filter(Date > ymd('2009-01-01') & Date < ymd('2011-12-31')) %>% 
  ggplot(aes(x = Date)) + geom_histogram(col = 'white', bins = 100)

mvt %>% ggplot(aes(x = Arrest, y = Date)) + geom_boxplot()

mvt %>% filter(Year == 2001) %>% summarise(ArrestProportion = mean(Arrest))

mvt %>% filter(Year == 2007) %>% summarise(ArrestProportion = mean(Arrest))

mvt %>% filter(Year == 2012) %>% summarise(ArrestProportion = mean(Arrest))

mvt %>% group_by(LocationDescription) %>% summarise(Observation = n()) %>% arrange(desc(Observation))

top5 = c('STREET', 'PARKING LOT/GARAGE(NON.RESID.)', 'ALLEY', 'GAS STATION', 'DRIVEWAY - RESIDENTIAL')

mvt %>% filter(LocationDescription %in% top5) %>% nrow()

mvt %>% filter(LocationDescription %in% top5) %>%
  group_by(LocationDescription) %>%
  summarise(ArrestRate = mean(Arrest)) %>%
  arrange(desc(ArrestRate))

mvt %>% filter(LocationDescription == 'GAS STATION') %>%
  group_by(Weekday) %>%
  summarise(Observation = n()) %>%
  arrange(desc(Observation))

mvt %>% filter(LocationDescription == 'DRIVEWAY - RESIDENTIAL') %>%
  group_by(Weekday) %>%
  summarise(Observation = n()) %>%
  arrange(desc(Observation))


