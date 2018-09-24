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

#Part 2 Stock price

IBM = read.csv('IBMStock.csv')
GE = read.csv('GEStock.csv')
ProcterGamble = read.csv('ProcterGambleStock.csv')
CocaCola = read.csv('CocaColaStock.csv')
Boeing = read.csv('BoeingStock.csv')

str(IBM)

IBM$Date = mdy(IBM$Date)
GE$Date = mdy(GE$Date)
ProcterGamble$Date = mdy(ProcterGamble$Date)
CocaCola$Date = mdy(CocaCola$Date)
Boeing$Date = mdy(Boeing$Date)

mean(IBM$StockPrice)

min(GE$StockPrice)

max(CocaCola$StockPrice)

median(Boeing$StockPrice)

sd(ProcterGamble$StockPrice)

CocaCola %>% ggplot(aes(x = Date, y = StockPrice)) + geom_line()

ggplot(CocaCola, aes(x = Date, y = StockPrice)) +
  geom_line() +
  geom_line(data = ProcterGamble, aes(x = Date, y = StockPrice), col = 'blue') + 
  geom_vline(xintercept = ymd('2000-03-01'))

StockPrice = bind_rows('IBM' = IBM, 'GE' = GE, 
                       'ProcterGamble' = ProcterGamble,
                       'CocaCola' = CocaCola, 'Boeing' = Boeing, .id = 'Company')

StockPrice$Company = as.factor(StockPrice$Company)

StockPrice %>% ggplot(aes(x = Date, y = StockPrice, col = Company)) + 
  geom_vline(xintercept = ymd('2000-03-01'), col = 'grey') +
  geom_vline(xintercept = ymd('1997-09-01'), col = 'grey') +
  geom_vline(xintercept = ymd('1997-10-01'), col = 'grey') +
  geom_line()

IBM %>% mutate(Month = month(Date)) %>%
  group_by(Month) %>%
  summarise(MonthSP = mean(StockPrice)) %>%
  filter(MonthSP > mean(IBM$StockPrice))

GE %>% mutate(Month = month(Date)) %>%
  group_by(Month) %>%
  summarise(MonthSP = mean(StockPrice)) %>%
  arrange(desc(MonthSP))

CocaCola %>% mutate(Month = month(Date)) %>%
  group_by(Month) %>%
  summarise(MonthSP = mean(StockPrice)) %>%
  arrange(desc(MonthSP))

