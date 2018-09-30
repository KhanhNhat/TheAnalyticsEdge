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

#Part 2: Stock price

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

#Part 3: Demographics and Employment in the United States
CPS = read.csv('CPSData.csv')

sort(table(CPS$Industry))

sort(table(CPS$State))

CPS %>% group_by(Citizenship) %>%
  summarise(CitizenProportion = n()/nrow(CPS))

CPS %>% group_by(Race) %>%
  summarise(RaceCounted = sum(Hispanic)) %>%
  arrange(desc(RaceCounted))

#Find out which variable has NA value
has_na = function(x){sum(is.na(x)) > 0}
CPS %>%
  select_if(CPS, has_na) %>%
  is.na.data.frame() %>% 
  colSums()

CPS %>%
  mutate(married_NA = is.na(Married)) %>%
  group_by(married_NA, Region) %>%
  summarise(numofIn = n())

#Two ways to find number of state has NA value
CPS %>%
  filter(is.na(MetroAreaCode)) %>%
  summarise(nState_NA = n_distinct(State))

data.frame(table(CPS$State, is.na(CPS$MetroAreaCode))) %>%
  filter(Var2 == TRUE, Freq > 0 ) %>%
  summarise(nState = n())

#When using summarise() and n(), if you want to keep the row that has value = 0
#You must combine with complete() from tidy package
#complete() will let this value = NA, so that we use fill argument to fill whatever value we want.
CPS %>%
  mutate(NA_AreaCode = is.na(MetroAreaCode)) %>%
  group_by(NA_AreaCode, State) %>%
  summarise(numOfInt = n()) %>%
  complete(State, fill = list(numOfInt = 0)) %>%
  filter(!NA_AreaCode, numOfInt == 0)

CPS %>%
  group_by(Region) %>%
  summarise(ProportionOfNA = mean(is.na(MetroAreaCode)))

CPS %>%
  group_by(State) %>%
  summarise(ProportionOfNA = mean(is.na(MetroAreaCode))) %>%
  filter(ProportionOfNA < 0.35, ProportionOfNA > 0.25) %>%
  arrange(desc(ProportionOfNA))

CPS %>%
  group_by(State) %>%
  summarise(ProportionOfNA = mean(is.na(MetroAreaCode))) %>%
  arrange(desc(ProportionOfNA))

MetroAreaMap = read.csv('MetroAreaCodes.csv')
CountryMap = read.csv('CountryCodes.csv')

CPS %>% 
  count(MetroAreaCode) %>%
  left_join(MetroAreaMap, by = c('MetroAreaCode' = 'Code')) %>%
  summarise(numArea = sum(!is.na(MetroArea)))

CPS %>% 
  count(CountryOfBirthCode) %>%
  left_join(CountryMap, by = c('CountryOfBirthCode' = 'Code')) %>%
  summarise(numCountry = sum(!is.na(Country)))

CPS_N = CPS %>%
  left_join(CountryMap, by = c('CountryOfBirthCode' = 'Code')) %>%
  left_join(MetroAreaMap, by = c('MetroAreaCode' = 'Code')) %>%
  glimpse()

#This is the way of MIT
CPS_MIT = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
summary(CPS_MIT)

CPS_N %>%
  group_by(MetroArea) %>%
  summarise(numOfInt = n()) %>%
  arrange(desc(numOfInt))

CPS_N %>%
  group_by(MetroArea) %>%
  summarise(ProportionHis = mean(Hispanic)) %>%
  arrange(desc(ProportionHis))

CPS_N %>%
  group_by(MetroArea) %>%
  summarise(ProportionAsian = mean(Race == 'Asian')) %>%
  filter(ProportionAsian >= 0.2) %>%
  arrange(desc(ProportionAsian))

CPS_N %>%
  count(Country) %>%
  arrange(desc(n))

CPS_N %>%
  filter(MetroArea == 'New York-Northern New Jersey-Long Island, NY-NJ-PA', !is.na(Country)) %>%
  summarise(notUS = mean(CountryOfBirthCode == 57))

CPS_N %>%
  filter(Country == 'India') %>%
  count(MetroArea) %>%
  arrange(desc(n))

CPS_N %>%
  filter(Country == 'Brazil') %>%
  count(MetroArea) %>%
  arrange(desc(n))

CPS_N %>%
  filter(Country == 'Somalia') %>%
  count(MetroArea) %>%
  arrange(desc(n))

CPS_N %>% 
  group_by(MetroArea) %>%
  filter(!is.na(Education)) %>%
  summarise(unEducatedPro = mean(Education == "No high school diploma")) %>%
  arrange(unEducatedPro)
