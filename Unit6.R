library(tidyverse)
library(caret)

movies = read_delim('movies.txt', delim = '|',
                    col_names = c('ID', 'Title', 'ReleaseDate', 'VideoReleaseDate', 'IMDB',
                                  'Unknown', 'Action','Adventure', 'Animation', 'Childen', 
                                  'Comedy', 'Crime', 'Documentary', 'Drama', 'Fantasy', 
                                  'FilmNoir', 'Horror', 'Musical', 'Mystery', 'Romance', 'SciFi',
                                  'Thriller', 'War', 'Western'))

movies = movies %>% select(-c(1, 3, 4, 5))
movies = unique(movies)

sum(movies$Comedy)
sum(movies$Western)
sum(movies$Romance & movies$Drama)

#Conten Filtering with Hirarchical method
distance = dist(movies[, -1], method = 'euclidean')
clusterMovies = hclust(distance, method = 'ward.D') 
movies$cluster = cutree(clusterMovies, k = 2)

geners = c('Action','Adventure', 'Animation', 'Childen', 
           'Comedy', 'Crime', 'Documentary', 'Drama', 'Fantasy', 
           'FilmNoir', 'Horror', 'Musical', 'Mystery', 'Romance', 'SciFi',
           'Thriller', 'War', 'Western')

#Create a summary of cluster
#Way 1: use summarise_at() function from dplyr
clusterSummary = movies %>% group_by(cluster) %>%
                            summarise_at(.vars = geners, .funs = mean) %>%
                            column_to_rownames('cluster') %>%
                            t(.) %>% round(., digits = 2)
                            

#Way 2: use map_dfr() and map() from purrr
movies %>% select(-c(1,2)) %>%
           split(.$cluster) %>%
           map_dfr(map, mean, na.rm = TRUE)

#Example of how to tranpose data with tidyverse 
df1 <- data.frame(rows = c("one","two","three"),two = 1:3,three=1:3)

df1 %>%
  gather(newrows,valname,-rows) %>%
  spread(rows,valname)

#Unit 6: Assignment 1
dailykos = read_csv('dailykos.csv')

word_dist = dist(dailykos, method = 'euclidean')
word_hc = hclust(word_dist, method = 'ward.D')

library(dendextend)

cluster7 = cutree(word_hc, k = 7)

#How many article in each cluster
dailykos %>% mutate(cluster = cluster7) %>%
             group_by(cluster) %>%
             summarise(ArticleInCluster = n())

#Find the 6 most common word in cluster 1
word_cl1 = dailykos %>% mutate(cluster = cluster7) %>%
                        filter(cluster == 1) %>%
                        summarise_all(.funs = sum) %>% t(.)

word_cl1_df = data.frame(word = row.names(word_cl1), numAppears = word_cl1)
word_cl1_df %>% arrange(desc(numAppears)) %>% head(.)

#How about cluster 2?
word_cl2 = dailykos %>% mutate(cluster = cluster7) %>%
                        filter(cluster == 2) %>%
                        summarise_all(.funs = sum) %>% t(.)

word_cl2_df = data.frame(word = row.names(word_cl2), numAppears = word_cl2)
word_cl2_df %>% arrange(desc(numAppears)) %>% head(.)

#How about other?
clusterSummary = dailykos %>% mutate(cluster = cluster7) %>%
                              group_by(cluster) %>%
                              summarise_all(.funs = sum) %>% t(.) %>% as.data.frame(.)

clusterSummary$word = row.names(clusterSummary)

clusterSummary %>% filter(word == 'iraq')
clusterSummary %>% filter(word %in% c('john', 'dean', 'vice', 'kerry', 'howard', 'bush'))

#Now try with kmeans()
set.seed(1000)
word_km = kmeans(dailykos, centers = 7)

dailykos %>% mutate(cluster = word_km$cluster) %>%
             group_by(cluster) %>%
             summarise(NumObservations = n())

kmSummary = dailykos %>% mutate(cluster = word_km$cluster) %>%
                         group_by(cluster) %>%
                         summarise_all(.funs = sum) %>% t(.) %>% as.data.frame(.)

kmSummary$word = row.names(kmSummary)

#Which cluster talks about 'iraq'
kmSummary %>% filter(word == 'iraq')

#Which cluster talks about Democratic party
kmSummary %>% filter(word %in% c('john', 'dean', 'vice', 'kerry', 'howard', 'bush'))

#Which Hierarchical Cluster best corresponds to K-Means Cluster 2, 3, 7, 6?
clusterSummary %>% map_dbl(~sum(.x == kmSummary$V2))

clusterSummary %>% map_dbl(~sum(.x == kmSummary$V3))

clusterSummary %>% map_dbl(~sum(.x == kmSummary$V7))

clusterSummary %>% map_dbl(~sum(.x == kmSummary$V6))

#Assignment 2.
airlines = read_csv('AirlinesCluster.csv')

summary(airlines)

#Use caret package to normalize data before process
preproc = preProcess(airlines)              #Create a model to preprocess
airlinesNorm = predict(preproc, airlines)   #Performance normalizing data

#Now, all variable have mean = 0, standard deviation = 1
summary(airlinesNorm)

airlines_dist = dist(airlinesNorm)
airlines_hc = hclust(airlines_dist, method = 'ward.D')

plot(color_branches(as.dendrogram(airlines_hc), k = 5))

air_hc5 = cutree(airlines_hc, k = 5)

#How many observation in each cluster
airlinesNorm %>% mutate(cluster = air_hc5) %>%
                 group_by(cluster) %>%
                 summarise(NumObservations = n())

#Calculate average variables for each cluster
airlinesNorm %>% mutate(cluster = air_hc5) %>%
                 group_by(cluster) %>%
                 summarise_all(.funs = mean)

#Now, try with kmeans()
set.seed(88)
airlines_km = kmeans(airlinesNorm, centers = 5, iter.max = 1000)

airlinesNorm %>% mutate(cluster = airlines_km$cluster) %>%
                 group_by(cluster) %>%
                 summarise(NumObservation = n())

airlinesNorm %>% mutate(cluster = airlines_km$cluster) %>%
                 group_by(cluster) %>%
                 summarise_all(.funs = mean)

#Assignment 3
stocks = read_csv('StocksCluster.csv')

stocks %>% filter(PositiveDec == 1) %>%
           summarise(NumPosDec = n())

library(corrplot)
corrplot(cor(stocks[,-12]), method = 'ellipse', type = 'lower')
cor(stocks[, -12])

stocks %>% summarise_all(.funs = mean) %>% glimpse(.)

library(caTools)

set.seed(144)
split = sample.split(stocks$PositiveDec, SplitRatio = 0.7)

stocksTrain = stocks[split, ]
stocksTest = stocks[!split, ]

stockLR = glm(PositiveDec ~ ., data = stocksTrain, family = 'binomial')

#The accuracy with training dataset
stocksTrain$probLR = predict(stockLR, type = 'response')
stocksTrain$predLR = ifelse(stocksTrain$probLR < 0.5, 0, 1)

mean(stocksTrain$PositiveDec == stocksTrain$predLR)

#The accuracy with testing dataset
stocksTest$probLR = predict(stockLR, newdata = stocksTest, type = 'response')
stocksTest$predLR = ifelse(stocksTest$probLR < 0.5, 0, 1)

mean(stocksTest$PositiveDec == stocksTest$predLR)

#The accuracy of base line when we only predict value =1
mean(stocksTest$PositiveDec == 1)

#Now, do clustering before building a model

#First, remove dependent variable
stocksTrain = stocks[split, ]     #Do these tasks again to make sure data are clean.
stocksTest = stocks[!split, ]

limitedTrain = stocksTrain %>% select(-12)
limitedTest = stocksTest %>% select(-12)

#Then normalize the data
preproc = preProcess(limitedTrain)  #Calculate mean, standard deviation
trainNorm = predict(preproc, newdata = limitedTrain) #Do normalizing data
testNorm = predict(preproc, newdata = limitedTest)

#Check the mean in normalized dataset
#Mean in testing dataset is not as close as training dataset
#Because their distribution are not the same and we built the model base on training dataset
mean(trainNorm$ReturnJan)
mean(testNorm$ReturnJan)

#Build kmeans
set.seed(144)
stocks_km = kmeans(trainNorm, centers = 3, iter.max = 1000)

trainNorm %>% mutate(cluster = stocks_km$cluster) %>%
              group_by(cluster) %>%
              summarise(NumObservations = n())


library(flexclust)

stocks_km_kcca = as.kcca(stocks_km, trainNorm)

train_cluster_kcca = predict(stocks_km_kcca)
test_cluster_kcca = predict(stocks_km_kcca, newdata = testNorm)

testNorm %>% mutate(cluster_kcca = test_cluster_kcca) %>%
             group_by(cluster_kcca) %>%
             summarise(NumObservation = n())

stocksTrain1 = stocksTrain[train_cluster_kcca == 1, ]
stocksTrain2 = stocksTrain[train_cluster_kcca == 2, ]
stocksTrain3 = stocksTrain[train_cluster_kcca == 3, ]

stocksTrain %>% mutate(cluster = train_cluster_kcca)
                group_by(cluster) %>%
                summarise(MeanDependent = mean(PositiveDec))

LR1 = glm(PositiveDec ~ ., data = stocksTrain1, family = 'binomial')
LR2 = glm(PositiveDec ~ ., data = stocksTrain2, family = 'binomial')
LR3 = glm(PositiveDec ~ ., data = stocksTrain3, family = 'binomial')

coef_df = data.frame(LR1 = LR1$coefficients, LR2 = LR2$coefficients, LR3 = LR3$coefficients)

coef_df %>% rownames_to_column(var = 'Month') %>% 
            filter((LR1 > 0 | LR2 > 0 | LR3 >0) & (LR1 < 0 | LR2 < 0 | LR3 < 0))

stocksTest1 = stocksTest[test_cluster_kcca == 1, ]
stocksTest2 = stocksTest[test_cluster_kcca == 2, ]
stocksTest3 = stocksTest[test_cluster_kcca == 3, ]

stocksTest1$prob = predict(LR1, newdata = stocksTest1, type = 'response')
stocksTest1$pred = ifelse(stocksTest1$prob < 0.5, 0, 1)
mean(stocksTest1$PositiveDec == stocksTest1$pred)

stocksTest2$prob = predict(LR2, newdata = stocksTest2, type = 'response')
stocksTest2$pred = ifelse(stocksTest2$prob < 0.5, 0, 1)
mean(stocksTest2$PositiveDec == stocksTest2$pred)

stocksTest3$prob = predict(LR3, newdata = stocksTest3, type = 'response')
stocksTest3$pred = ifelse(stocksTest3$prob < 0.5, 0, 1)
mean(stocksTest3$PositiveDec == stocksTest3$pred)

allPredict = c(stocksTest1$pred, stocksTest2$pred, stocksTest3$pred)
allTrueValue = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)

mean(allPredict == allTrueValue)

#More practice with kmeans
#Get dataset of line up position of 2 soccer teams.
#Use kmeans to identify players for each team.
#With kmeans() function we can easily to create model
#Hold on a second, how can we choose k = ? if we don't know there are 2 teams on the field?

#First method, use your eyes with scree plot

totalWithin = map_dbl(1:10, function(k) {
                              model = kmeans(lineup, centers = k)
                              model$tot.withinss
                            })
totalWithinDf = data.frame(k = 1:10, totalWithin)

ggplot(totalWithinDf, aes(x = k, y = totalWithin)) +
  geom_point(col = 'blue', size = 2) +
  geom_line() +
  ylab('Total within Sum of Square Error') +
  xlab('Num of cluster') +
  scale_x_continuous(breaks = 1:10)

#We can see that, in this case, k is good from 2:6
#Because of 2 teams, so we get k = 2

#Now, try the second way with 'cluster' package: Silhouette Analysis
library(cluster)

#Create a vector that store average Silhouette width of k from 2 to 10
sihouetteWidths = map_dbl(2:10, function(k){
                                  pamk = pam(lineup, k = k)
                                  pamk$silinfo$avg.width
                                })

sihouetteWidthDF = data.frame(k = 2:10, sihouetteWidths)

ggplot(sihouetteWidthDF, aes(x = k, y = sihouetteWidths)) +
  geom_point(col = 'red', size = 2) +
  geom_line(col = 'blue') +
  xlab('Num of cluster') +
  scale_x_continuous(breaks = 2:10) +
  ylab('Average Sihouette Width')

#Choose the model that has highest Average Silhouette Width, k = 2
#In this case, choose by Silhouette is more clearlier than Elbow Analysis.

