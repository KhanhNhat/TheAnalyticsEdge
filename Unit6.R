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

