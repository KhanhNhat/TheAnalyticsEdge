library(tidyverse)

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
