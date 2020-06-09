################################
# Create edx set, validation set - code provided by edx
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

################################
# Exploratory data analysis
################################
# Dimensions of the dataset ----
dim_dat<- dim(edx)
n_var<- dim_dat[2] #number of variables
n_obs<- dim_dat[1] #number of observations

#Exploring the variables ----
vars<-names(edx) #variable names
n_users<- n_distinct(edx$userId) #number of users included in the dataset
n_movies<- n_distinct(edx$movieId) #number of movies included in the dataset
n_ratings<- n_distinct(edx$rating) #number of possible ratings
n_genres<- edx %>%  # number of genres
  group_by(genres)%>% 
  distinct(genres) %>%
  separate_rows(genres, sep = "\\|") %>% #some movies have multiple genres listed
  summarise(n_distinct(genres))
class(edx$timestamp) <- c('POSIXt','POSIXct') # turning the timestmap in a user friendly format (aka date)
first_rating<- min(edx$timestamp) # when did the first rating in the data set occur
last_rating<- max(edx$timestamp) # # when did the last rating in the data set occur

## Exploring the users ----
users<- edx %>%
  group_by(userId) %>%
  summarise(n_ratings = n()) %>%
  arrange(desc(n_ratings))

summary(users$n_ratings) # finding min, max, avg and median ratings per user

# visualising user activity
users %>%
  ggplot(aes(x=userId,y=n_ratings, colour = n_ratings))+
  geom_point()+
  scale_colour_gradientn(colours = rainbow(7), name = "Number of Ratings")+
  scale_y_log10()+  #using log10 scale to normalise for outliers (users with exceptionally high number of ratings)
  labs(title = "Ratings per User", x = "UserId", y = "Number of Ratings (log10 scale)")+
  theme_classic()+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
 
#active users by year
users_by_year<- edx %>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>%
  summarise(users = n_distinct(userId)) %>%
  arrange(desc(users))

summary(users_by_year$users)

users_by_year

#plot users by year
users_by_year %>% 
  arrange(year) %>%
  ggplot(aes(x=year, y=users))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title = "Users by Year", x = "Year", y="Number of users")

# user ratings
user_ratings<- edx %>%
  group_by(userId)%>%
  summarise(n_ratings = n(), avg = mean(rating), med = median(rating), sd = sd(rating)) %>%
  arrange(desc(n_ratings))

head(user_ratings)

#plot avg rating per user
user_ratings %>%
  ggplot(aes(x=userId, y=avg, colour = avg))+
  geom_point(alpha=0.1)+
  scale_colour_gradientn(colours = rainbow(5), name = "Average Ratings")+
  theme_classic()+
  labs(title = "Average Rating by User", x = "UserId", y="Average Rating")

#plot sd of rating per user
user_ratings %>%
  ggplot(aes(x=userId, y=sd, colour = sd))+
  geom_point(alpha=0.1)+
  scale_colour_gradientn(colours = rainbow(5), name = "Standard Deviation of Ratings")+
  theme_classic()+
  labs(title = "Standard Deviation of Rating by User", x = "UserId", y="Standard Deviation of Rating")

## Exploring ratings ----
edx %>%
  ggplot(aes(y=rating))+
  geom_boxplot()+
  theme_classic()+
  labs(title = "Distribution of Ratings", y="Rating")+
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank())

# ratings over time
year_ratings<- edx%>%
  mutate(year = year(timestamp)) %>%
  group_by(year) %>%
  summarise(n_ratings = n(), avg = mean(rating), med=median(rating), sd=sd(rating))

summary(year_ratings$n_ratings)

#plot ratings by year
year_ratings %>% 
  arrange(year) %>%
  ggplot(aes(x=year, y=n_ratings))+
  geom_bar(stat="identity")+
  theme_classic()+
  labs(title = "Ratings per Year", x = "Year", y="Number of ratings")

#plot distribution of ratings by year
edx %>%
  mutate(year = year(timestamp)) %>%
  ggplot(aes(group=year,x=year,y=rating))+
  geom_boxplot()+
  theme_classic()+
  labs(title = "Distribution of Ratings by Year", x = "Year", y="Rating")


## Exploring the movies ----

movies<- edx %>%
  group_by(title) %>%
  summarise(n_ratings = n(), avg = mean(rating), sd = sd(rating)) %>%
  arrange(desc(n_ratings))

summary(movies)
head(movies)

# movies rated per year
movies_by_year<- edx %>%
  mutate(year = year(timestamp)) %>%
  group_by(title, year) %>%
  summarise(n_ratings = n(), avg = mean(rating), sd = sd(rating))%>%
  arrange(year)

# plot movies rated by year 
movies_by_year %>%
  ggplot(aes(x = year, y = n_ratings))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Movies rated by Year", x = "Year", y="Number of movies")

#plot avg rating by year by movie
movies_by_year %>%
  ggplot(aes(x=year, y=avg, colour = avg))+
  geom_point(alpha=0.1)+
  scale_colour_gradientn(colours = rainbow(5), name = "Average Ratings")+
  theme_classic()+
  labs(title = "Average Rating by Movie by Year", x = "Year", y="Average Rating")

# movies by release year (release year contained in movie title)
movie_year<- edx %>%
  mutate(title = str_trim(title)) %>% #removing blank spaces
  extract(title, c("title", "release_year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$",remove = TRUE, convert = TRUE) #splitting the title into title and release year columns

# plot movies produced by year
movie_year %>%
  group_by(release_year, title) %>%
  summarise(n_movies = n_distinct(movieId)) %>%
  ggplot(aes(x=release_year, y=n_movies))+
  geom_bar(stat = "identity")+
  labs(title = "Movies Produced by Year", x = "Year", y="Number of Movies")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot number of ratings by production year
movie_year %>%
  group_by(release_year, title) %>%
  summarise(n_ratings = n()) %>%
  ggplot(aes(x=release_year, y=n_ratings))+
  geom_bar(stat = "identity")+
  labs(title = "Number of ratings by Movie Release Year", x = "Year", y="Number of Ratings")+
  theme_classic()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# 10 most reviewed movies
movies %>% slice(1:10)

#Top 10 movies by rating
movies %>% 
  arrange(desc(avg)) %>%
  slice(1:10)

#Top 10 movies by rating with over 1k ratings
movies %>%
  filter(n_ratings >=1000) %>%
  arrange(desc(avg)) %>%
  slice(1:10)
  
## Exploring the movie genres ----

genres<- edx %>%
  group_by(genres) %>%
  summarise(sum_ratings = sum(rating), n_ratings = n())# we can see that movies ofte have more than one genre

dim(genres)

#breaking the genres down
genres <- genres %>%
  separate_rows(genres, sep = "\\|")

genres_ratings <- genres %>%
  group_by(genres) %>%
  summarise(n_ratings = sum(n_ratings), avg = sum(sum_ratings)/sum(n_ratings) ) %>%
  arrange(desc(n_ratings))

head(genres_ratings)

# plot genres by number of ratings
genres_ratings %>%
  ggplot(aes(x = reorder(genres, -n_ratings), y=n_ratings))+
  geom_bar(stat = "identity")+
  theme_classic()+
  labs(title = "Number of Ratings by Genre", x = "Genres", y="Number of Ratings")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot genres by avg rating
genres_ratings %>%
  ggplot(aes(x = reorder(genres, -avg), y=avg))+
  geom_point()+
  theme_classic()+
  labs(title = "Average Rating by Genre", x = "Genres", y="Average Rating")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  ylim(3,5) #as avg ratings don't vary much

#plot the most popular genres by avg rating (popular = more than 1M ratings)
#required packages: ggrepl + scales
genres_ratings %>%
  filter(n_ratings >= 1000000) %>%
  mutate(n_r = paste(round(n_ratings/1e6,1),"M")) %>%
  ggplot(aes(x = reorder(genres, -avg), y=avg, label = n_r))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = n_r), vjust = -1)+
  theme_classic()+
  labs(title = "Average Rating by Genre with more than 1M ratings", x = "Genres", y="Average Rating")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_y_continuous(limits = c(3,5), oob=rescale_none) #as avg ratings don't vary much


################################
# Modelling the data
################################

## Creating train and test sets----
set.seed(1, sample.kind = "Rounding")
test_index<- createDataPartition(y=edx$rating, times = 1, p=0.2, list = FALSE)
train_set<- edx[-test_index,]
temp<- edx[test_index,]

# Make sure userId and movieId in test set are also in train set
test_set <- temp %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# Add rows removed from test set back into train set
removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

#remove objects that we won't need anymore
rm(temp, removed, test_index)

# Write RMSE function ----
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# Basic model with just the average ----
mu_hat<- mean(train_set$rating)
naive_rmse<- RMSE(test_set$rating, mu_hat)
naive_rmse

#creating a tibble for storing RMSE results
rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)

# Modeling movie effects ----
mu <- mean(train_set$rating) 
movie_avgs <- train_set %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu))

predicted_ratings <- mu + test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)
movie_rmse<-RMSE(predicted_ratings, test_set$rating)

rmse_results<- bind_rows(rmse_results, data_frame(method = "Movie effect model",
                                                  RMSE = movie_rmse))

# Modeling movie + user effects ----
user_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

movie_user_rmse<-RMSE(predicted_ratings, test_set$rating)
rmse_results<- bind_rows(rmse_results, data_frame(method = "Movie + user effect model",
                                                  RMSE = movie_user_rmse))

## Modeling movie + user + genre effects ----
genre_avgs <- train_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by = 'userId') %>%
  group_by(genres) %>%
  summarize(b_g = mean(rating - mu - b_i - b_u))

predicted_ratings <- test_set %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(genre_avgs, by = 'genres') %>%
  mutate(pred = mu + b_i + b_u +b_g) %>%
  pull(pred)

movie_user_genre_rmse<-RMSE(predicted_ratings, test_set$rating)
rmse_results<- bind_rows(rmse_results, data_frame(method = "Movie + user + genre effect model",
                                                  RMSE = movie_user_genre_rmse))

## Regularized movie + user + genre model ----
# regularising the estimates to account for variability in the number of observations
lambdas <- seq(0, 10, 0.25) #looking for the best lambda (penalty trem)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
lambda # 4.75 appears to be the best lambda

# running the model with lambda = 4.75
mu <- mean(train_set$rating)

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+lambda))

predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)

regularised_m_u_g_rmse<-RMSE(predicted_ratings, test_set$rating)

rmse_results<- bind_rows(rmse_results, data_frame(method = "Regularised movie + user + genre effect model",
                                                  RMSE = regularised_m_u_g_rmse))

## Regularized movie + user + genre + timestamp model ----
# regularising the estimates to account for variability in the number of observations

# transforming the timestamp into date format
class(train_set$timestamp)<- c('POSIXt','POSIXct')
class(test_set$timestamp)<- c('POSIXt','POSIXct')

train_set$timestamp<- format(as.Date(train_set$timestamp), "%Y-%m")
test_set$timestamp<- format(as.Date(test_set$timestamp), "%Y-%m")

lambdas <- seq(0, 10, 0.25) #looking for the best lambda (penalty trem)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  b_g <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    group_by(genres) %>%
    summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+l))

  b_t <- train_set %>% 
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by="genres") %>%
    group_by(timestamp) %>%
    summarize(b_t = sum(rating - b_i - b_u-b_g - mu)/(n()+l))
  
  predicted_ratings <- 
    test_set %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_g, by = "genres") %>%
    left_join(b_t, by="timestamp") %>%
    mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_set$rating))
})

lambda <- lambdas[which.min(rmses)]
lambda # 5 appears to be the best lambda

# running the model with lambda = 5
mu <- mean(train_set$rating)

b_i <- train_set %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_g <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_t <- train_set %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(timestamp) %>%
  summarize(b_t = sum(rating - b_i - b_u-b_g - mu)/(n()+lambda))

predicted_ratings <- 
  test_set %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by="timestamp") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)

regularised_m_u_g_t_rmse<-RMSE(predicted_ratings, test_set$rating)

rmse_results<- bind_rows(rmse_results, data_frame(method = "Regularised movie + user + genre + timestamp effect model",
                                                  RMSE = regularised_m_u_g_t_rmse))

################################
# Showtime - testing the best model with the edx and validation sets
################################
# using the regularised movie + user + genre + timestamp model on validation data set to get RMSE results
# changing the timestamp format first
class(validation$timestamp)<- c('POSIXt','POSIXct')
validation$timestamp<- format(as.Date(validation$timestamp), "%Y-%m")
class(edx$timestamp)<- c('POSIXt','POSIXct')
edx$timestamp<- format(as.Date(edx$timestamp), "%Y-%m")

# Predicting using the best model tested above: ----
# Regularised movie + user + genre + timestamp model
mu <- mean(edx$rating)
lambda<- 5 #from the above fitting

# creating the reglurarised estimates
b_i <- edx %>% 
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))

b_u <- edx %>% 
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

b_g <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating - b_i - b_u - mu)/(n()+lambda))

b_t <- edx %>% 
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by="genres") %>%
  group_by(timestamp) %>%
  summarize(b_t = sum(rating - b_i - b_u-b_g - mu)/(n()+lambda))

#predicting the ratings
predicted_ratings <- 
  validation %>% 
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  left_join(b_t, by="timestamp") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_t) %>%
  pull(pred)

# RMSE result ----
RMSE(predicted_ratings, validation$rating)




