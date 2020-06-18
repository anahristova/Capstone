################################
# Setup - load packages and data
################################
# Install required packages ----
# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(leaps)) install.packages("leaps", repos = "http://cran.us.r-project.org")
if(!require(pls)) install.packages("pls", repos = "http://cran.us.r-project.org")

## download and import the abalone.csv file from GitHub ----
# url  is https://raw.githubusercontent.com/anahristova/Capstone/master/abalone.csv

url<- "https://raw.githubusercontent.com/anahristova/Capstone/master/abalone.csv"
abalone<- read_csv(url(url))

#renaming the columns for ease of use
names(abalone)=c("sex", "length", "diameter", "height", "whole","shucked", "viscera", "shell", "rings")

## For the purpose of accurately modelling the data and obtaining predictions we can validate
# the data will be split in two parts - abalone and validation. 
# Validation set will be 10% of the whole abalone data
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = abalone$rings, times = 1, p = 0.1, list = FALSE)
validation <- abalone[test_index,]
abalone <- abalone[-test_index,]

#removing url and test_index as we don't need those anymore
rm(url, test_index)

################################
# Exploring the data
################################

# Dimensions of the dataset ----
dim_dat<- dim(abalone)
n_var<- dim_dat[2] #number of variables
n_obs<- dim_dat[1] #number of observations

#Exploring the variables ----
str(abalone)
head(abalone) # viewing the first 6 rows of data
abalone$sex<- as.factor(abalone$sex) # changing sex from character to factor, as it is categorical
length(unique(abalone$rings)) # rings, which represent the age of the abalone are discrete

# Exploring the sex variable ----

# Let's look into how many abalones we have for each sex
abalone %>%
  group_by(sex) %>%
  summarise(count = n())


# Exploring the size variables ----
#examining the size variables
abalone %>%
  select(height, diameter, length) %>%
  summary

# there appears to be some abalone with Hight = 0 mm, let's check them out
abalone %>%
  filter(height==min(height))

# plotting the distribution of the size variables
abalone %>%
  select(height, diameter, length) %>%
  pivot_longer(cols = c(height, diameter, length), names_to = "variable", values_to = "Value") %>%
  ggplot(aes(Value, color = variable))+
  geom_boxplot()+
  coord_flip()

#examining the relationships between the size variables

abalone %>%
  select(height, diameter, length) %>%
  cor %>%
  corrplot(type = "upper")


# Exploring the weight variables ----
#examining the weight variables
abalone %>%
  select(whole,shucked,viscera,shell) %>%
  summary

# plotting the distribution of the weight variables
abalone %>%
  select(whole,shucked,viscera,shell) %>%
  pivot_longer(cols = c(whole,shucked,viscera,shell), names_to = "variable", values_to = "Value") %>%
  ggplot(aes(Value, color = variable))+
  geom_boxplot()+
  coord_flip()

#examining the relationships between the weight variables

abalone %>%
  select(whole,shucked,viscera,shell) %>%
  cor %>% 
  corrplot(type = "upper")

# examining relationship between weight and rings
abalone %>%
  select(whole, rings)%>%
  cor


# Exploring the relationships between weight and size ----

abalone %>%
  select(-sex, -rings) %>%
  cor %>%
  corrplot(type = "upper")

# Exploring the relationship between size, weight and sex ----
abalone%>%
  pivot_longer(cols = c(height, diameter, length), names_to = "variable", values_to = "Value") %>%
  ggplot(aes(Value, color = sex))+
  facet_wrap(~variable)+
  geom_boxplot()+
  coord_flip()

# Exploring the relationships between size, weight, sex and rings ----

# sex & rings
# average number of rings by sex
abalone %>%
  group_by(sex) %>%
  summarise(avg_rings = mean(rings))

# plot distribution of rings by sex
abalone %>%
  ggplot(aes(rings, color = sex))+
  geom_boxplot()+
  coord_flip()

abalone %>%
  ggplot(aes(rings, fill = sex))+
  geom_density()

# Size & rings

# examining relationship between size and rings
abalone %>%
  mutate(size = diameter*3.14*height) %>%
  select(size, rings)%>%
  cor

abalone %>%
  mutate(size = diameter*3.14*height) %>%
  ggplot(aes(x=size, y=rings))+
  geom_point()

## Examining the relationship between all vars and rings
abalone %>%
     select(-sex) %>%
     gather(key = "var", value = "val") %>%
     ggplot(aes(val))+
     facet_wrap(~ var, scales = "free")+
     geom_histogram(bins=10)+
  labs(title = "Distributions of the Numeric Variables")+
  theme_classic()


abalone %>%
  select(-sex) %>%
  cor %>%
  corrplot(type = "upper")


abalone %>%
  mutate(sex = ifelse(sex == "I",0,1)) %>%
  cor %>%
  corrplot(type = "upper", method = "number")

################################
# Modelling the data
################################
## Splitting the data into train and test sets ----
set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = abalone$rings, times = 1, p = 0.1, list = FALSE)
test <- abalone[test_index,]
train <- abalone[-test_index,]

## Write RMSE function ----
RMSE <- function(true_rings, predicted_rings){
  sqrt(mean((true_rings - predicted_rings)^2))
}

#Principal component regression ----
set.seed(1, sample.kind = "Rounding")
train_pc<- train(
  rings~.-sex, data = train, method = "pcr",
  scale = TRUE,
  trControl = trainControl("cv", number = 7),
  tuneLength = 7
)

plot(train_pc)

fit_pc<- train_pc$finalModel
pred_pc<- predict(fit_pc, test)
pc_reg<-RMSE(test$rings, pred_pc)

#creating a tibble for storing RMSE results ----
rmse_results <- tibble(method = "Principal component regression", RMSE = pc_reg )

# Partial least squares 
set.seed(1, sample.kind = "Rounding")
train_pls <- train(
  rings~.-sex, data = train, method = "pls",
  scale = TRUE,
  trControl = trainControl("cv", number = 7),
  tuneLength = 7
)

plot(train_pls)

fit_pls<- train_pls$finalModel
pred_pls<- predict(fit_pls, test)
pls<-RMSE(test$rings, pred_pls)

rmse_results<- bind_rows(rmse_results, data_frame(method = "Partial least squares",
                                                  RMSE = pls))

##kNN model ----
fit_knn<- train(rings~., method = "knn",
                tuneGrid=data.frame(k=seq(1,20,1)),
                data=train)
ggplot(fit_knn)

fit_knn<- train(rings~., method = "knn", data=train)
pred_knn<- predict(fit_knn, test)
knn_model<-RMSE(test$rings,pred_knn)

rmse_results<- bind_rows(rmse_results, data_frame(method = "k Nearest Neigbors",
                                                  RMSE = knn_model))



################################
# Showtime - testing the best model with the abalone and validation sets
################################
# Determining the best model based on RMSE
rmse_results %>%
  filter(rmse_results$RMSE == min(rmse_results$RMSE))

# Predicting using the kNN model: ----

fit_knn_final<- train(rings~., method = "knn", data=abalone)
pred_knn_final<- predict(fit_knn_final, validation)

# RMSE result ----
RMSE(validation$rings, pred_knn_final)


