#knn


library(ISLR)
library(GGally)
library(tidyverse)
library("reshape2")
library(MASS)
library(caret)
library(class)
library(dplyr)
library(e1071)

#install.packages('e1071')
#install.packages('dplyr')
#install.packages('rpart')
#install.packages('caret')

library(FNN) 
library(gmodels) 
library(psych)
library(fastDummies)

library('gmodels')
library('caret')

setwd('D:/Spring_2021/STAT 515_Prof.ScottBruce/Final')
getwd()

churn <- read.csv("Churn_Modelling.csv")
head(churn)
tail(churn)

shuffle_index <- sample(1:nrow(churn))
head(shuffle_index)

churn <- churn[shuffle_index, ]
head(churn)

#churn <- subset (churn, select = -c(RowNumber, CustomerId, Surname))
#head(churn)


clean_churn <- churn %>%
  dplyr::select(-c(RowNumber, CustomerId, Surname)) %>% 
  mutate(HasCrCard = factor(HasCrCard, levels = c(0,1), labels = c('No', 'Yes')),
         Exited = factor(Exited, levels = c(0,1), labels = c('No', 'Yes')),
         IsActiveMember = factor(IsActiveMember, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()

glimpse(clean_churn)
head(clean_churn)
create_train_test <- function(clean_churn, size = 0.7, train = TRUE)
{
  n_row = nrow(clean_churn)
  total_row = size * n_row
  
  
  train_sample <- 1: total_row;
  
  if (train == TRUE) {
    return (clean_churn[train_sample, ])
  } else {
    return (clean_churn[-train_sample, ])
  }
  
}
---
  s <- nrow(churn)*0.7
s
t <- 1:s
t
clean_churn[t, ]
----
data_train <- create_train_test(clean_churn, 0.7, train = TRUE)
data_test <- create_train_test(clean_churn, 0.7, train = FALSE)
dim(data_train)
dim(data_test)



attach(churn)
# KNN Classification

head(data_train)
str(clean_churn)
exited_outcome <- data_train %>% dplyr::select(Exited)
exited_outcome$Exited

# Feature Scaling

data_train[, c("CreditScore", "Age", "Tenure", "Balance", "NumOfProducts",
               "EstimatedSalary")] <- scale(data_train[, c("CreditScore", "Age", "Tenure", "Balance", "NumOfProducts",
                                                           "EstimatedSalary")])

head(data_train)

data_train <- data_train %>% dplyr::select(-Exited)


head(data_train)

glimpse(data_train)
str(data_train)
data_train$HasCrCard <- ifelse(data_train$HasCrCard  == "yes", 1, 0)
data_train$IsActiveMember <- ifelse(data_train$IsActiveMember == "yes", 1, 0)

#data_train$NumOfProducts = as.factor(data_train$NumOfProducts)
#dummy
data_train <- dummy_cols(data_train)
head(data_train)

data_train <- data_train %>% dplyr::select(-Geography)
data_train <- data_train %>% dplyr::select(-Gender)
#data_train <- data_train %>% dplyr::select(-NumOfProducts)


###test

head(data_test)

exited_outcome_test <- data_test %>% dplyr::select(Exited)
exited_outcome_test$Exited

# Feature Scaling

data_test[, c("CreditScore", "Age", "Tenure", "Balance", "NumOfProducts",
               "EstimatedSalary")] <- scale(data_test[, c("CreditScore", "Age", "Tenure", "Balance", "NumOfProducts",
                                                           "EstimatedSalary")])

head(data_test)

data_test <- data_test %>% dplyr::select(-Exited)

data_test
head(data_test)


str(data_test)

data_test$HasCrCard <- ifelse(data_test$HasCrCard  == "yes", 1, 0)
data_test$IsActiveMember <- ifelse(data_test$IsActiveMember == "yes", 1, 0)


#data_train$NumOfProducts = as.factor(data_train$NumOfProducts)
#dummy
data_test <- dummy_cols(data_test)
head(data_test)

data_test <- data_test %>% dplyr::select(-Geography)
data_test <- data_test %>% dplyr::select(-Gender)




head( exited_outcome)

# Fitting KNN Model 
# to training dataset k=1

library(class)
nrow(data_train)
v1 = exited_outcome[,1]
classifier_knn <- knn(train = data_train,
                      test = data_test,
                      cl = v1,
                      k = 1)
classifier_knn


cm <- table(exited_outcome_test$Exited, classifier_knn)
cm

misClassError <- mean(classifier_knn != exited_outcome_test$Exited)
print(paste('Accuracy =', 1-misClassError))

#k=10


classifier_knn <- knn(train = data_train,
                      test = data_test,
                      cl = v1,
                      k = 10)
classifier_knn


cm <- table(exited_outcome_test$Exited, classifier_knn)
cm

misClassError <- mean(classifier_knn != exited_outcome_test$Exited)
print(paste('Accuracy =', 1-misClassError))


#k=9


classifier_knn <- knn(train = data_train,
                      test = data_test,
                      cl = v1,
                      k = 9)
classifier_knn


cm <- table(exited_outcome_test$Exited, classifier_knn)
cm

misClassError <- mean(classifier_knn != exited_outcome_test$Exited)
print(paste('Accuracy =', 1-misClassError))




