library(ISLR)
library(GGally)
library(tidyverse)
library("reshape2")

install.packages('rpart')

setwd('D:/Spring_2021/STAT 515_Prof.ScottBruce/Final')
getwd()

churn <- read.csv("Churn_Modelling.csv")
head(churn)
tail(churn)
str(churn)
shuffle_index <- sample(1:nrow(churn))
head(shuffle_index)

churn <- churn[shuffle_index, ]
head(churn)

#Decision Tree Classification
library(dplyr)
str(churn)
# Drop variables
clean_churn <- churn %>%
  select(-c(RowNumber, CustomerId, Surname)) %>% 
  mutate(HasCrCard = factor(HasCrCard, levels = c(0,1), labels = c('No', 'Yes')),
         Exited = factor(Exited, levels = c(0,1), labels = c('No', 'Yes')),
         IsActiveMember = factor(IsActiveMember, levels = c(0, 1), labels = c('No', 'Yes'))) %>%
  na.omit()


glimpse(clean_churn)
head(clean_churn)
--------
#churn <- subset (churn, select = -c(RowNumber, CustomerId, Surname))
#head(churn)
-----------------
#create_train_test(churn, size = 0.7, train = TRUE)

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

prop.table(table(data_train$Exited))
prop.table(table(data_test$Exited))

#printcp(fit) 

install.packages("rpart.plot")

library(rpart)
library(rpart.plot)
fit <- rpart(Exited~ Age + IsActiveMember +NumOfProducts, data = data_train, method = 'class')
rpart.plot(fit, extra = 106)


fit <- rpart(Exited~ Age + IsActiveMember +NumOfProducts, data = data_test, method = 'class')
rpart.plot(fit, extra = 106)


#pdf("tree.pdf")
#fancyRpartPlot(fit)
#dev.close()
#system("evince tree.pdf")





  
  

