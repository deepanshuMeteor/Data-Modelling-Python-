#
# Titanic task
#
setwd("C:\\Users\\Admin\\Desktop\\R code\\titanic_dataset")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("descr")
library(tidyverse)
library(dplyr)
library(descr)

# read the data
train <- read.csv("titanic_train.csv", header = TRUE)
test  <- read.csv("titanic_test.csv",  header = TRUE)

test <- data.frame(Survived = rep(-1, nrow(test)), test)
train$IsTrainSet <- TRUE
test$IsTrainSet  <- FALSE
titanic <- rbind(train, test)

# Data cleaning starts here
table(titanic$Embarked)
freq(titanic$Embarked)

titanic[titanic$Embarked == "", "Embarked"] <- "S"
freq(titanic$Embarked)

# Next, let's find all the records where Age is missing
freq(titanic$Age)
# if we try calculating median
median(titanic$Age)
# we'll get NA - just because dataset contains NA
# we need to calculate median excluding NA's:
median(titanic$Age, na.rm = TRUE)
#
# and store it into those 263 records:
titanic[is.na(titanic$Age), "Age"] <- median(titanic$Age, na.rm = TRUE)
# check it:
freq(titanic$Age)

#
# Finishing touches - convert some fields to categorical data
# (in R they are called factors)
#
str(titanic)
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass   <- as.factor(titanic$Pclass)
str(titanic)

#
# FINALLY,
# split the titanic dataset back into training and testing sets
train <- titanic[titanic$IsTrainSet  , ]
test  <- titanic[!titanic$IsTrainSet , ]

# and drop the unused columns:
train <- train %>% select(-IsTrainSet)
test  <- test  %>% select(-IsTrainSet, -Survived)

###########################################################################
install.packages("rpart")
library(rpart)

equation <- "Survived ~ Pclass + Age + Sex"
result <- rpart(equation, data = train, method = "class")

plot(result)
text(result)
summary(result)

###########################################################################
install.packages("rattle")
install.packages("rpart.plot")
install.packages("RColorBrewer")
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(result)

###########################################################################
equation <- "Survived ~ Pclass + Sex + Age + SibSp + Fare + Parch + Embarked"
result <- rpart(equation, data = train, method = "class")
fancyRpartPlot(result)

