rm(list=ls())

train <- read.csv("train_data.csv")

train$Positive <- as.factor(train$sentiment==1)
train$Neutral <-as.factor(train$sentiment==0)

train$Negative <-as.factor(train$sentiment==-1)

#only left neutral columns
Neutral <- subset(train, select = -c(sentiment, Positive , Negative ))

#remove all sentiment = 0, neutral rows
NonNeutral <- subset(train, sentiment != 0)
NonNeutral <- subset(train, select = -c(sentiment, Neutral , Negative ))

# ----------------------Train and Test Dataset--------------------------
library(caTools)
set.seed(123)

# Create train and test sets (with balanced response)

# Neutral
spl_Neutral <- sample.split(Neutral$Neutral,SplitRatio=0.7)
train_Neutral <- subset(Neutral, spl_Neutral==TRUE)
test_Neutral <- subset(Neutral, spl_Neutral==FALSE)

# NonNeutral
spl_NonNeutral <- sample.split(NonNeutral$Positive,SplitRatio=0.7)
train_NonNeutral <- subset(NonNeutral, spl_NonNeutral==TRUE)
test_NonNeutral <- subset(NonNeutral, spl_NonNeutral==FALSE)