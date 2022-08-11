rm(list=ls())

source("modelSubset.R")

library(randomForest)

# Random Forests
# ---------------------------Neutral Model---------------------------
modelNeutral <- randomForest(Neutral~., data=train_Neutral) 
# summary(modelNeutral)

# importance(modelNeutral)
varImpPlot(modelNeutral) # plot

predict_nn <- predict(modelNeutral, newdata=test_Neutral, type="class")

tab_nn <- table(predict_nn,test_Neutral$Neutral)
Accuracy = (tab_nn[1,1]+tab_nn[2,2])/sum(tab_nn)
Accuracy # [1] 0.6862365

# ---------------------------Non-Neutral Model--------------------------- 
modelPositive <- randomForest(Positive~., data=train_NonNeutral)
# summary(modelPositive)

# importance(modelPositive)
varImpPlot(modelPositive) # plot

predict_pn <- predict(modelPositive, newdata=test_NonNeutral, type="class")

tab_pn <- table(predict_pn, test_NonNeutral$Positive)
Accuracy = (tab_pn[1,1]+tab_pn[2,2])/sum(tab_pn)
Accuracy # [1] 0.823225