rm(list=ls())

source("modelSubset.R")

library(randomForest)

# Logistic regression and Random Forest
# ---------------------------Neutral Model--------------------------- 
modelNeutral <- glm(Neutral~0+., data=train_Neutral, family=binomial)
summary(modelNeutral)

neutral_pVal <- summary(modelNeutral)$coefficients[,4] #p values
neutral_sigVal <- names(neutral_pVal[neutral_pVal <= 0.01]) #1% level

# length(neutral_sigVal)

modelNeutral_2 <- randomForest(as.formula(
  paste0("Neutral ~ ",
         paste0(neutral_sigVal, collapse = " + "),
         "-1")  
) , data = train_Neutral)

# summary(modelNeutral_2)
# importance(modelNeutral)
varImpPlot(modelNeutral_2) # plot

predict_nn <- predict(modelNeutral_2, newdata=test_Neutral, type="class")
tab_nn <- table(predict_nn,test_Neutral$Neutral)
Accuracy = (tab_nn[1,1]+tab_nn[2,2])/sum(tab_nn)
# Accuracy # [1] 0.690765 # Higher than earlier model


# ---------------------------Non-Neutral +ve Model--------------------------- 
modelPositive <- glm(Positive~0+., data=train_NonNeutral, family=binomial)

positive_pVal <- summary(modelPositive)$coefficients[,4] #p values
positive_sigVal <- names(positive_pVal[positive_pVal <= 0.01]) #1% level

modelPositive_2 <- randomForest(as.formula(
  paste0("Positive ~ ",
         paste0(positive_sigVal, collapse = " + "),
         "-1")  
) , data = train_NonNeutral)

varImpPlot(modelPositive_2) # plot

predict_pn <- predict(modelPositive_2, newdata=test_NonNeutral, type="class")

tab_pn <- table(predict_pn, test_NonNeutral$Positive)
Accuracy = (tab_pn[1,1]+tab_pn[2,2])/sum(tab_pn)
# Accuracy # [1] 0.8274301 # Higher than earlier model

