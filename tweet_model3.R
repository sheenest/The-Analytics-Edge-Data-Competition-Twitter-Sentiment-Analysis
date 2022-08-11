rm(list=ls())
source("modelSubset.R")
library(randomForest)

# Logistic regression and Random Forest
# ---------------------------Neutral Model--------------------------- 
modelNeutral <- glm(Neutral~0+., data=Neutral, family=binomial)

neutral_pVal <- summary(modelNeutral)$coefficients[,4] #p values
neutral_sigVal <- names(neutral_pVal[neutral_pVal <= 0.05]) #5% level
# length(neutral_sigVal) 

modelNeutral_2 <- randomForest(as.formula(
  paste0("Neutral ~ ",
         paste0(neutral_sigVal, collapse = " + "),
         "-1")  
) , data = Neutral)

# importance(modelNeutral_2)
# varImpPlot(modelNeutral_2)

# ---------------------------Non-Neutral +ve Model--------------------------- 
modelPositive <- glm(Positive~0+., data=NonNeutral, family=binomial)

positive_pVal <- summary(modelPositive)$coefficients[,4] #p values
positive_sigVal <- names(positive_pVal[positive_pVal <= 0.1]) #10% level
# length(positive_sigVal) # [1] 152

modelPositive_2 <- randomForest(as.formula(
  paste0("Positive ~ ",
         paste0(positive_sigVal, collapse = " + "),
         "-1")  
) , data = NonNeutral)

# importance(modelPositive)
# varImpPlot(modelPositive_2) 