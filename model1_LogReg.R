rm(list=ls())

source("modelSubset.R")

# Logistic regression
# ---------------------------Neutral Model--------------------------- 
modelNeutral <- glm(Neutral~0+., data=train_Neutral, family=binomial)
summary(modelNeutral) # AIC: 17588

neutral_pVal <- summary(modelNeutral)$coefficients[,4] #p values
neutral_sigVal <- names(neutral_pVal[neutral_pVal <= 0.05]) #10% level

modelNeutral_2 <- glm(as.formula(
  paste0("Neutral ~ ",
         paste0(neutral_sigVal, collapse = " + "),
         "-1")  
) , data = train_Neutral, family = binomial)
summary(modelNeutral_2) #AIC: 17396

# ---------------------------Non-Neutral Model--------------------------- 
modelPositive <- glm(Positive~0+., data=train_NonNeutral, family=binomial)


positive_pVal <- summary(modelPositive)$coefficients[,4] #p values
positive_sigVal <- names(positive_pVal[positive_pVal <= 0.1]) #10% level

modelPositive_2 <- glm(as.formula(
  paste0("Positive ~ ",
         paste0(positive_sigVal, collapse = " + "),
         "-1")  
) , data = train_NonNeutral, family = binomial)


# ---------------------------Model Predictions--------------------------- 
# ---------------------------Non-Neutral Model---------------------------
predict_pn <- predict( modelPositive_2, test_NonNeutral , type = 'response' ) 

tab_pn <- table(predict_pn>0.53,test_NonNeutral$Positive) #CM
Accuracy = (tab_pn[1,1]+tab_pn[2,2])/sum(tab_pn)
Accuracy # [1] 0.8267831

library(ROCR)
predict_pn1 <- prediction( predict_pn , test_NonNeutral$Positive )
perf_pn <- performance(predict_pn1,x.measure="fpr",measure="tpr")
plot(perf_pn,colorize=T,print.cutoffs.at=c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,0.9,1),text.adj=c(-.02,1.7))


# --------------------------------Neutral Model---------------------------
predict_nn <- predict( modelNeutral_2 , train_Neutral , type = 'response' ) 
tab_nn <- table(predict_nn>0.49,train_Neutral$Neutral) #CM
Accuracy = (tab_nn[1,1]+tab_nn[2,2])/sum(tab_nn)
Accuracy # [1] 0.6950163

predict_nn1 <- prediction( predict_nn , test_Neutral$Neutral )
perf_nn <- performance(predict_pn1,x.measure="fpr",measure="tpr")
plot(perf_nn,colorize=T,print.cutoffs.at=c(0,0.1,0.2,0.3,0.5,0.6,0.7,0.8,0.9,1),text.adj=c(-.02,1.7))