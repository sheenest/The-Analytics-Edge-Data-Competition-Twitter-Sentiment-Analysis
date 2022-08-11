rm(list=ls())
source("tweet_model3.R")

# --------------------------- Model Execution on Test Data Set --------------------------- 
test_data <- read.csv("test_data.csv")
# format test to match the same columns as train
# num1 is the number of words which are the train data but not in the test data,
# add those words to test data

ncol(train)-4

num1 <- 0
for( i in 1:ncol(train[ 1: ( ncol(train)-4) ]) ){
  if ( ! (colnames(train)[i] %in% colnames(test_data) ) ){
    
    new_col <- colnames(train)[i]
    test_data[,new_col]<- 0
    # print( new_col )
    # test_data$new_col<- 0
    num1<-num1+1
  }
  
}
num1
# 0

test_data

# num2 is the number of words in the test data that were not in the training data
# remove those wirds from test data
num2<-length( ( colnames(test_data) %in% colnames(train)==TRUE ) )
test_data <- test_data[ ,  (colnames(test_data) %in% colnames(train)) ]
num2
# 8805

test_data$id <- 1:nrow(test_data)
test_data

predict_neutral <- predict( modelNeutral_2 , newdata = test_data , type = 'response' )
predict_neutral

test_data$Neutral <- as.factor( predict_neutral )
# test_data$Neutral

neutral <- test_data[ test_data$Neutral == TRUE  , ]
non_neutral <- test_data[ test_data$Neutral == FALSE  , ]

nrow(neutral)
nrow(non_neutral)
ncol( non_neutral )

colnames(non_neutral)
positive_sigVal

length( positive_sigVal )

sig_col <- non_neutral[ colnames(non_neutral) %in% positive_sigVal ]
sig_col$id <- non_neutral$id
sig_col$id
nrow( sig_col )
ncol( sig_col )

filter_neutral <- sig_col[ rowSums( sig_col[ 1: (ncol(sig_col)-1) ] ) == 0   ,  ]
nrow( filter_neutral )

filter_non_neutral <- sig_col[ rowSums( sig_col[ 1: (ncol(sig_col)-1) ] ) > 0   ,  ]
nrow( filter_non_neutral )

filter_non_neutral$id
nrow( filter_non_neutral )

filter_neutral$Neutral = TRUE
filter_neutral$Neutral

colnames(filter_neutral)
colnames( neutral )


filter_neutral <- filter_neutral[ , c("id" , 'Neutral')]
length(filter_neutral)
neutral <- neutral[ , c("id" , 'Neutral') ]


neutral <- rbind( neutral , filter_neutral )
nrow( neutral )
neutral$id


filter_non_neutral
predict_positive <- predict( modelPositive_2 , newdata = filter_non_neutral , type = 'response' )

filter_non_neutral$Positive <- as.factor( predict_positive )
positive <- filter_non_neutral[ filter_non_neutral$Positive == TRUE ,]
negative <- filter_non_neutral[ filter_non_neutral$Positive == FALSE ,]


neutral$id
positive$id
negative$id

nrow(positive)
nrow(neutral)
nrow(negative)

nrow( test_data ) - nrow(positive) - nrow(neutral) - nrow(negative) #run 

for ( i in 1:nrow(test_data) ){
  
  if( i %in% neutral$id ){
    test_data$sentiment[i] = 2
  }
  
  else if( i %in% positive$id ){
    test_data$sentiment[i] = 3
  }
  
  else if ( i %in% negative$id ){
    test_data$sentiment[i] = 1
  }
  
}

test_data$sentiment

out <- data.frame( id = 1:nrow(test_data) ,
                   sentiment = test_data$sentiment )

write.csv( out,"tweet_predictions.csv", row.names = FALSE)
