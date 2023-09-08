

## This script is for the cleaning and processing of the data.
## prep_data takes in the string name of the csv file that would be cleaned and processed, 
## and produces the dataframe that would be used for training and testing.

rm(list=ls()) # Clear the environment

library(stringr) 
library(spellcheckr)
library(dplyr)
library(tm)
library(SnowballC)

emo_data <- read.csv("emoticons_long.csv")

# check example
emo_data$Score[ emo_data$Emoticon == "<3" ]

smle <-emo_data$Emoticon[ emo_data$Score == 1 ]
sadd <-emo_data$Emoticon[ emo_data$Score == -1 ]

smle

clean_punc <- function(x){
  
  #extract punctuations
  punc <- str_replace_all(x, "[[:alnum:]]", "" )
  
  save_punc <- data.frame(excl = c(0) , 
                          qsnm = c(0) , 
                          smle = c(0) ,
                          sadd = c(0) ,
                          dott = c(0) )
  # print( "save_punc")
  # print( save_punc )
  
  if( grepl( "!", punc, fixed = TRUE) ){
    # save_punc <- append( save_punc , "!" )
    save_punc$excl[1] <- 1
  }
  
  if( grepl( "?", punc, fixed = TRUE) ){
    save_punc$qsnm[1] <- 1
  }
  
  # to preserve ! and ? first, assuming the links do not contain those characters
  # for removal of website links and url
  words <- strsplit( x , split = "\\s+" )[[1]]
  words <- words[words != ""]
  
  
  # websites characters
  # http .com .net .sg 
  if ( length(words) >= 1 ){
    for ( i in 1:length(words) ){
      
      if( grepl( "http" , words[i] , fixed = TRUE  ) ){
        
        words <- words[ words!= words[i] ]   
        
      }
      if( grepl( ".com" , words[i] , fixed = TRUE  ) ){
        
        words <- words[ words!= words[i] ]   
        
      }
      if( grepl( ".net" , words[i] , fixed = TRUE  ) ){
        
        words <- words[ words!= words[i] ]   
        
      }
      if( grepl( ".sg" , words[i] , fixed = TRUE  ) ){
        
        words <- words[ words!= words[i] ]   
        
      }
      
    }
    
  }
  
  x <- paste( words , collapse = " ")
  punc <- str_replace_all(x, "[[:alnum:]]", "" )
  
  # print( punc )
  # assigning values to smle and sadd based on data from the emoticons dataset
  for( i in 1: length(smle) ){
    # print(smle[i])
    if( grepl(  smle[i] , punc, fixed = TRUE) ){
      save_punc$smle[1] <- 1
      # print("smle")
      # val <- TRUE
      break
    }
  }
  
  for( i in 1: length(sadd) ){
    # print(sadd[i])
    if( grepl(  sadd[i] , punc, fixed = TRUE) ){
      save_punc$sadd[1] <- 1
      # print('sadd')
      # val <- TRUE
      break
    }
  }
  
  
  
  if( grepl( "..", punc, fixed = TRUE) ){
    save_punc$dott[1] <- 1
  }
  
  # extract words with 3 consecutively repeating characters and correct them
  wrong_words <- unlist( str_extract_all(x, "\\p{L}*(\\p{L})\\1\\1+\\p{L}*" ) )
  # print( wrong_words )
  correct_words <- list()
  if ( length( wrong_words) >= 1 ){
    for ( i in 1:length(wrong_words )){
      
      wword <- as.character (wrong_words[i])
      
      wword <-  as.character( gsub('([[:alpha:]])\\1\\1+', '\\1', wword ) )
      
      correct_words[i] <- wword 
      
      correct_words[i] <- correct( correct_words[[i]] )
      
    }
    
  }
  
  
  words <- str_replace_all(x, "[[:punct:]]", "")
  #list of words
  # print(words)
  words <- strsplit( words , split = " " )[[1]]
  # print(words)
  #remove wrong words
  words<-words[ words %in% wrong_words == FALSE]  
  #add in correct words
  # print(words)
  words <- append( words , correct_words )
  # print(words)
  
  words <- words[ words != ""]
  # print(words)
  words <- paste( words, collapse = " ")
  
  out <- list()
  
  out$words <- words
  out$punc <- save_punc 
  
  return(out)
}

str3 <- clean_punc( " yeeeeah .. . and it was in into too  seen Empire top 100 computer games? http://www.empireonline.com/100greatestgames/" )

print( str3$words)

## test code for clean_punc function
# clean_punc( "45 minutes until im off from work. I guess i`ll go back to work in 15 minutes. It`s slow today thank god for one of those days" )
# clean_punc( " yeeeeah .. . and it was in into too  seen Empire top 100 computer games? http://www.empireonline.com/100greatestgames/" )
# clean_punc("i kno look ? :) (: i doooo!!!!!!!!!! yall partyin with out me htttp://www.google.com" )
# clean_punc( "fuckkk i need sleepppppppp lol, happy mothers day mummy" )
# clean_punc("heyheyheyheyehyeyyyyyyyyyyyyyyyy")

#uncomment from here 
# str1

prep_data <- function(x){
  twitter <- read.csv( x ,stringsAsFactors=FALSE)
  str(twitter)  # 2664 observations of 2 variable
  # head(twitter) # First part of the dataframe
  # tail(twitter) # Last part of the dataframe
  summary(twitter) # Summary of the data
  
  # ?VectorSource
  # mtcars[, names(mtcars) != "carb"]          #only works on a single column
  corpus <- Corpus(VectorSource(twitter[,names(twitter) == "tweet" ]))
  # corpus[[1]]
  # as.character(corpus[[1]])
  # corpus[[20610]]
  # as.character(corpus[[20610]])
  # getTransformations()
  # ?tm_map
  
  
  # 1 convert text to lower case and remove stopwords
  # to fix the above error when converting text to lower case
  corpus <- tm_map(corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  corpus <- tm_map(corpus, content_transformer(function(x)    iconv(enc2utf8(x), sub = "bytes")))
  corpus <- tm_map(corpus, content_transformer(tolower))
  # corpus
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  
  
  # 1.5 clean words(remove punctuations right next to works) and extract punctuations
  # this code took 9 hours to load only 50% of the rows from the train data
  punc_df <- data.frame(matrix(ncol = 5, nrow = 0))
  #provide column names
  colnames(punc_df) <- c('excl', 'qsnm', 'smle' , 'sadd' , "dott" )
  
  for( i in 1:length(corpus) ) {
    # for( i in 1:100 ){
    
    # print( paste0(i , " out of " , length(corpus) , " done") )
    as.character( corpus[[ i ]] )
    corpus_test <- corpus[[i]][[1]]
    
    class(str)
    ls <- list()
    ls <- clean_punc( corpus_test )
    
    corpus[[ i ]][[1]] <- ls$words
    
    punc_df <- rbind( punc_df , ls$punc )
    as.character( corpus[[ i ]] )
  }
  
  if( nrow(punc_df) < length( corpus ) ){
    rows_left <-length( corpus ) -nrow(punc_df)
    punc_df <- rbind( punc_df , data.frame( excl = rep( 0 , rows_left ) , 
                                            qsnm = rep( 0 , rows_left ) ,
                                            smle = rep( 0 , rows_left ) ,
                                            sadd = rep( 0 , rows_left ) , 
                                            dott = rep( 0 , rows_left ) ))
  }
  
  punc_df[1,]
  punc_df[10,]
  as.character(corpus[[1]])
  as.character(corpus[[10]])
  
  # as.character( corpus [[26]])
  
  #2 remove stopwords
  corpus <- tm_map(corpus,removeWords,stopwords("english"))
  # And ... Let's check a couple of documents
  
  as.character(corpus[[1]])
  as.character(corpus[[10]])
  
  #3 remove punctuation
  corpus <- tm_map(corpus,removePunctuation)
  # And ... Let's check a couple of documents
  # as.character(corpus[[1]])
  # as.character(corpus[[20610]])
  corpus <- tm_map(corpus,removeNumbers)
  
  
  # #4 Stemming
  corpus <- tm_map(corpus,stemDocument)
  # And ... Let's check a couple of documents
  as.character(corpus[[1]])
  as.character(corpus[[10]])
  
  # corpus_list <- list()
  # for( i in 1:length(corpus) ) {
  #
  #   corpus_list[i] <- as.character( corpus[[i]])
  #
  # }
  
  
  #5 remove sparse terms
  
  dtm <- DocumentTermMatrix(corpus)
  
  if ("sentiment" %in% colnames(twitter)  ){ #means its the test data set
    print("removing sparse terms")
    dtm <- removeSparseTerms(dtm,0.995)
    terms <- dtm[[6]][2]$Terms
    terms
  }
  
  print("save data")
  train_data <- as.data.frame(as.matrix(dtm))
  nrow( punc_df )
  nrow( train_data )
  
  
  train_data <- cbind( train_data , punc_df )
  
  
  # punc_df$excl
  # train_data$excl
  
  out<- list()
  if ("sentiment" %in% colnames(twitter)  ){
    out$sentiment <- twitter$sentiment-2
  }
  
  out$data <- train_data
  
  return( out )
  
  
}

