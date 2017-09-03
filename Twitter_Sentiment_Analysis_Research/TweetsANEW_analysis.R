######################################################################
# Tweet for Democracy (2014) sample research script                  #
# ANEW word analysis by state                                        #
# All tweets analyzed used relevant Election 2012 hashtags           #
# Note: Tweets were obtained from GNip at this time from 10/31/2012  #
# to 11/5/2012                                                       #
######################################################################


#loading in the needed libraries
library(plyr)
library(stringr)
library(e1071)

# load ANEW word polarity list and format it
# Section specifies the specific ANEW word list for reference to one of the three sentiment categories 
# [1] Dominance(extent of feeling as though one can effect change)
# [2] Valence (extent of feeling positive or negatively)
# [3] Arousal (extent of feeling a sense of urgancy)

#This script refers to the ANEW arousal(urgency) word list
anew_list <- read.delim("ANEWaro.csv", header=F,sep=",", stringsAsFactors=FALSE)
names(anew_list) <- c('word', 'score')
anew_list$word <- tolower(anew_list$word)   

# Create variables that store each word with its sentiment score
Terms9 <- anew_list$word[anew_list$score==9]
Terms8 <- anew_list$word[anew_list$score==8]
Terms7 <- anew_list$word[anew_list$score==7]
Terms6 <- anew_list$word[anew_list$score==6]
Terms5 <- anew_list$word[anew_list$score==5]
Terms4 <- anew_list$word[anew_list$score==4]
Terms3 <- anew_list$word[anew_list$score==3] 
Terms2 <- anew_list$word[anew_list$score==2] 
Terms1 <- anew_list$word[anew_list$score==1] 


#Load in tweets data (different .csv files were created for each state)
# Note: All operations and tweets could be loaded via one script --and can be done in replicated studies 

# In this case, Wyoming tweets were examined
Tweets<- read.delim(file='Tweetsbystate/NYtweets.csv', header=FALSE, sep=',', stringsAsFactors=FALSE)
Tweets<- Tweets$V3
#Tweets<- unlist(lapply(Tweets)) #function(x) { str_split(x, ",") }))  
Tweets<- lapply(Tweets)

#function to calculate number of words in each category within a sentence, the overall sentiment score in each tweet & the mean sentiment score. 
sentimentScore <- function(sentences, Terms9, Terms8, Terms7,Terms6, Terms5, Terms4, Terms3, Terms2, Terms1,OverallScore)
  {
  final_scores <- matrix('', 0, 11)
  scores <- laply(sentences, function(sentence, Terms9, Terms8, Terms7,Terms6, Terms5, Terms4, Terms3, Terms2, Terms1,OverallScore)
    {
    initial_sentence <- sentence

    #remove unnecessary characters and split by word 
    sentence <- gsub('http://t.co/\\S+','', sentence)
    sentence <- gsub('[[:punct:]]', '', sentence)
    sentence <- gsub('[[:cntrl:]]', '', sentence)
    sentence <- gsub('#\\S+','', sentence)
    sentence <- gsub('# \\S+','', sentence)
    sentence <- gsub('@\\S+','', sentence)
    sentence <- gsub('@ \\S+','', sentence)
    sentence <- gsub('\\d+', '', sentence)
    
    sentence <- tolower(sentence)
    wordList <- str_split(sentence, '\\s+')
    words <- unlist(wordList)
    
    Matches9 <- match(words, Terms9)
    Matches8 <- match(words, Terms8)
    Matches7 <- match(words, Terms7)
    Matches6 <- match(words, Terms6)
    Matches5 <- match(words, Terms5)
    Matches4 <- match(words, Terms4)
    Matches3 <- match(words, Terms3)
    Matches2 <- match(words, Terms2)
    Matches1 <- match(words, Terms1)
    
    
    #Sum the number of words in each category
    Matches9 <- sum(!is.na(Matches9))
    Matches8 <- sum(!is.na(Matches8))
    Matches7 <- sum(!is.na(Matches7))
    Matches6 <- sum(!is.na(Matches6))
    Matches5 <- sum(!is.na(Matches5))
    Matches4 <- sum(!is.na(Matches4))
    Matches3 <- sum(!is.na(Matches3))
    Matches2 <- sum(!is.na(Matches2))
    Matches1 <- sum(!is.na(Matches1))
 
    
    #designate values for matches
    TVal9 <- Matches9*9
    TVal8 <- Matches8*8
    TVal7 <- Matches7*7
    TVal6 <- Matches6*6
    TVal5 <- Matches5*5
    TVal4 <- Matches4*4
    TVal3 <- Matches3*3
    TVal2 <- Matches2*2
    TVal1 <- Matches1*1
   
    
    #sum sentiment score for each tweet
    OverallScore <- sum(TVal9,TVal8, TVal7, TVal6,TVal5,TVal4,TVal3,TVal2,TVal1)
    
    score <- c(Matches9,Matches8,Matches7,Matches6,Matches5,Matches4,Matches3,Matches2,Matches1, OverallScore)
    
    #add row to scores table
    newrow <- c(initial_sentence, score)
    
    
    final_scores <- rbind(final_scores, newrow)
    return(final_scores)
  }, Terms9, Terms8, Terms7,Terms6, Terms5, Terms4, Terms3, Terms2, Terms1,OverallScore)
  return(scores)
}    

#Build tables of positive and negative sentences with scores
Result <- data.frame(sentimentScore(Tweets, Terms9, Terms8, Terms7,Terms6, Terms5, Terms4, Terms3, Terms2, Terms1,OverallScore))
Result <- cbind(Result)
colnames(Result) <- c('Tweets', 'Terms9', 'Terms8', 'Terms7','Terms6', 'Terms5', 'Terms4', 'Terms3', 'Terms2', 'Terms1','Sentiment')

#Check the structure of the dataframe.
#str(Result)

#changed the col Sentiment to numeric
sent <- as.numeric(as.character(Result[[11]]))

#Mean value of Overall_Sentiment across all tweets for the tweets of the specific state as indicated by the
# specific tweets .csv file

#In this case, the New York state tweets file (Tweetsbystate/NYtweets.csv)
Overall_Sentiment <- summary(sent)

#Veiw the NY object
Overall_Sentiment
