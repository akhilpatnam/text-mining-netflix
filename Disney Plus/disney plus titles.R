getwd()
setwd("C:/Users/akhil/OneDrive/Documents/Project 3/Disney Plus")

#disney_plus titles
disney_plus_titles_1=read.csv("disney_plus_titles.csv")

disney_plus_titles=read.csv("disney_plus_titles.csv")

summary(as.factor(disney_plus_titles$typeOfPeriod))

################################################################################################################################################
## TEXT MINING ####FOR GOOD TITLES DISNEY PLUS##################################################################################################
################################################################################################################################################
disney_plus_titles_good <- subset(disney_plus_titles, typeOfPeriod == "good") 
disney_plus_precorpus1 <- subset(disney_plus_titles_good,select = c(show_id,listed_in))
colnames(disney_plus_precorpus1) <- c("ID","TitleType")

disney_plus_precorpus1$TitleType <- gsub("'", "", disney_plus_precorpus1$TitleType) # remove apostrophes
disney_plus_precorpus1$TitleType <- gsub("[[:punct:]]", " ", disney_plus_precorpus1$TitleType)  # replace punctuation with space
disney_plus_precorpus1$TitleType <- gsub("[[:cntrl:]]", " ", disney_plus_precorpus1$TitleType)  # replace control characters with space
disney_plus_precorpus1$TitleType <- gsub("^[[:space:]]+", "", disney_plus_precorpus1$TitleType) # remove whitespace at beginning of documents
disney_plus_precorpus1$TitleType <- gsub("[[:space:]]+$", "", disney_plus_precorpus1$TitleType) # remove whitespace at end of documents
disney_plus_precorpus1$TitleType <- gsub("[^a-zA-Z -]", " ", disney_plus_precorpus1$TitleType) # allows only letters
disney_plus_precorpus1$TitleType <- tolower(disney_plus_precorpus1$TitleType)  # force to lowercase
head(disney_plus_precorpus1$TitleType)
library(quanteda)
disney_plus_dfm.simple1<- dfm(disney_plus_precorpus1$TitleType,
                          remove = stopwords("english"),
                          verbose=TRUE,
                          stem=FALSE)

topfeatures(disney_plus_dfm.simple1, n=50)


# create a custom dictonary
disney_plus_swlist1 = c("age","varieti*","show","cook","docuseri*","spi*","seri*","medic","come","espionag*","soap")

disney_plus_dfm1<- dfm(disney_plus_precorpus1$TitleType,
                   remove = c(disney_plus_swlist1,stopwords("english")),
                   verbose=TRUE,
                   stem=FALSE)

topfeatures(disney_plus_dfm1, n=100)

topfeatures_good <- as.data.frame(topfeatures(disney_plus_dfm1, n=100))
write.csv(topfeatures_good,"topfeatures_gooddisney.csv")


#update for bigrans using tokens
disney_plus_toks.1<-tokens(disney_plus_precorpus1$TitleType)   #creates tokens
disney_plus_toks.2<-tokens_remove(disney_plus_toks.1, stopwords("english"))  #remove stopwords from tokens
disney_plus_toks.3 <-tokens_ngrams(disney_plus_toks.2, n=2) # ngram =2
disney_plus_dfm.ngram1<- dfm(disney_plus_toks.3, verbose=TRUE)

topfeatures(disney_plus_dfm.ngram1, n=100)


#########################
### WORD CLOUD ########
#########################
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
disney_plus_freq1 <-topfeatures(disney_plus_dfm1, n=500)

wordcloud(names(disney_plus_freq1), 
          disney_plus_freq1, max.words=500, 
          scale=c(6, .6), 
          colors="#153866")

#################################################################################################################################################
## Text mining ####For BAD Titles disney_plus####################################################################################################
#################################################################################################################################################
disney_plus_titles_bad <- subset(disney_plus_titles, typeOfPeriod == "poor") 
disney_plus_precorpus2 <- subset(disney_plus_titles_bad,select = c(show_id,listed_in))
colnames(disney_plus_precorpus2) <- c("ID","TitleType")

disney_plus_precorpus2$TitleType <- gsub("'", "", disney_plus_precorpus2$TitleType) # remove apostrophes
disney_plus_precorpus2$TitleType <- gsub("[[:punct:]]", " ", disney_plus_precorpus2$TitleType)  # replace punctuation with space
disney_plus_precorpus2$TitleType <- gsub("[[:cntrl:]]", " ", disney_plus_precorpus2$TitleType)  # replace control characters with space
disney_plus_precorpus2$TitleType <- gsub("^[[:space:]]+", "", disney_plus_precorpus2$TitleType) # remove whitespace at beginning of documents
disney_plus_precorpus2$TitleType <- gsub("[[:space:]]+$", "", disney_plus_precorpus2$TitleType) # remove whitespace at end of documents
disney_plus_precorpus2$TitleType <- gsub("[^a-zA-Z -]", " ", disney_plus_precorpus2$TitleType) # allows only letters
disney_plus_precorpus2$TitleType <- tolower(disney_plus_precorpus2$TitleType)  # force to lowercase
head(disney_plus_precorpus2$TitleType)
library(quanteda)
disney_plus_dfm.simple2<- dfm(disney_plus_precorpus2$TitleType,
                          remove = stopwords("english"),
                          verbose=TRUE,
                          stem=FALSE)

topfeatures(disney_plus_dfm.simple2, n=50)


# create a custom dictonary
disney_plus_swlist2 = c("science","varieti*","spi*","show*","surviv*","film*","movi*","seri*","soap*","polic*","cop*","age","docuseri*")

disney_plus_dfm2<- dfm(disney_plus_precorpus2$TitleType,
                   remove = c(disney_plus_swlist2,stopwords("english")),
                   verbose=TRUE,
                   stem=FALSE)

topfeatures(disney_plus_dfm2, n=70)


topfeatures_bad <- as.data.frame(topfeatures(disney_plus_dfm2, n=100))
write.csv(topfeatures_bad,"topfeatures_baddisney.csv")



#update for bigrans using tokens
disney_plus_toks2.1<-tokens(disney_plus_precorpus2$TitleType)   #creates tokens
disney_plus_toks2.2<-tokens_remove(disney_plus_toks2.1, stopwords("english"))  #remove stopwords from tokens
disney_plus_toks2.3 <-tokens_ngrams(disney_plus_toks2.2, n=2) # ngram =2
disney_plus_dfm.ngram2<- dfm(disney_plus_toks2.3, verbose=TRUE)

topfeatures(disney_plus_dfm.ngram2, n=100)


#########################
### WORD CLOUD ########
#########################
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
disney_plus_freq2 <-topfeatures(disney_plus_dfm2, n=500)

wordcloud(names(disney_plus_freq2), 
          disney_plus_freq2, max.words=500, 
          scale=c(6, .6), 
          colors="#153866")

###########################################################################################################################################
## Text mining ####For NEUTRAL Titles disney_plus##########################################################################################
###########################################################################################################################################
disney_plus_titles_neutral <- subset(disney_plus_titles, typeOfPeriod == "neutral") 
disney_plus_precorpus3 <- subset(disney_plus_titles_neutral,select = c(show_id,listed_in,date_added))
colnames(disney_plus_precorpus3) <- c("ID","TitleType")

disney_plus_precorpus3$TitleType <- gsub("'", "", disney_plus_precorpus3$TitleType) # remove apostrophes
disney_plus_precorpus3$TitleType <- gsub("[[:punct:]]", " ", disney_plus_precorpus3$TitleType)  # replace punctuation with space
disney_plus_precorpus3$TitleType <- gsub("[[:cntrl:]]", " ", disney_plus_precorpus3$TitleType)  # replace control characters with space
disney_plus_precorpus3$TitleType <- gsub("^[[:space:]]+", "", disney_plus_precorpus3$TitleType) # remove whitespace at beginning of documents
disney_plus_precorpus3$TitleType <- gsub("[[:space:]]+$", "", disney_plus_precorpus3$TitleType) # remove whitespace at end of documents
disney_plus_precorpus3$TitleType <- gsub("[^a-zA-Z -]", " ", disney_plus_precorpus3$TitleType) # allows only letters
disney_plus_precorpus3$TitleType <- tolower(disney_plus_precorpus3$TitleType)  # force to lowercase
head(disney_plus_precorpus3$TitleType)
library(quanteda)
disney_plus_dfm.simple3<- dfm(disney_plus_precorpus3$TitleType,
                          remove = stopwords("english"),
                          verbose=TRUE,
                          stem=FALSE)

topfeatures(disney_plus_dfm.simple3, n=50)


# create a custom dictonary
disney_plus_swlist3 = c("tv","movi","show","docuseri","fi","classic","seri","show","featur","talk","come*")

disney_plus_dfm3<- dfm(disney_plus_precorpus3$TitleType,
                   remove = c(disney_plus_swlist3,stopwords("english")),
                   verbose=TRUE,
                   stem=FALSE)

topfeatures(disney_plus_dfm3, n=70)


disney_plus_dfm.stem3<- dfm(disney_plus_precorpus3$TitleType,
                        remove = c(disney_plus_swlist3,stopwords("english")),
                        verbose=TRUE,
                        stem=TRUE)

data.frame(topfeatures(disney_plus_dfm.stem3, n=50))

#update for bigrans using tokens
disney_plus_toks3.1<-tokens(disney_plus_precorpus3$TitleType)   #creates tokens
disney_plus_toks3.2<-tokens_remove(disney_plus_toks3.1, stopwords("english"))  #remove stopwords from tokens
disney_plus_toks3.3 <-tokens_ngrams(disney_plus_toks3.2, n=2) # ngram =2
disney_plus_dfm.ngram3<- dfm(disney_plus_toks3.3, verbose=TRUE)

topfeatures(disney_plus_dfm.ngram3, n=100)


#########################
### WORD CLOUD ########
#########################
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed  
disney_plus_freq3 <-topfeatures(disney_plus_dfm.stem3, n=500)

wordcloud(names(disney_plus_freq3), 
          disney_plus_freq3, max.words=500, 
          scale=c(6, .6), 
          colors="#153866")
