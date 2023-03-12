library(tidyverse)
install.packages("stringr")  # Install & load stringr
library("stringr")

getwd()
setwd("C:/Users/akhil/OneDrive/Documents/Project 3/Hulu/hulu_titles_new")


#hulu titles
hulu_titles_1=read.csv("hulu_titles.csv")

hulu_titles=read.csv("hulu_titles.csv")


###############################################################################################################################
## Text mining ####For GOOD Titles HULU########################################################################################
###############################################################################################################################
hulu_titles_good <- subset(hulu_titles, typeOfPeriod == "good") 
hulu_precorpus1 <- subset(hulu_titles_good,select = c(show_id,listed_in))
colnames(hulu_precorpus1) <- c("ID","TitleType")

hulu_precorpus1$TitleType <- gsub("'", "", hulu_precorpus1$TitleType) # remove apostrophes
hulu_precorpus1$TitleType <- gsub("[[:punct:]]", " ", hulu_precorpus1$TitleType)  # replace punctuation with space
hulu_precorpus1$TitleType <- gsub("[[:cntrl:]]", " ", hulu_precorpus1$TitleType)  # replace control characters with space
hulu_precorpus1$TitleType <- gsub("^[[:space:]]+", "", hulu_precorpus1$TitleType) # remove whitespace at beginning of documents
hulu_precorpus1$TitleType <- gsub("[[:space:]]+$", "", hulu_precorpus1$TitleType) # remove whitespace at end of documents
hulu_precorpus1$TitleType <- gsub("[^a-zA-Z -]", " ", hulu_precorpus1$TitleType) # allows only letters
hulu_precorpus1$TitleType <- tolower(hulu_precorpus1$TitleType)  # force to lowercase
head(hulu_precorpus1$TitleType)
library(quanteda)
hulu_dfm.simple1<- dfm(hulu_precorpus1$TitleType,
                  remove = stopwords("english"),
                  verbose=TRUE,
                  stem=FALSE)

topfeatures(hulu_dfm.simple1, n=50)


# create a custom dictonary
hulu_swlist1 = c("show","health","up*")

hulu_dfm1<- dfm(hulu_precorpus1$TitleType, 
                remove = hulu_swlist1, 
                verbose=TRUE,
                stem=FALSE)

topfeatures(hulu_dfm1, n=50)

topfeatures_goodhulu <- as.data.frame(topfeatures(hulu_dfm1, n=100))
write.csv(topfeatures_goodhulu,"topfeatures_goodhulu.csv")

#update for bigrans using tokens
hulu_toks.1<-tokens(hulu_precorpus1$TitleType)   #creates tokens
hulu_toks.2<-tokens_remove(hulu_toks.1, stopwords("english"))  #remove stopwords from tokens
hulu_toks.3 <-tokens_ngrams(hulu_toks.2, n=2) # ngram =2
hulu_dfm.ngram1<- dfm(hulu_toks.3, verbose=TRUE)

topfeatures(hulu_dfm.ngram1, n=100)


#########################
### WORD CLOUD ########
#########################
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
hulu_freq1 <-topfeatures(hulu_dfm1, n=500)

wordcloud(names(hulu_freq1), 
          hulu_freq1, max.words=500, 
          scale=c(6,0.6), 
          colors="#66aa33")

##########################################################################################################################
## Text mining ####For BAD Titles HULU####################################################################################
##########################################################################################################################
hulu_titles_bad <- subset(hulu_titles, typeOfPeriod == "poor") 
hulu_precorpus2 <- subset(hulu_titles_bad,select = c(show_id,listed_in,date_added))
colnames(hulu_precorpus2) <- c("ID","TitleType")

hulu_precorpus2$TitleType <- gsub("'", "", hulu_precorpus2$TitleType) # remove apostrophes
hulu_precorpus2$TitleType <- gsub("[[:punct:]]", " ", hulu_precorpus2$TitleType)  # replace punctuation with space
hulu_precorpus2$TitleType <- gsub("[[:cntrl:]]", " ", hulu_precorpus2$TitleType)  # replace control characters with space
hulu_precorpus2$TitleType <- gsub("^[[:space:]]+", "", hulu_precorpus2$TitleType) # remove whitespace at beginning of documents
hulu_precorpus2$TitleType <- gsub("[[:space:]]+$", "", hulu_precorpus2$TitleType) # remove whitespace at end of documents
hulu_precorpus2$TitleType <- gsub("[^a-zA-Z -]", " ", hulu_precorpus2$TitleType) # allows only letters
hulu_precorpus2$TitleType <- tolower(hulu_precorpus2$TitleType)  # force to lowercase
head(hulu_precorpus2$TitleType)
library(quanteda)
hulu_dfm.simple2<- dfm(hulu_precorpus2$TitleType,
                  remove = stopwords("english"),
                  verbose=TRUE,
                  stem=FALSE)

topfeatures(hulu_dfm.simple2, n=50)


# create a custom dictonary
hulu_swlist2 = c("stori*","food","show","night","health","technolog*","sketch")

hulu_dfm2<- dfm(hulu_precorpus2$TitleType,
           remove = c(hulu_swlist2,stopwords("english")),
           verbose=TRUE,
           stem = FALSE)

topfeatures(hulu_dfm2, n=70)

topfeatures_badhulu <- as.data.frame(topfeatures(hulu_dfm2, n=100))
write.csv(topfeatures_badhulu,"topfeatures_badhulu.csv")

#update for bigrans using tokens
hulu_toks2.1<-tokens(hulu_precorpus2$TitleType)   #creates tokens
hulu_toks2.2<-tokens_remove(hulu_toks2.1, stopwords("english"))  #remove stopwords from tokens
hulu_toks2.3 <-tokens_ngrams(hulu_toks2.2, n=2) # ngram =2
hulu_dfm.ngram2<- dfm(hulu_toks2.3, verbose=TRUE)

topfeatures(hulu_dfm.ngram2, n=100)


#########################
### WORD CLOUD ########
#########################
library(wordcloud)
set.seed(142)   #keeps cloud' shape fixed
hulu_freq2 <-topfeatures(hulu_dfm2, n=500)

wordcloud(names(hulu_freq2), 
          hulu_freq2, max.words=500, 
          scale=c(6, .6), 
          colors="#66aa33")