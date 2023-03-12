getwd()
setwd("C:/Users/akhil/OneDrive/Documents/Project 3/Hulu/hulu_titles_new")
library(tidyverse)
hulu_sub <- read.csv("hulu_subscribers.csv")
preproshulu_titles <- read.csv("hulu_titles.csv")
head(preproshulu_titles)
hulu_titles <- select(preproshulu_titles, -c(show_id, director,cast, country))

# Extract first 3 characters 
hulu_titles$Mon <- substr(hulu_titles$date_added, 1, 3)

# Specify number of characters to extract 
n_last <- 4

# Extract last four characters 
hulu_titles$Year <- substr(hulu_titles$date_added, 
                           nchar(hulu_titles$date_added) - n_last + 1, 
                           nchar(hulu_titles$date_added))

# Combibe the Month and year to form a modified format
hulu_titles$date_added_mod <- paste(hulu_titles$Mon,
                                    hulu_titles$Year,
                                    sep=", ")

hulu_titles$quartertemp <- ifelse(hulu_titles$Mon == "Jan" | hulu_titles$Mon == "Feb" | hulu_titles$Mon == "Mar", "Q1",
                                 ifelse(hulu_titles$Mon == "Apr" | hulu_titles$Mon == "May" | hulu_titles$Mon == "Jun", "Q2",
                                        ifelse(hulu_titles$Mon == "Jul" | hulu_titles$Mon == "Aug" | hulu_titles$Mon == "Sep", "Q3",
                                               "Q4")))

hulu_titles$Quarter <- paste(hulu_titles$Year,
                             hulu_titles$quartertemp,
                             sep=" ")


hulu_titles <- select(hulu_titles, 
                      -c(date_added, 
                         quartertemp))

hulu_titles$Quarter <- as.Date(hulu_titles$Quarter)
hulu_titles$Mon <- as.Date(hulu_titles$Mon)
hulu_titles$Year <- as.factor(hulu_titles$Year)

library(ggplot2)
Monthdata <- as.data.frame(table(hulu_titles$Mon))
# Basic barplot
p <- ggplot(data=Monthdata, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity",fill = "red")
p + theme(axis.text.x = element_text(angle = 90, hjust = 1))


Yeardata <- as.data.frame(table(hulu_titles$Year))
# Basic barplot
q <- ggplot(data=Yeardata, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity",fill = "magenta")
q + theme(axis.text.x = element_text(angle = 90, hjust = 1))


Quarterdata <- as.data.frame(table(hulu_titles$Quarter))
# Basic barplot
r <- ggplot(data=Quarterdata, aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity",fill = "blue")
r + theme(axis.text.x = element_text(angle = 90, hjust = 1))

hulu_titles$rating <- as.factor(hulu_titles$rating)
summary(hulu_titles$rating)
