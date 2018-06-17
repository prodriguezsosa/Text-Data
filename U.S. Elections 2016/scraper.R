###########################################
# Title: scrape US presidential text data
# Date begun: 10-09-2017
# Date last modified: 10-09-2017
# Author: Pedro L. Rodriguez
############################################

rm(list=ls())
library(rvest)
library(XML)
library(quanteda)
library(zoo)
library(qdap)
setwd("/Users/pedrorodriguez/Dropbox/Research/SentimentAnalysis/Data/US/")

##################################
#
#
# PRESIDENTIAL DEBATES 2016
#
#
##################################

# download links to debates
url <- "http://www.presidency.ucsb.edu/debates.php"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)
# keep only the debates for 2016 (main debates)
links <- unname(links[46:71])

# clean debates
debate_texts <- list()
for(i in 1:length(links)){
  # specify the url of the website to be scraped
  url <- links[i]
  # read the HTML code from the website
  webpage <- read_html(url)
  # extract the text
  text_html <- html_nodes(webpage,'span p')
  # convert to text
  text <- html_text(text_html)
  # locate speakers
  speakers <- regexpr(":", text, fixed = TRUE)
  speakers <- ifelse(speakers >0 & speakers <25, 1, 0)
  debate_text <- data.frame(textID = i, speaker = NA, text = text, stringsAsFactors = FALSE)
  debate_text$speaker <- sapply(strsplit(text,":"), `[`, 1)
  debate_text$speaker[speakers==0] <- NA
  debate_text$speaker <- na.locf(debate_text$speaker,na.rm=FALSE)
  # extract speaker from text
  speakers_to_extract <- paste(unique(debate_text$speaker), ":", sep ="")
  debate_text$text <- mgsub(speakers_to_extract, "", debate_text$text)
  debate_texts[[i]] <- debate_text
  rm(speakers, debate_text, speakers_to_extract, url, webpage, text_html, text)
}

##################################
#
#
# HILLARY CLINTON SPEECHES
#
#
##################################

# download links to speeches
url <- "http://www.presidency.ucsb.edu/2016_election_speeches.php?candidate=70&campaign=2016CLINTON&doctype=5000"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)
# keep only sppeches
links <- unname(links[46:241])
# fix shortened links
links <- gsub("\\.\\.", "http://www.presidency.ucsb.edu/", links)

# clean debates
clinton_texts <- list()
for(i in 1:length(links)){
  # specify the url of the website to be scraped
  url <- links[i]
  # read the HTML code from the website
  webpage <- read_html(url)
  # extract the text
  text_html <- html_nodes(webpage,'span p')
  # convert to text
  text <- html_text(text_html)
  # locate speakers
  speakers <- regexpr(":", text, fixed = TRUE)
  speakers <- ifelse(speakers >0 & speakers <25, 1, 0)
  clinton_text <- data.frame(textID = i, speaker = NA, text = text, stringsAsFactors = FALSE)
  clinton_text$speaker <- sapply(strsplit(text,":"), `[`, 1)
  clinton_text$speaker[speakers==0] <- NA
  clinton_text$speaker <- na.locf(clinton_text$speaker,na.rm=FALSE)
  # extract speaker from text
  speakers_to_extract <- paste(unique(clinton_text$speaker), ":", sep ="")
  clinton_text$text <- mgsub(speakers_to_extract, "", clinton_text$text)
  clinton_texts[[i]] <- clinton_text
  rm(speakers, clinton_text, speakers_to_extract, url, webpage, text_html, text)
}

##################################
#
#
# TRUMP SPEECHES
#
#
##################################


# download links to speeches
url <- "http://www.presidency.ucsb.edu/2016_election_speeches.php?candidate=45&campaign=2016TRUMP&doctype=5000"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)
# keep only sppeches
links <- unname(links[46:119])
# fix shortened links
links <- gsub("\\.\\.", "http://www.presidency.ucsb.edu/", links)

# clean debates
trump_texts <- list()
for(i in 1:length(links)){
  # specify the url of the website to be scraped
  url <- links[i]
  # read the HTML code from the website
  webpage <- read_html(url)
  # extract the text
  text_html <- html_nodes(webpage,'span p')
  # convert to text
  text <- html_text(text_html)
  # locate speakers
  speakers <- regexpr(":", text, fixed = TRUE)
  speakers <- ifelse(speakers >0 & speakers <25, 1, 0)
  trump_text <- data.frame(textID = i, speaker = NA, text = text, stringsAsFactors = FALSE)
  trump_text$speaker <- sapply(strsplit(text,":"), `[`, 1)
  trump_text$speaker[speakers==0] <- NA
  trump_text$speaker <- na.locf(trump_text$speaker,na.rm=FALSE)
  # extract speaker from text
  speakers_to_extract <- paste(unique(trump_text$speaker), ":", sep ="")
  trump_text$text <- mgsub(speakers_to_extract, "", trump_text$text)
  trump_texts[[i]] <- trump_text
  rm(speakers, trump_text, speakers_to_extract, url, webpage, text_html, text)
}

##################################
#
#
# SANDERS SPEECHES
#
#
##################################


# download links to speeches
url <- "http://www.presidency.ucsb.edu/2016_election_speeches.php?candidate=107&campaign=2016SANDERS&doctype=5000"
doc <- htmlParse(url)
links <- xpathSApply(doc, "//a/@href")
free(doc)
# keep only sppeches
links <- unname(links[46:105])
# fix shortened links
links <- gsub("\\.\\.", "http://www.presidency.ucsb.edu/", links)

# clean debates
sanders_texts <- list()
for(i in 1:length(links)){
  # specify the url of the website to be scraped
  url <- links[i]
  # read the HTML code from the website
  webpage <- read_html(url)
  # extract the text
  text_html <- html_nodes(webpage,'span p')
  # convert to text
  text <- html_text(text_html)
  # locate speakers
  speakers <- regexpr(":", text, fixed = TRUE)
  speakers <- ifelse(speakers >0 & speakers <25, 1, 0)
  sanders_text <- data.frame(textID = i, speaker = NA, text = text, stringsAsFactors = FALSE)
  sanders_text$speaker <- sapply(strsplit(text,":"), `[`, 1)
  sanders_text$speaker[speakers==0] <- NA
  sanders_text$speaker <- na.locf(sanders_text$speaker,na.rm=FALSE)
  # extract speaker from text
  speakers_to_extract <- paste(unique(sanders_text$speaker), ":", sep ="")
  sanders_text$text <- mgsub(speakers_to_extract, "", sanders_text$text)
  sanders_texts[[i]] <- sanders_text
  rm(speakers, sanders_text, speakers_to_extract, url, webpage, text_html, text)
}

# save data
save(debate_texts, file = "debate_texts.RData")
save(trump_texts, file = "trump_texts.RData")
save(clinton_texts, file = "clinton_texts.RData")
save(sanders_texts, file = "sanders_texts.RData")
