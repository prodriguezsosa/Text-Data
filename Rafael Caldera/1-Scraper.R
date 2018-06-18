rm(list=ls())
#====================================
# LOAD LIBRARIES
#====================================
library(rvest)
library(XML)
library(stringr)
library(data.table)

#====================================
# SCRAPE LINKS
#====================================
# download links to discourses
url <- "http://rafaelcaldera.com/categoria-biblioteca/discursos/" # base url
doc <- htmlParse(url)
links <- xpathSApply(doc, "//h3//a/@href") # extract only links with h3 titles (does away with all other irrelevant links in the page)
free(doc)

#====================================
# SCRAPE EACH LINK
#====================================
caldera_corpus <- data.table() 
for(i in 1:length(links)){
  # specify the url of the website to be scraped
  url <- links[i]
  # read the HTML code from the website
  webpage <- read_html(url)
  # extract the text
  text_html <- html_nodes(webpage,'p')
  # convert to text
  text <- html_text(text_html)
  # remove non-text entries
  text <- text[grepl("[A-z]", text)]
  # remove excess white space
  text <- str_replace_all(text, "^ +| +$|( ) +", "\\1") 
  # collapse
  text <- paste(text, collapse = " ")
  # extract title and description
  html <- htmlParse(url, useInternalNodes=T)
  title <- unname(unlist(html['//meta[@property="og:title"]/@content']))
  description <- unname(unlist(html['//meta[@property="og:description"]/@content']))
  # extract date if available
  year <- str_extract(title, "\\d{4}") # find 4 consecutive digits in title
  date <- str_extract(description, "\\d{2}\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}\\d{4}")
  #digit_present <- unlist(str_split(description, " ")) %>% grepl("[[:digit:]]",.)
  #date <- unlist(str_split(description, " ")) %>% .[which(digit_present == TRUE)[1]:which(digit_present == TRUE)[2]] %>% paste(., collapse = " ")
  caldera_corpus <- rbind(caldera_corpus, data.table("title" = title, "description" = description, "year" = year, "date" = date, "text" = text))
}

saveRDS(caldera_corpus, "/Users/pedrorodriguez/Dropbox/Research/Divisive Rhetoric/Caldera/caldera_corpus.rds")
