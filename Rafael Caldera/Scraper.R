rm(list=ls())
#====================================
# LOAD LIBRARIES
#====================================
library(rvest)
library(XML)
library(stringr)
library(data.table)
library(progress)

#====================================
# SCRAPE LINKS
#====================================
"http://rafaelcaldera.com/categoria-biblioteca/articulos/"
caldera_corpus <- data.table() 
for(j in c("discursos", "articulos", "conferencias")){ 
# download links to discourses
url <- paste0("http://rafaelcaldera.com/categoria-biblioteca/", j, "/") # base url
doc <- htmlParse(url)
links <- xpathSApply(doc, "//h3//a/@href") # extract only links with h3 titles (does away with all other irrelevant links in the page)
free(doc)

#====================================
# SCRAPE EACH LINK
#====================================

print(j)
pb <- progress_bar$new(total = length(links))
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
  # remove last line if it contains descriptive info (evidenced by "iEduc")
  if(grepl("iEduc", text[length(text)])){text <- text[1:(length(text)-1)]}
  # if date appears in text, extract
  date <- NA
  # date format: 11 de diciembre de 1984
  if(grepl("\\d+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}\\d+", text[1])){ # if a date appears in the first line of the text
  date <- str_extract(text[1], "\\d+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}\\d+") # extract the date
  text <- text[2:length(text)] # remove first line (presence of date indicates first line is descriptive)
  }
  # collapse
  text <- paste(text, collapse = " ")
  # extract title and description
  html <- htmlParse(sub("s", "", url), useInternalNodes=T)
  title <- unname(unlist(html['//meta[@property="og:title"]/@content']))
  description <- unname(unlist(html['//meta[@property="og:description"]/@content']))
  # extract year from title
  year <- str_extract(title, "\\d{4}") # find 4 consecutive digits in title
  # if date was not in text, check if it is in the description
  if(is.na(date)){date <- str_extract(description, "\\d+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}[A-z]+\\s{1}\\d+")} # find date in description
  #digit_present <- unlist(str_split(description, " ")) %>% grepl("[[:digit:]]",.)
  #date <- unlist(str_split(description, " ")) %>% .[which(digit_present == TRUE)[1]:which(digit_present == TRUE)[2]] %>% paste(., collapse = " ")
  # if year is missing, extract from date (will retunr NA if it's also missing from date)
  if(is.na(year)){year <- str_extract(date, "\\d{4}")}
  caldera_corpus <- rbind(caldera_corpus, data.table(type = j, "title" = title, "description" = description, "year" = year, "date" = date, "text" = text))
  pb$tick()
  }
}

saveRDS(caldera_corpus, "/Users/pedrorodriguez/Dropbox/Research/Divisive Rhetoric/Caldera/caldera_corpus.rds")
