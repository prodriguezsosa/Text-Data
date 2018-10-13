rm(list=ls())
library(RSelenium)
library(rvest)
library(text2vec)
library(stringr)
library(data.table)
library(quanteda)
library(magrittr)
library(progress)
library(pbapply)

# ================================
#
# SCRAPE LINKS
#
# ================================
# FIRST: 
# manually open Selenium Server on the terminal by running Selenium Server binary
# binary is downloaded from: http://selenium-release.storage.googleapis.com/index.html
# once donwloaded, open terminal
# navigate to where binary is located: cd /Users/pedrorodriguez/Documents/Programs/Selenium
# run binary: java -jar selenium-server-standalone-3.6.0.jar
# last sentence on output should read: "INFO - Selenium Server is up and running"
# NOTE: default port 4444

# ONCE Selenium Server is running
# connect to server by instantiating a new remoteDriver
# make sure port is equal to server port (default: 4444)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")

# specify base link
url <- 'http://pdok.bundestag.de/index.php?q=plenarprotokoll&dart=Plenarprotokoll'

# legislatures to scrape (Wahlperiode)
legislature <- list("26.10.1998 - 17.10.2002",
                    "17.10.2002 - 18.10.2005",
                    "18.10.2005 - 27.10.2009",
                    "27.10.2009 - 22.10.2013",
                    "22.10.2013 - 24.10.2017",
                    "seit 24.10.2017") # only up to legislature 14 is in machine readable PDF format

# create empty list to be filled with links
links_all <- list()

start_time <- Sys.time() # this takes aboyt 1hr on my old mac pro laptop
# loop over all legislatures
remDr$open()  # open window
pb <- progress_bar$new(total = length(legislature))
for(i in 3:length(legislature)){
  # ================================
  # go to page with texts for a given legislature
  # ================================
  links_i <- list()
  # navigate to url
  remDr$navigate(url)
  # give some time for loading (10 seconds is probably too conservative)
  Sys.sleep(5)   
  #navigate to specific legislature
  # NOTE: to find xpaths use Firefox/Tools/Web Develper/Inspector to select item of interes on page
  webElem <- remDr$findElement(using = 'xpath', value = paste0("//*[@title=", "\"", legislature[[i]], "\"", "]"))
  webElem$clickElement()
  # get page html
  page_source <- unlist(remDr$getPageSource())
  # find number of pages
  num_pages <- page_source %>% read_html %>% html_nodes('a') %>% html_attr("title")
  num_pages <- num_pages[grepl("zu Seite", num_pages)] %>% str_extract("([0-9]+)") %>% as.numeric %>% max
  next_pages <- paste("zu Seite", 2:num_pages, sep = " ")
  
  # parse first page and extract all links
  links <- page_source %>% read_html %>% html_nodes('a') %>% html_attr("href")
  
  # keep only publication links
  links <- links[grepl(".pdf", links)]
  links_i <- c(links_i, links)
  
  # ================================
  # loop through remaining pages
  # ================================
  for(j in 1:(num_pages-1)){
    # find next page button
    webElem <- remDr$findElement(using = 'xpath', value = paste0("//*[@title=", "\"", next_pages[j], "\"", "]"))
    webElem$clickElement()
    
    # scrape page html
    page_source <- unlist(remDr$getPageSource())

    # parse page and extract all links
    links <- page_source %>% read_html %>% html_nodes('a') %>% html_attr("href")
    
    # keep only publication links
    links <- links[grepl(".pdf", links)]
    links_i <- c(links_i, links)
    
    # give some time for loading (10 seconds is probably too conservative)
    Sys.sleep(5)
  }
  links_all[[i]] <- links_i
  pb$tick()
}
# close session
remDr$closeWindow()
Sys.time() - start_time
# IMPORTANT: on the terminal press "Ctrl + C" to close driver
# save list of links
names(links_all) <- unlist(legislature)
saveRDS(links_all, "/Users/pedrorodriguez/Dropbox/GitHub/Text-Data/German Legislature/Scraper/links.rds")


# ================================
#
# SCRAPE TEXT FROM LINKS
# inspiration: https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
# ================================
library(tm)
# file to temporarily save PDFs
setwd("/Users/pedrorodriguez/Dropbox/GitHub/Text-Data/German Legislature/PDFs/")

# FUNCTIONS
# create PDF extracting function
read <- readPDF(control = list(text = "-layout"))

# scrape pdf link
ScrapeText <- function(url_pdf){
  #url_pdf <- links_all[[i]][[j]]
  # download pdf
  file_name <- str_split(url_pdf, pattern = "/") %>% unlist %>% .[length(.)]
  pdf_file <- download.file(url_pdf, file_name, mode="wb")
  
  # scrape pdf
  document <- Corpus(URISource(file_name), readerControl = list(reader = read))
  
  # keep content
  doc <- content(document[[1]])
  
  # delete PDF
  if(file.exists(file_name)){file.remove(file_name)}
  
  # identify relevant content (reading the texts I identified (A) as an indicator of where the debate transcripts begin)
  line_begin <- which(grepl("\\(A\\)", doc))[1] 
  doc <- doc[line_begin:length(doc)] # each item is a page
  
  # split text by line
  doc_split <- lapply(doc, function(x) str_split(x, "\\\n")) %>% unlist(recursive = FALSE)
  return(doc_split)
} 

corpus <- list()
for(i in 1:1){
  url_pdf <- links_all[[i]]
  corpus[[i]] <- lapply(url_pdf, ScrapeText)
}

# ================================
#
# ORGANIZE TEXT
# 
# ================================
# FUNCTIONS
# function to organize text in a given sentence
organizeSentence <- function(sent_i){
  #sent_i <- doc_split[11]
  # determine length of text
  if(nchar(sent_i) > 60){
    sent_i <- data.table("column1" = substring(sent_i, 1, last = 60), "column2" = substring(sent_i, 60, last = 1000000L))
  }else{
    sent_i <- data.table("column1" = sent_i, "column2" = "")
  }
  return(sent_i)
}

# function to organize text in a given page
organizeText <- function(doc_i){ # doc_i is the output of lapply(doc, str_split("\\\n")) %>% unlist
  #doc_i <- doc[1]
  # split by sentence 
  #doc_split <- str_split(doc_i, "\\\n") %>% unlist
  # if there is a character in location 60, it is header text
  header_text <- lapply(doc_i, function(x) grepl("[[:alpha:]]", substring(x, 60, last = 65)))  %>% unlist
  doc_i <- doc_i[!header_text]
  # eliminate parentheses and brackets text
  doc_i <- str_replace_all(doc_i, "\\[.*?\\]", " ") # remove content in brackets (tends to be descriptive)
  doc_i <- str_replace_all(doc_i, "\\(.*?\\)", " ") # remove content in parentheses (tends to be descriptive)
  doc_i <- doc_i[grepl("[[:alpha:]]", doc_i)] # keep only lines with text
  # split into columns
  result <- lapply(doc_i, organizeSentence) %>% unlist(recursive = FALSE) %>% unlist
  result_col1 <- paste(result[which(names(result) == "column1")], collapse = " ") %>% str_trim %>% str_squish
  result_col2 <- paste(result[which(names(result) == "column2")], collapse = " ") %>% str_trim %>% str_squish
  result <- paste(result_col1, result_col2, collapse = " ")
  # remove word splits
  result <- gsub("- ", "", result)
  return(result)
}

corpus_clean <- list()
for(i in 1:1){
  corpus_i <- corpus[[i]] %>% unlist(recursive = FALSE)
  corpus_clean[[i]] <- lapply(corpus_i, organizeText)
}

