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
for(i in 1:length(legislature)){
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
saveRDS(links_all, "/Volumes/Potosi/Research/German Legislature/links.rds")

# ================================
#
# SCRAPE TEXT FROM LINKS
# inspiration: https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e
# ================================
library(tm)
#links_all <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Text-Data/German Legislature/Scraper/links.rds")
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
  pdf_file <- NULL
  tryCatch(pdf_file <- download.file(url_pdf, file_name, mode="wb"), error=function(e){}) # to avoid stopping due to failed connections
  Sys.sleep(5)
  if(!is.null(pdf_file)){
  # scrape pdf
  document <- Corpus(URISource(file_name), readerControl = list(reader = read))
  
  # keep content
  doc <- content(document[[1]])
  
  # delete PDF
  if(file.exists(file_name)){file.remove(file_name)}
  
  # identify relevant content (reading the texts I identified (A) as an indicator of where the debate transcripts begin)
  line_begin <- which(grepl("\\(A\\)", doc))[1]
  if(!is.na(line_begin)){
  doc <- doc[line_begin:length(doc)] # each item is a page
  
  # split text by line
  doc_split <- lapply(doc, function(x) str_split(x, "\\\n")) %>% unlist(recursive = FALSE)
  }else{
    doc_split <- NA
  }}else{
    doc_split <- NA
  }
  return(doc_split)
} 

corpus <- list()
pb <- progress_bar$new(total = length(legislature))
for(i in 2:length(legislature)){
  url_pdf <- links_all[[i]]
  corpus[[i]] <- lapply(url_pdf, ScrapeText)
  pb$tick()
}

saveRDS(corpus, "/Volumes/Potosi/Research/German Legislature/corpus.rds")

# ================================
#
# ORGANIZE TEXT
# 
# ================================
corpus <- readRDS("/Volumes/Potosi/Research/German Legislature/corpus.rds")

# for testing
legislature_i <- corpus[[1]]
doc_i <- legislature_i[[1]]
page_i <- doc_i[[1]]
sent_i <- page_i[1]

# FUNCTIONS
# function to organize text in a given sentence
organizeSentence <- function(sent_i){
  if(nchar(sent_i) > 60){ # determine length of text to evaluate if it has one or two columns
    # e.g. [1] "       Vizepräsidentin Petra Bläss: Guten Tag, liebe                     mit 9,8 Prozent wieder das beste Ergebnis erzielt. Eine"
    # manual inspection of a subset of the documents suggests 60 as a pt of column split (1000000L = select to end)  
      sent_i <- data.table("column1" = substring(sent_i, 1, last = 60), "column2" = substring(sent_i, 60, last = 1000000L))
  }else{ # if nchar(sent_i) <= 60, sentence only has one column
    sent_i <- data.table("column1" = sent_i, "column2" = "")
  }
  return(sent_i)
}

# function to organize text in a given page
organizePage <- function(page_i){
  #legislature_i <- corpus[[1]]
  #doc_i <- legislature_i[[1]]
  #page_i <- doc_i[[1]]
  #sent_i <- page_i[1]
  #if(!is.na(page_i)){
  # if there is a character in the middle of the space, it is header text
  #e.g.: [1] "                         Deutscher Bundestag – 14. Wahlperiode – 78. Sitzung. Berlin, Mittwoch, den 15. Dezember 1999         7121"
  header_text <- lapply(page_i, function(x) grepl("[[:alpha:]]", substring(x, 60, last = 65)))  %>% unlist
  page_i <- page_i[!header_text] # delete al sentences on page that are header text
  # eliminate parentheses and brackets text
  page_i <- str_replace_all(page_i, "\\[.*?\\]", " ") # remove content in brackets (tends to be descriptive)
  page_i <- str_replace_all(page_i, "\\(.*?\\)", " ") # remove content in parentheses (tends to be descriptive)
  page_i <- page_i[grepl("[[:alpha:]]", page_i)] # keep only lines with text
  # split into columns
  result <- lapply(page_i, organizeSentence) %>% unlist(recursive = FALSE) %>% unlist
  result_col1 <- paste(result[which(names(result) == "column1")], collapse = " ") %>% str_trim %>% str_squish
  result_col2 <- paste(result[which(names(result) == "column2")], collapse = " ") %>% str_trim %>% str_squish
  result <- paste(result_col1, result_col2, collapse = " ")
  # remove word splits
  result <- gsub("- ", "", result)
  # split by speakers
  result <- str_split(result, ":") %>% lapply(.,str_trim) %>% lapply(.,str_squish) %>% unlist(recursive = FALSE)
  #}else{
  #  result <- NA
  #}
  return(result)
}

organizeDocument <- function(doc_i){
  doc_i <- lapply(doc_i, organizePage) %>% unlist
  return(doc_i)
}

organizeLegislature <- function(legislature_i){
  legislature_i <- pblapply(legislature_i, organizeDocument) %>% unlist
  return(legislature_i)
}

corpus_clean <- list()
for(i in 2:length(corpus)){
  legislature_i <- corpus[[i]]
  corpus_clean[[i]] <- organizeLegislature(legislature_i)
  print(i)
}

saveRDS(corpus_clean, "/Volumes/Potosi/Research/German Legislature/corpus_clean.rds")
