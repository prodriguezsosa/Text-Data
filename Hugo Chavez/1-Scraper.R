###########################################
# Task: scrape todochavez.com
# Date begun: 10-17-2017
# Date last modified: 11-19-2017
############################################

##################################
#
#
# SCRAPE LINKS
#
#
##################################

rm(list=ls())
library(RSelenium)

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
url_base <- "http://www.todochavezenlaweb.gob.ve/todochavez/#categoria="

# create empty list to be filled with links
links <- list(alopresidente = NA, discursos_y_alocuciones = NA, encuentros_y_coloquios = NA, entrevistas_y_declaraciones = NA, escritos = NA)

# loop over the different discourse types (they have the same base link and just vary the number at the end)
for(i in 1:5){
  # specify url
  url <- paste0(url_base, i)
  # open window
  remDr$open()
  # navigate to url
  remDr$navigate(url)
  # give some time for loading (10 seconds is probably too conservative)
  Sys.sleep(10)                                                     
  # click on "Más resultados" button until it no longer appears (will allow us to donwload all links at once)
  loadMoreButtonExists <- TRUE                                          
  while(loadMoreButtonExists){                                              # while load "Más resultados" button appears
    #webElem <- try(remDr$findElement(using = "id", value = "more"))         # find button
    webElem <- try(remDr$findElement(using = "xpath", value = "//*[@id='more']/div/a"))         # find button
    if(class(webElem) == "try-error"){loadMoreButtonExists <- FALSE}        # check if search for button yields a search error
    if(loadMoreButtonExists){webElem$clickElement()}                        # if button exists, click on it
    Sys.sleep(10)                                                           # rest  (10 seconds is probably too conservative)
  }
  # once the page has loaded in its entirety (i.e. all links appear) get the page html
  page_source <- unlist(remDr$getPageSource())
  # parse it and extract the nodes of interest (links to texts)
  links[[i]] <- read_html(page_source) %>% html_nodes(".titulo a") %>% html_attr("href")
  # close browser
  remDr$closeWindow()
  # tidy up
  rm(webElem, page_source, loadMoreButtonExists, url)
}

# close session
remDr$close()
# IMPORTANT: on the terminal press "Ctrl + C" to close driver
# save list of links
saveRDS(links, "/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/scraping/links.rds")

##################################
#
#
# SCRAPE TEXT USING SCRAPED LINKS
#
#
##################################
rm(list = ls())
# libraries
library(rvest)
library(data.table)
library(dplyr)
# load links
links <- readRDS("/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/scraping/links.rds")
# remove full stop at beginnig of link ending
links <- lapply(links, function(x) gsub("^\\.", "", x))

# define base url
url_base <- "http://www.todochavezenlaweb.gob.ve/todochavez"

########################################################################
# FUNCTION: scrape_link
#
# input1: link = link ending scraped above
# input2: type = type of discourse (see list(names(links) for options)
#
# output: data.table with six variables (type, subtype, title, date, address, text)
#
#
#
########################################################################
scrape_link <- function(link, type){
  url <- paste0(url_base, link)
  webpage <- read_html(url)
  # extract the text
  text_html <- html_nodes(webpage,'.texto')
  # convert to text
  text <- html_text(text_html)
  # split text into components %>% remove empty components %>% remove redundant white space
  components <- unlist(strsplit(text, split = "\n")) %>% .[.!=""] %>% gsub("^ +| +$|( ) +", "\\1", .)
  # split the first component into date, title and address %>% remove redundant white space
  metadata <- unlist(strsplit(components[1], split = "\\.")) %>% .[.!=" "] %>% gsub("^ +| +$|( ) +", "\\1", .)
  # output data.table
  if(type == "alopresidente"){return(data.table(type = metadata[2], subtype = NA, title = components[2], date = metadata[1], address = metadata[3], text = components[3]))}
  if(type %in% c("discursos_y_alocuciones", "encuentros_y_coloquios", "entrevistas_y_declaraciones", "escritos")){
    return(data.table(type = metadata[2], subtype = metadata[3], title = components[2], date = metadata[1], address = metadata[4], text = components[3]))
    }
  Sys.sleep(5)  
}

# function to run scrape_link function on all discourse types
apply_scrape_link  <- function(type = "alopresidente"){
  # pre-allocate an empty list of length = # of documents
  temp <- vector("list", length(links[[type]]))
  # apply function scrape_link function
  temp <- lapply(links[[type]], function(x) scrape_link(x, type = type))
  # name object and return as output
  assign(type, temp) %>% rbindlist(., use.names = TRUE) %>% return(.)
}

# run apply_scrape_link
chavez_discourse <- lapply(names(links), function(x) apply_scrape_link(x))
# bind all discourse types
chavez_discourse <- rbindlist(chavez_discourse, use.names = TRUE)
# save data
saveRDS(chavez_discourse, "/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/scraping/chavez_discourse.rds")

