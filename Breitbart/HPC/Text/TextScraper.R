#!/usr/bin/env Rscript
#===========================================
# purpose: scrape breitbart
# first commit: 11-06-2019
# most recent commit: 11-12-2019
#===========================================

rm(list=ls())
library(rvest)
library(dplyr)
library(stringr)

# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- 1
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=1) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

links <- readRDS(paste0("/scratch/plr250/Scraper/Breitbart/Outputs/links.rds"))
links_sequence <- split(links, ceiling(seq_along(links)/4000))
links_sequence <- links_sequence[[args]]

# to close connections
CatchupPause <- function(Secs){
  Sys.sleep(Secs) #pause to let connection work
  closeAllConnections()
  gc()
}

# sometimes there's connection errors. This function keeps trying to scrape until scrape is successful
try_scrape <- function(link){
  error_message <- TRUE
  while(error_message){
    pg <- tryCatch(read_html(link), error=function(e) e)
    if(is.null(pg$message)){error_message <- FALSE}else{
    if(pg$message == "HTTP error 404."){ # check if link is broken
      pg <- NA
      error_message <- FALSE}} # otherwise keep trying
    Sys.sleep(sample(5, 1) * 0.1)}
  return(pg)
}

# read html
process_url <- function(link){
  #pg <- read_html(paste0(url_base, url))
  #pg <- try_scrape(paste0(url_base, url))
  
  pg <- tryCatch(read_html(link), error=function(e) e)
  if(!is.null(pg$message)){pg <- NA}
  Sys.sleep(sample(3, 1) * 0.1)
  
  #CatchupPause(3)
  
  if(!is.na(pg)){
  headline <- html_nodes(pg, '#MainW h1') %>% html_text() %>% str_trim()
  headline_subtitle <- html_nodes(pg, '#MainW h2') %>% html_text() %>% str_trim()
  date <- html_nodes(pg, 'time') %>% html_text() %>% str_trim()
  text <- html_nodes(pg, '.entry-content p') %>% html_text() %>% unlist() %>% str_trim() %>% .[.!=""]
  read_more <- html_nodes(pg, '.rmoreabt a , .byC') %>% html_text() %>% str_trim() %>% .[.!=""]
  facebook <- html_nodes(pg, xpath = '//*[@id="bbvb"]/a[1]/span') %>% html_text()
  #Sys.sleep(sample(10, 1) * 0.1)
  return(list(headline = headline, headline_subtitle = headline_subtitle, date = date, text = text, read_more = read_more, facebook = facebook, link = paste(url_base, url, sep = "/")))}else
    return(NA)
}

# apply function
#library(pbapply)
scraped_data <- lapply(links, process_url)

#library(progress)
#scraped_data <- list()
#pb <- progress_bar$new(total = length(links))
#for(i in 1:length(links)){
#  scraped_data[[i]] <- process_url(links[i])
#  print(i)
  #pb$tick()
#  }

# save text
saveRDS(scraped_data, paste0("/scratch/plr250/Scraper/Breitbart/Outputs/text", args, ".rds"))


