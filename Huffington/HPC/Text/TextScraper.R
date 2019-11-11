#!/usr/bin/env Rscript
#===========================================
# purpose: scrape huffington
# first commit: 11-06-2019
# most recent commit: 11-10-2019
#===========================================

rm(list=ls())
library(rvest)
library(dplyr)
library(stringr)

setwd("/scratch/plr250/Scraper/Huffington/Code/Text/")

# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- 1
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=1) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# load urls
#links <- readRDS(paste0("/Volumes/Potosi/Research/Data/Huffington/Outputs/links", args, ".rds")) %>% unlist() %>% unname()  # local
links <- readRDS(paste0("/scratch/plr250/Scraper/Huffington/Outputs/links", args, ".rds")) %>% unlist() %>% unname()
#link_dates <- names(links)
#links <- unname(unlist(links))

# sometimes there's connection errors. This function keeps trying to scrape until scrape is successful
try_scrape <- function(link){
  error_message <- TRUE
  while(error_message){
    pg <- tryCatch(read_html(link), error=function(e) e)
  if(is.null(pg$message)){error_message <- FALSE}
  Sys.sleep(sample(10, 1) * 0.1)}
  return(pg)
}

# read html
process_link <- function(link){
  pg <- try_scrape(link)
  headline <- html_nodes(pg, '.headline') %>% html_text() %>% str_trim()
  if(length(headline) == 0){headline <- html_nodes(pg, '.headline__title') %>% html_text() %>% str_trim()}
  headline_subtitle <- html_nodes(pg, '.headline__subtitle') %>% html_text() %>% str_trim()
  date <- html_nodes(pg, 'time') %>% html_text() %>% str_trim()
  if(length(date) == 0){date <- html_nodes(pg, '.timestamp__date--published span') %>% html_text() %>% str_trim()}
  text <- html_nodes(pg, 'p') %>% html_text() %>% unlist() %>% str_trim() %>% .[.!=""]
  read_more <- html_nodes(pg, '.yr-tag') %>% html_text() %>% str_trim() %>% .[.!=""]
  return(list(headline = headline, headline_subtitle = headline_subtitle, date = date, text = text, read_more = read_more, link = link))
}

# apply function
#library(pbapply)
scraped_data <- pblapply(links, process_link)

#library(progress)
#scraped_data <- list()
#pb <- progress_bar$new(total = length(links))
#for(i in 1:length(links)){
#  scraped_data[[i]] <- process_link(links[i])
#  pb$tick()
#  }

# save text
saveRDS(scraped_data, paste0("/scratch/plr250/Scraper/Huffington/Outputs/text", args, ".rds"))


