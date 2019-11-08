#!/usr/bin/env Rscript
#===========================================
# purpose: scrape breitbart
# first commit: 11-06-2019
# most recent commit: 11-07-2019
#===========================================

rm(list=ls())
library(rvest)
library(dplyr)
library(stringr)

setwd("/scratch/plr250/Scraper/Breitbart/Code/Text/")

# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- 1
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=1) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# specify base link
url_base <- "https://www.breitbart.com"

# load urls
#links <- readRDS(paste0("/Volumes/Potosi/Research/Data/Breitbart/links/links", args, ".rds")) %>% unlist() %>% unname()  # local
links <- readRDS(paste0("/scratch/plr250/Scraper/Breitbart/Outputs/links", args, ".rds")) %>% unlist() %>% unname()
#link_dates <- names(links)
#links <- unname(unlist(links))

try_scrape <- function(link){
  error_message <- TRUE
  while(error_message){
    pg <- tryCatch(read_html(paste0(url_base, links[5])), error=function(e) e)
  if(is.null(pg$message)){error_message <- FALSE}
  Sys.sleep(sample(10, 1) * 0.1)}
  return(pg)
}

# read html
process_link <- function(link){
  #pg <- read_html(paste0(url_base, link))
  pg <- try_scrape(paste0(url_base, link))
  headline <- html_nodes(pg, '#MainW h1') %>% html_text() %>% str_trim()
  headline_subtitle <- html_nodes(pg, '#MainW h2') %>% html_text() %>% str_trim()
  date <- html_nodes(pg, 'time') %>% html_text() %>% str_trim()
  text <- html_nodes(pg, '.entry-content p') %>% html_text() %>% unlist() %>% str_trim() %>% .[.!=""]
  read_more <- html_nodes(pg, '.rmoreabt a , .byC') %>% html_text() %>% str_trim() %>% .[.!=""]
  facebook <- html_nodes(pg, xpath = '//*[@id="bbvb"]/a[1]/span') %>% html_text()
  Sys.sleep(sample(10, 1) * 0.1)
  return(list(headline = headline, headline_subtitle = headline_subtitle, date = date, text = text, read_more = read_more, facebook = facebook, link = paste(url_base, link, sep = "/")))
}

# apply function
#library(pbapply)
scraped_data <- lapply(links, process_link)

#library(progress)
#scraped_data <- list()
#pb <- progress_bar$new(total = length(links))
#for(i in 1:length(links)){
#  scraped_data[[i]] <- process_link(links[i])
#  pb$tick()
#  }

# save text
saveRDS(scraped_data, paste0("/scratch/plr250/Scraper/Breitbart/Outputs/text", args, ".rds"))


