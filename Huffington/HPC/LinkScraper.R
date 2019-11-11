#!/usr/bin/env Rscript
#===========================================
# purpose: scrape huffington
# first commit: 11-06-2019
# most recent commit: 11-11-2019
#===========================================

rm(list=ls())
library(rvest)
library(dplyr)

# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- 8
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=1) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# specify base link
url_base <- "https://www.huffpost.com/archive/"

# define start and end date
st <- as.Date("2005-05-08")
#en <- as.Date(Sys.Date())
en <- as.Date("2019-11-11")
date_sequence <- as.character(seq(en, st, by = "-1 day"))
date_sequence <- split(date_sequence, ceiling(seq_along(date_sequence)/200))
date_sequence <- unname(unlist(date_sequence[args]))

# sometimes there's connection errors. This function keeps trying to scrape until scrape is successful
try_scrape <- function(link){
  error_message <- TRUE
  while(error_message){
    pg <- tryCatch(read_html(link), error=function(e) e)
    if(is.null(pg$message)){error_message <- FALSE}
    Sys.sleep(sample(10, 1) * 0.1)}
  return(pg)
}

# function to extract news urls
extract_urls <- function(date_value){
  
  # download html
  pg <- try_scrape(paste0(url_base, date_value))
  #pg <- read_html(paste0(url_base, date_value))
  
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  urls_of_interest <- unique(all_urls[grepl("entry", all_urls)])

  # return output
  return(urls_of_interest)
}

links <- lapply(date_sequence, function(x) extract_urls(x)) %>% setNames(date_sequence)

# save links
saveRDS(links, paste0("/scratch/plr250/Scraper/Huffington/Outputs/links", args, ".rds"))
