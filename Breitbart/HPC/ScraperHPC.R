#!/usr/bin/env Rscript
#===========================================
# purpose: scrape breitbart
# first commit: 11-06-2019
# most recent commit: 11-07-2019
#===========================================

rm(list=ls())
library(rvest)
library(dplyr)

# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- 1
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=1) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# specify base link
url_base <- "https://www.breitbart.com/politics/"

# define start and end date
st <- as.Date("2009/09/10/")
en <- as.Date("2019/11/07")
date_sequence <- as.character(seq(en, st, by = "-1 day"))
date_sequence <- gsub("-", "/", date_sequence)
date_sequence <- split(date_sequence, ceiling(seq_along(date_sequence)/100))
date_sequence <- unname(unlist(date_sequence[args]))

# function to extract news urls
extract_urls <- function(date_value){
  # download html
  pg <- read_html(paste0(url_base, date_value, "/"))
  
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  urls_of_interest <- unique(all_urls[grepl("^\\/[[:print:]][^\\/]+\\/[[:digit:]]+", all_urls)])
  urls_of_interest <- urls_of_interest[!grepl("page\\/[[:digit:]]+", urls_of_interest)]
  
  # return output
  return(urls_of_interest)
}

links <- lapply(date_sequence, function(x) extract_urls(x)) %>% setNames(date_sequence)

# save links
saveRDS(links, paste0("/scratch/plr250/Scraper/Breitbart/Outputs/links", args, ".rds"))
