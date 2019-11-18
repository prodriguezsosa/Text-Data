#!/usr/bin/env Rscript
#===========================================
# purpose: scrape breitbart
# first commit: 11-06-2019
# most recent commit: 11-12-2019
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
topics <- c("politics", "economy", "entertainment", "the-media", "world-news", 
            "europe", "border", "middle-east", "africa", "asia", "latin-america", 
            "tech", "sports", "social-justice", "tag/b-inspired-news")


url_base <- "https://www.breitbart.com/"

# define start and end date
st <- as.Date("2009/09/10/")
#en <- as.Date(Sys.Date())
en <- as.Date("2019/11/12")
date_sequence <- as.character(seq(en, st, by = "-1 day"))
date_sequence <- gsub("-", "/", date_sequence)
date_sequence <- split(date_sequence, ceiling(seq_along(date_sequence)/100))
date_sequence <- unname(unlist(date_sequence[args]))

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
    Sys.sleep(sample(3, 1) * 0.1)}
  return(pg)
}

# function to extract news urls
extract_urls <- function(date_value){
  
  topic_urls <- vector("list", length(topics)) %>% setNames(topics)
  for(topic in topics){
    
    #pg <- tryCatch(read_html(paste0(url_base, topic, "/", date_value, "/")), error=function(e) e)
    #if(!is.null(pg$message)){pg <- NA}
    #Sys.sleep(sample(3, 1) * 0.1)
    pg <- try_scrape(paste0(url_base, topic, "/", date_value, "/"))
    
    #CatchupPause(3)
    
  # download html
  #pg <- try_scrape(paste0(url_base, topic, "/", date_value, "/"))
  #pg <- read_html(paste0(url_base, topic, "/", date_value, "/"))
  
  #CatchupPause(2)
  
  if(!is.na(pg)){
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  urls_of_interest <- unique(all_urls[grepl("^\\/[[:print:]][^\\/]+\\/[[:digit:]]+", all_urls)])
  urls_of_interest <- urls_of_interest[!grepl("page\\/[[:digit:]]+", urls_of_interest)]}else{urls_of_interest <- NA}
  
  #store
  topic_urls[[topic]] <- urls_of_interest
  }
  
  # return output
  return(topic_urls)
}

# loop version
start_time <- proc.time()
#library(progress)
scraped_data <- list()
#pb <- progress_bar$new(total = length(links))
for(i in 2154:length(date_sequence)){
  scraped_data[[i]] <- extract_urls(date_sequence[i])
  print(i)
  #pb$tick()
}
proc.time() - start_time

# apply version
links <- lapply(date_sequence, function(x) extract_urls(x)) %>% setNames(date_sequence)

# save links
saveRDS(links, paste0("/scratch/plr250/Scraper/Breitbart/Outputs/links", args, ".rds"))
