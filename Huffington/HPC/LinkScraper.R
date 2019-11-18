#!/usr/bin/env Rscript
#===========================================
# purpose: scrape huffington
# first commit: 11-06-2019
# most recent commit: 11-12-2019
#===========================================

rm(list=ls())
library(rvest)
library(dplyr)
library(pbapply)

# ================================
# arguments (used to facilitate HPC processing)
# ================================
#args <- 18
args <- commandArgs(trailingOnly = TRUE)
if(length(args)!=1) stop(paste0("Not the right number of arguments!", args))
args <- as.integer(args)

# specify base link
url_base <- "https://www.huffpost.com/archive/"

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

# function to extract news urls
extract_urls <- function(date_value){
  
  # download html
  pg <- try_scrape(paste0(url_base, date_value))
  #pg <- read_html(paste0(url_base, date_value))
  
  CatchupPause(3)
  
  if(!is.na(pg)){
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  urls_of_interest <- unique(all_urls[grepl("entry", all_urls)])}else{urls_of_interest <- NA}

  # return output
  return(urls_of_interest)
}

links_list <- list()
for(i in 1:length(c(36, 37, 39:48))){
  args <- c(36, 37, 39:48)[i]
  # define start and end date
  st <- as.Date("2005-05-08")
  #en <- as.Date(Sys.Date())
  en <- as.Date("2019-11-12")
  date_sequence <- as.character(seq(en, st, by = "-1 day"))
  date_sequence <- split(date_sequence, ceiling(seq_along(date_sequence)/100))
  #date_sequence <- unname(unlist(date_sequence[args]))
  date_sequence <- unname(unlist(date_sequence[args]))
  
  #lapply(date_sequence, function(x) extract_urls(x)) %>% setNames(date_sequence)
  
  links_list[[i]] <- pblapply(date_sequence, function(x) extract_urls(x)) %>% setNames(date_sequence)
  print(i)
}

#start_time <- proc.time()
# for testing
#links <- vector()
#for(i in 1:length(date_sequence)){
#  links[i] <- extract_urls(date_sequence[i])
#  print(i)
#}
#proc.time() - start_time

# save links
#saveRDS(links, paste0("/scratch/plr250/Scraper/Huffington/Outputs/links", args, ".rds"))

saveRDS(links_list, "/Users/pedrorodriguez/Drobox/GitHub/Text-Data/Huffington/links_list.rds")
