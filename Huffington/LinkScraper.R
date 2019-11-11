#===========================================
# purpose: scrape huffington post
# first commit: 11-06-2019
# most recent commit: 11-10-2019
#===========================================

#===========================================
# SCRAPE LINKS
#===========================================

rm(list=ls())
#library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)
library(progress)

in_path <- "~/Dropbox/GitHub/Text-Data/Breitbart"

# specify base link
url_base <- "https://www.huffpost.com/archive/"

# create empty vector to be filled with links
links <- vector()

# define start and end date
st <- as.Date("2005-05-08")
en <- as.Date(Sys.Date())
date_sequence <- as.character(seq(en, st, by = "-1 day"))

# loop over all dates
i <- 1
start_time <- proc.time()
#pb <- progress_bar$new(total = length(date_sequence))
for(j in date_sequence){
  
  # read html
  pg <- read_html(paste0(url_base, j))
  
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  links <- append(links, unique(all_urls[grepl("entry", all_urls)]), after = length(links))
  
  # print summary
  cat("pages scraped", i, "out of", length(date_sequence),  "\n")
  elapsed_time <- (proc.time() - start_time)/60
  cat("estimated minutes remaining:", (elapsed_time["elapsed"]/i)*(length(date_sequence) - i), "\n")
  cat("total number of scraped links", length(links), "\n")
  cat("number of unique links", length(unique(links)), "\n")
  
  # progress bar
  i <- i + 1
  #pb$tick()
  
  # give some time to avoid being blocked (5 seconds is probably too conservative)
  Sys.sleep(1)
}

# save links
saveRDS(links, "/Users/pedrorodriguez/Drobox/GitHub/Text-Data/Huffington/links.rds")

