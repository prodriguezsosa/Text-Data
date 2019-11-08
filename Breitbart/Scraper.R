#===========================================
# purpose: scrape breitbart
# first commit: 11-06-2019
# most recent commit: 11-06-2019
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
url_base <- "https://www.breitbart.com/politics/"

# create empty vector to be filled with links
links <- vector()

# define start and end date
st <- as.Date("2009/09/10/")
en <- as.Date("2019/11/07")
date_sequence <- as.character(seq(en, st, by = "-1 day"))
date_sequence <- gsub("-", "/", date_sequence)

# loop over all dates
i <- 1
start_time <- proc.time()
#pb <- progress_bar$new(total = length(date_sequence))
for(j in date_sequence){
  
  # read html
  pg <- read_html(paste0(url_base, j, "/"))
  
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  links <- append(links, unique(all_urls[grepl("^\\/[[:print:]][^\\/]+\\/[[:digit:]]+", all_urls)]), after = length(links))
  
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
saveRDS(links, "/Users/pedrorodriguez/Drobox/GitHub/Text-Data/Breitbart/links.rds")

#===========================================
# SCRAPE TEXT USING SCRAPED LINKS
#===========================================

# read html
process_link <- function(link){
  pg <- read_html(paste(url_base, link, sep = "/"))
  headline <- html_nodes(pg, '#MainW h1') %>% html_text() %>% str_trim()
  headline_subtitle <- html_nodes(pg, '#MainW h2') %>% html_text() %>% str_trim()
  date <- html_nodes(pg, 'time') %>% html_text() %>% str_trim()
  text <- html_nodes(pg, '.entry-content p') %>% html_text() %>% unlist() %>% str_trim() %>% .[.!=""]
  read_more <- html_nodes(pg, '.rmoreabt a , .byC') %>% html_text() %>% str_trim() %>% .[.!=""]
  facebook <- html_nodes(pg, xpath = '//*[@id="bbvb"]/a[1]/span') %>% html_text()
  return(list(headline = headline, headline_subtitle = headline_subtitle, date = date, text = text, read_more = read_more, facebook = facebook, link = paste(url_base, link, sep = "/")))
}

#library(pbapply)
#scraped_data <- pblapply(links[1:20], process_link)


library(progress)
scraped_links <- vector("list", length(links))
pb <- progress_bar$new(total = length(links))

for(i in 1:length(links)){
  scraped_links[[i]] <- process_link(links[i])
  pb$tick()
}



