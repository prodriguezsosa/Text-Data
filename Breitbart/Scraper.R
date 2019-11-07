#===========================================
# purpose: scrape breitbart
# first commit: 11-06-2019
# most recent commit: 11-06-2019
#===========================================

#===========================================
# SCRAPE LINKS
#===========================================

rm(list=ls())
library(RSelenium)
library(rvest)
library(dplyr)
library(stringr)

in_path <- "~/Dropbox/GitHub/Text-Data/Breitbart"

# connect to server by instantiating a selenium server and browser
rD <- rsDriver(browser = "firefox")
remDr <- rD[["client"]]

# specify base link
url_base <- "https://www.breitbart.com/politics/"

# create empty vector to be filled with links
links <- vector()

# navigate to url
remDr$navigate(url_base)

# initialize a load more button
loadMoreButtonExists <- TRUE 
#i <- 1

while(loadMoreButtonExists){
#while(loadMoreButtonExists){
  
  # read url
  current_url <- unlist(remDr$getCurrentUrl())
  
  # read html
  pg <- read_html(current_url)
  
  # extract links
  all_urls <- html_attr(html_nodes(pg, "a"), "href")
  
  # from links extract relevant ones
  links <- append(links, unique(all_urls[grepl("^\\/[[:print:]][^\\/]+\\/[[:digit:]]+", all_urls)]), after = length(links))
  
  # check if there's a load more button
  webElem <- try(remDr$findElement(using = "xpath", value = "/html/body/div[2]/div/div/section/div/a"))  # find button
  if(class(webElem) == "try-error"){loadMoreButtonExists <- FALSE}  # check if search for button yields a search error
  if(loadMoreButtonExists){webElem$clickElement()}  # if button exists, click on it
  
  # give some time for loading (10 seconds is probably too conservative)
  Sys.sleep(5)

  cat("pages scraped", i, "\n")
  cat("total number of scraped links", length(links), "\n")
  cat("number of unique links", length(unique(links)), "\n")
  
  i = i + 1
  
}

# close browser
remDr$close()

# stop the selenium server
rD[["server"]]$stop()

# IMPORTANT: on the terminal press "Ctrl + C" to close driver
# save list of links
#saveRDS(links, paste0(in_path, "links.rds"))

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



