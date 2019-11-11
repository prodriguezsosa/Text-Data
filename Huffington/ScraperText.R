#===========================================
# purpose: scrape huffington post
# first commit: 11-06-2019
# most recent commit: 11-06-2019
#===========================================
#https://www.huffpost.com/news/politics
#https://www.breitbart.com/politics
#http://www.ebizmba.com/articles/political-websites

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
url_base <- "https://www.huffpost.com/news/politics"

# navigate to url
remDr$navigate(url_base)

# divisibility fcn
div_10 <- function(x) x[x %% 10 == 0]

# load all the links
loadMoreButtonExists <- TRUE 
i <- 1

links <- vector()
start_time <- proc.time()
#while(loadMoreButtonExists & i <=10){ # while load "Load More Articles" button appears
while(loadMoreButtonExists){   
  webElem <- try(remDr$findElement(using = "xpath", value = "//*[@id='zone-twilight2']/section/button"))  # find button
  if(class(webElem) == "try-error"){loadMoreButtonExists <- FALSE}  # check if search for button yields a search error
  if(loadMoreButtonExists){webElem$clickElement()}  # if button exists, click on it
  
  Sys.sleep(4)                                     # rest  (10 seconds is probably too conservative)
  
  if(length(div_10(i)) == 1){
    
    # read html
    page_source <- unlist(remDr$getPageSource())
    pg <- read_html(page_source)
    
    # extract links
    all_urls <- html_attr(html_nodes(pg, "a"), "href")
    
    # from links extract relevant ones
    links <- unique(all_urls[grepl("entry", all_urls)])
    
    # print summary
    cat("number of unique links scraped", length(unique(links)), "\n")
  }
  
  # print summary
  cat("number of button presses", i, "\n")
  
  i <- i + 1
}
proc.time() - start_time

# read html
page_source <- unlist(remDr$getPageSource())
pg <- read_html(page_source)
  
# from links extract relevant ones
links <- all_urls[grepl("https://www.huffpost.com/entry/", all_urls)] %>% unique()
  
# close browser
remDr$close()

# stop the selenium server
rD[["server"]]$stop()




#===========================================
# SCRAPE TEXT USING SCRAPED LINKS
#===========================================

# read html
process_link <- function(link){
  pg <- read_html(link)
  headline <- html_nodes(pg, '.headline__title') %>% html_text() %>% str_trim()
  headline_subtitle <- html_nodes(pg, '.headline__subtitle') %>% html_text() %>% str_trim()
  date <- html_nodes(pg, '.timestamp__date--published span') %>% html_text() %>% str_trim()
  text <- html_nodes(pg, 'p') %>% html_text() %>% unlist() %>% str_trim() %>% .[.!=""]
  read_more <- html_nodes(pg, '.yr-tag') %>% html_text() %>% str_trim() %>% .[.!=""]
  return(list(headline = headline, headline_subtitle = headline_subtitle, date = date, text = text, read_more = read_more, link = link))
}

#library(pbapply)
#scraped_data <- pblapply(links[1:20], process_link)


library(progress)
scraped_links <- vector("list", length(links))
pb <- progress_bar$new(total = length(links))

for(i in length(links)){
  scraped_links[[i]] <- process_link(links[i])
  pb$tick()
}



