rm(list=ls())
library(RSelenium)
library(rvest)
library(text2vec)
library(stringr)
library(data.table)
library(quanteda)
library(magrittr)
library(progress)
library(pbapply)

# ================================
#
# SCRAPE LINKS
#
# ================================
# FIRST: 
# manually open Selenium Server on the terminal by running Selenium Server binary
# binary is downloaded from: http://selenium-release.storage.googleapis.com/index.html
# once donwloaded, open terminal
# navigate to where binary is located: cd /Users/pedrorodriguez/Documents/Programs/Selenium
# run binary: java -jar selenium-server-standalone-3.6.0.jar
# last sentence on output should read: "INFO - Selenium Server is up and running"
# NOTE: default port 4444

# ONCE Selenium Server is running
# connect to server by instantiating a new remoteDriver
# make sure port is equal to server port (default: 4444)
remDr <- remoteDriver(remoteServerAddr = "localhost", port = 4444L, browserName = "firefox")

# specify base link
url <- 'http://www.congreso.es/portal/page/portal/Congreso/Congreso/Publicaciones'

legislatures <- list("PU12", "PU11", "PU10", "PUW9", "PUW8", "PUW7", "PUW6", "PUW5") # only up to legislature V is available in html format
# TO DO: scrape PDFs for previous legislatures (see: https://medium.com/@CharlesBordet/how-to-extract-and-clean-data-from-pdf-files-in-r-da11964e252e)
names(legislatures) <- c("XII Legislatura", "XI Legislatura", "X Legislatura", "IX Legislatura", "VIII Legislatura", "VII Legislatura", "VI Legislatura", "V Legislatura")

# create empty list to be filled with links
links_all <- list()

start_time <- Sys.time() # this takes aboyt 1hr on my old mac pro laptop
# loop over all legislatures
remDr$open()  # open window
for(i in 1:length(legislatures)){
  # ================================
  # go to page with texts for a given legislature
  # ================================
  links_i <- list()
  # navigate to url
  remDr$navigate(url)
  # give some time for loading (10 seconds is probably too conservative)
  Sys.sleep(5)       
  #find "Publicacion" dropdown menu
  # NOTE: to find xpaths use Firefox/Tools/Web Develper/Inspector to select item of interes on page
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="idLegislatura"]')
  webElem$clickElement()
  
  #select legislatura
  webElem1 <- remDr$findElement(using = 'xpath', value = paste0("//*[@value=", "'", legislatures[[i]], "'", "]"))
  webElem1$clickElement()
  
  #find "Publicacion" dropdown menu
  webElem <- remDr$findElement(using = 'xpath', value = '//*[@id="PUBL"]')
  webElem$clickElement()
  
  #select "diarios de sesiones"
  webElem1 <- remDr$findElement(using = 'xpath', value = "//*[@value='D']")
  webElem1$clickElement()
  
  #find "buscar" button
  webElem1 <- remDr$findElement(using = 'xpath', value = "//*[@alt='Buscar']")
  webElem1$clickElement()
  
  # ================================
  # loop through all pages for that legislature
  # ================================
  MorePages <- TRUE 
  while(MorePages){
    # get page html
    page_source <- unlist(remDr$getPageSource())
    # parse it and extract all links
    links <- page_source %>% read_html %>% html_nodes('a') %>% html_attr("href")
    
    # keep only publication links
    links <- links[grepl("CODI.#1", links)]
    links_i <- c(links_i, links)
    
    # next button link
    siguiente_labels <- page_source %>% read_html %>% html_nodes(xpath = "//div[@class='paginacion_brs']") %>% html_nodes('a') %>% html_text
    if(any(grepl('Siguiente >>', siguiente_labels))){
      siguiente <- page_source %>% read_html %>% html_nodes(xpath = "//div[@class='paginacion_brs']") %>% html_nodes('a') %>% html_attr("href")
      siguiente <- siguiente[which(siguiente_labels == 'Siguiente >>')]
      # go to next page
      base_link <- 'http://www.congreso.es'
      if(grepl(base_link, siguiente[1])){remDr$navigate(siguiente[1])}else{
        remDr$navigate(paste0('http://www.congreso.es', siguiente[1]))
      }
    }else{MorePages <- FALSE}
    # give some time for loading (10 seconds is probably too conservative)
    Sys.sleep(5)
  }
  links_all[[i]] <- links_i
}
# close session
remDr$closeWindow()
Sys.time() - start_time
# IMPORTANT: on the terminal press "Ctrl + C" to close driver
# save list of links
names(links_all) <- names(legislatures)
saveRDS(links_all, "/Users/pedrorodriguez/Dropbox/GitHub/Text-Data/Spanish Legislature/Scraper/links.rds")


# ================================
#
# SCRAPE TEXT FROM LINKS
#
# ================================
# function to convert to LATIN1
convert_latin1 <- function(string){ 
string <- space_tokenizer(string) %>% unlist
string <- lapply(string, function(x) iconv(x, "UTF-8", "LATIN1")) %>% unlist %>% paste(collapse = " ")
return(string)
}

# function to scrape text
scrapeText <- function(link){
  url <- paste0(url_base, link)
  page <- NULL
  #suppressWarnings(page <- read_html(url)) # to avoid stopping due to failed connections
  tryCatch(page <- read_html(url), error=function(e){}) # to avoid stopping due to failed connections
  if(length(page) == 2){
  Sys.sleep(2)
  # meta info
  meta <- page %>% html_nodes(xpath = '//*[(@id = "CABECERA_TEXTO_POPUP")]') %>% html_text
  meta <- meta[2] %>% convert_latin1(.) %>% str_replace_all("^ +| +$|( ) +", "\\1") 
  # text 
  text <- page %>% html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "texto_completo", " " ))]') %>% html_text
  text <- convert_latin1(text) # convert special characters
  text <- str_split(text, "\n\n") %>% unlist # separate by sentence
  text <- str_replace_all(text, "\\n", " ") %>% unlist  # remove spacing symbols
  text <- str_replace_all(text, "^ +| +$|( ) +", "\\1")  # remove excess white space
  result <- list("text" = text, "meta" = meta)
  }else{result <- "fail"}
  return(result)
}

# load links
links_all <- readRDS("/Users/pedrorodriguez/Dropbox/GitHub/Text-Data/Spanish Legislature/Scraper/links.rds")

url_base <- "http://www.congreso.es"
#corpus_text <- list()
#corpus_meta <- list()
corpus <- list()
start_time <- Sys.time() # this takes about 1hr on my old mac pro laptop
for(i in 1:length(links_all)){
  print(i)
  links_i <- links_all[[i]] %>% unlist
  #links_i <- links_i[1:5]
  pb <- progress_bar$new(total = length(links_i))
  corpus_i <- list()
  for(j in 1:length(links_i)){
  corpus_i[[j]] <-scrapeText(links_i[j])
  pb$tick()
  }
  #corpus[[i]] <- pblapply(links_i, scrapeText)
  corpus[[i]] <- corpus_i
}
Sys.time() - start_time

# ================================
#
# PRE-PROCESSING
#
# ================================
# function to remove speakers
remove_speakers <- function(string){
  string <- str_split(string, ":") %>% unlist
  if(length(string)==2){string <- string[2]}else{
    string <- paste(string, collapse = " ")
  }
  return(string)
}


text <- lapply(text, remove_speakers) %>% unlist # remove speakers
text <- chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", text) # replace accents
text <- str_replace_all(text, "^ +| +$|( ) +", "\\1")  # remove excess white space
text <- text[!(grepl("^[A-Z][A-Z]", text))]
text <- text[!(grepl("^Pagina", text))]
text <- str_replace_all(text, "NA", " ")
text <- str_replace_all(text, "\\[.*?\\]", " ") # remove all text in [], these are descriptive (e.g. [pausa])
text <- str_replace_all(text, "\\(.*?\\)", " ") # remove all text in (), these are descriptive (e.g. [pausa])
text <- str_replace_all(text, "^ +| +$|( ) +", "\\1")  # remove excess white space
text <- text[text!=""]
