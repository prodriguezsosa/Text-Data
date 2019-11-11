rm(list = ls())
library(progress)
library(stringr)
library(dplyr)
library(pbapply)

# ================================
# define paths
# ================================
in_path <- "/Volumes/Potosi/Research/Data/Breitbart/Outputs/"
out_path <- ""

# ================================
# list of file names
# ================================
files <- as.list(list.files(in_path))
files_text <- files[grepl(pattern = "text", x = files)] %>% unlist() # text
files_link <- files[grepl(pattern = "link", x = files)] %>% unlist() # text

# buld links tibble for checking purposes
links <- pblapply(files_link, function(x) readRDS(paste0(in_path, x)) %>% unlist() %>% unname()) %>% unlist
links[grepl("central-american", links)]
  
# ================================
# load and pre-process data
# ================================
# each article is store as a list
# this fcn converts it into a tibble to then bind together
# next iteration: save as tibble directly
list_to_tibble <- function(text_file){
  read_more = gsub(" ", "_", text_file$read_more) %>%  paste(collapse = " ")
  return(tibble(headline = text_file$headline, 
                headline_subtitle = text_file$headline_subtitle,
                date = text_file$date,
                text = paste(text_file$text, collapse = " "),
                read_more =  read_more,
                facebook = text_file$facebook,
                link = text_file$link))
}

corpus <- list()
pb <- progress_bar$new(total = length(files_text))
for(i in 1:length(files_text)){
  
  # upload text
  texts <- readRDS(paste0(in_path, files_text[i]))
  
  # convert each element to a tibble
  texts <- lapply(texts, list_to_tibble)
  
  # bind together
  texts <- do.call(rbind, texts) %>% as_tibble
  
  # pre-process
  #texts$text <- gsub("’", "", texts$text) # remove apostrophes
  #texts$text <- gsub("[^[:alpha:]]", " ", texts$text) # remove all non-alpha characters
  #texts$text <- str_replace_all(texts$text, "^ +| +$|( ) +", "\\1")  # remove excess white space
  #texts$text <- tolower(texts$text)  # lowercase
  #texts <- texts[texts$text!="",] # remove nuls
  
  # store
  corpus[[i]] <- texts
  pb$tick()
}

# bind
corpus <- do.call(rbind, corpus)

# save
saveRDS(corpus, "/Volumes/Potosi/Research/Data/Breitbart/Outputs/breitbart_corpus.rds")
