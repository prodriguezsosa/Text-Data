rm(list = ls())
library(progress)
library(stringr)
library(dplyr)
library(pbapply)

# ================================
# define paths
# ================================
in_path <- "/Volumes/Potosi/Research/Data/Breitbart/Outputs/"

url_base <- "https://www.breitbart.com"

# ================================
# list of file names
# ================================
files <- as.list(list.files(in_path))
#files_text <- files[grepl(pattern = "text", x = files)] %>% unlist() # text
files_link <- files[grepl(pattern = "[[:digit:]]+", x = files)] %>% unlist() # text

# build links tibble for checking purposes
links <- pblapply(files_link, function(x) readRDS(paste0(in_path, x)) %>% unlist() %>% unname()) %>% unlist
links <- links[!is.na(links)] %>% unique
links <- paste0(url_base, links)

# save
saveRDS(links, paste0(in_path, "links.rds"))
