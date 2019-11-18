rm(list = ls())
library(progress)
library(stringr)
library(dplyr)
library(pbapply)

# ================================
# define paths
# ================================
in_path <- "~/Drobox/GitHub/Text-Data/Huffington/Output/"
out_path <- ""

# ================================
# list of file names
# ================================
files <- as.list(list.files(in_path))
#files_text <- files[grepl(pattern = "text", x = files)] %>% unlist() # text
files_link <- files[grepl(pattern = "link", x = files)] %>% unlist() # text

# build links tibble for checking purposes
links <- pblapply(files_link, function(x) readRDS(paste0(in_path, x)) %>% unlist() %>% unname()) %>% unlist
links <- links[!is.na(links)] %>% unique

# save
saveRDS(links, "~/Drobox/GitHub/Text-Data/Huffington/Output/links.rds")
