#============================================
# purpose: pre-process output from Scrape.R
# first commit: 10-18-2017
# most recent commit: 10-08-2018
#============================================
rm(list=ls())
library(tidyr)
library(dplyr)
library(stringr)
library(text2vec)
library(pbapply)

in_path <- "~/Dropbox/Research/WordEmbeddings/Venezuela/Output/scraping/"
in_path_buggy <- "~/Dropbox/Research/WordEmbeddings/Venezuela/Inputs/"
out_path <- "~/Dropbox/Research/WordEmbeddings/Venezuela/Output/preprocessing/"
# load scraped documents and convert to tbl class
texts <- paste0(in_path, "chavez_discourse.rds") %>% readRDS %>% tbl_df 

#============================================
# fix mis-assigned type labels (this required manual checking)
#============================================
index <- which(texts$type %in% c("1:40 PM", "10:25 AM", "12:34 AM", "2:00 AM - 4:00 AM", "3:45 PM", "5:00 PM"))
texts[index, "type"] <- texts[index, "subtype"]
texts[index, "subtype"] <- texts[index, "address"]
texts[index, "address"] <- NA

# fix mis-assigned subtype labels
index <- which(texts$subtype %in% c("Salon Ayacucho, Palacio de Miraflores, Caracas, Distrito Capital, Venezuela",
                                    "Galleria Hotel Salon Plaza II Western, Houston, Texas, Estados Unidos",
                                    "Salon Ayacucho, Palacio de Miraflores, Parroquia Catedral, Municipio Libertador, Caracas, Distrito Capital, Venezuela",
                                    "Palacio de Miraflores, Caracas, Distrito Capital, Venezuela",
                                    "Palacio de Miraflores, Caracas, Parroquia Catedral, Municipio Libertador,",
                                    "San Francisco de Yare, Municipio Simon Bolivar, Estado Miranda, Venezuela",
                                    "Caracas, Municipio Libertador, Distrito Capital, Venezuela",
                                    "Carcel de Yare, San Francisco de Yare"))

texts[index, "address"] <- texts[index, "subtype"]
texts[index, "subtype"] <- "Escritos"

# remove non-texts (these are links to books and documents)
texts <- texts %>% filter(!(subtype %in% c("Documentos", "Libros")))

# correct mis-assgined columns (applies to tweets. Easier to do it here than write a scraper specific to tweets)
texts[texts$subtype %in% c("Twitter @chavezcandanga"), "text"] <- texts[texts$subtype %in% c("Twitter @chavezcandanga"), "address"]
texts[texts$subtype %in% c("Twitter @chavezcandanga"), "address"] <- NA

#============================================
# BUGGY CHARACTERS
#============================================
# load buggy character table
# source: http://www.i18nqa.com/debug/utf8-debug.html
char_table <- readRDS(paste0(in_path_buggy, "buggy_character_conversion_table.rds"))

# function to convert to LATIN1
convert_latin1 <- function(string){ 
  string <- space_tokenizer(string) %>% unlist
  string <- lapply(string, function(x) iconv(x, "UTF-8", "LATIN1")) %>% unlist %>% paste(collapse = " ")
  return(string)
}

# identify buggy texts
buggy <- texts[,"text"] %>% pblapply(function(x) grepl(paste(char_table$Actual, collapse="|"), x)) %>% unlist %>% unname
#texts$text[buggy]  <- lapply(texts$text[buggy], convert_latin1) %>% unlist
texts$type[buggy]  <- lapply(texts$type[buggy], convert_latin1) %>% unlist
texts$subtype[buggy] <- lapply(texts$subtype[buggy], convert_latin1) %>% unlist
texts$title[buggy] <- lapply(texts$title[buggy], convert_latin1) %>% unlist
texts$address[buggy] <- lapply(texts$address[buggy], convert_latin1) %>% unlist

#============================================
# CLEAN COVARIATES
#============================================
# remove accents and other special characters
texts[,"type"] <- lapply(texts[,"type"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"subtype"] <- lapply(texts[,"subtype"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"title"] <- lapply(texts[,"title"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"address"] <- lapply(texts[,"address"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"title"] <- lapply(texts[,"title"], function(x) gsub(paste("N°", "Nº", sep = "|"), "", x)) %>% unlist

#============================================
# CLEAN TEXT
#============================================
# perform non-word specific pre-processing (needs to be done on full texts)
texts <- texts %>% mutate(text = str_replace_all(text, "@[1-9]*[a-z,A-Z]*[1-9]*", " ")) # remove @ mentions from tweets
texts <- texts %>% mutate(text = str_replace_all(text, "http*", " "))                   # remove URLs
texts <- texts %>% mutate(text = str_replace_all(text, "RT", " "))                      # remove retweets
texts <- texts %>% mutate(text = str_replace_all(text, "#[a-z,A-Z]*"," "))              # remove hashtags
texts <- texts %>% mutate(text = str_replace_all(text, "\\[.*?\\]", " "))               # remove all text in [], these are descriptive (e.g. [risas])
texts <- texts %>% mutate(text = str_replace_all(text, "\\(.*?\\)", " "))               # remove all text in (), these are descriptive (e.g. (risas)
texts[,"text"] <- lapply(texts[,"text"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist # replace special characters
texts <- texts %>% mutate(text = tolower(text))                                         # lower case
texts <- texts %>% mutate(text = str_replace_all(text, "[^[:alpha:]]", " "))            # replace non-alpha characters
texts <- texts %>% mutate(text = str_replace_all(text, "([a-km-qs-z])\\1+", "\\1"))     # remove repeated letters except rr and ll (e.g. gooool)
#texts <- texts %>% mutate(text = str_replace_all(text, "\\b\\w{1,3}\\b", ""))           # remove 1-3 letter words
texts <- texts %>% mutate(text = str_replace_all(text, "^ +| +$|( ) +", "\\1"))         # remove excess white space
texts <- filter(texts, text!="" & !is.na(text))                                         # drop tokens that were deleted as a result of preprocessing

# save
saveRDS(texts, paste0(out_path, "chavez_discourse_preprocessed.rds"))
