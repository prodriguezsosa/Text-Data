#============================================
# purpose: pre-process output from Scrape.R
# first commit: 10-18-2017
# most recent commit: 08-03-2018
#============================================
rm(list=ls())
library(tidyr)
library(dplyr)
library(stringr)

in_path <- "~/Dropbox/GitHub/Text-Data/Hugo Chavez/"
# load scraped documents and convert to tbl class
texts <- paste0(in_path, "chavez_discourse.rds") %>% readRDS %>% tbl_df 

#============================================
# BUGGY CHARACTERS
#============================================
#texts <- texts %>% mutate(text = iconv(text, to = "UTF-8"))  # does not help
# NOTE: buggy characters don't seem map uniquely to non-buggy characters
# otherwise we could simply substitute them for the expected character (see http://www.i18nqa.com/debug/utf8-debug.html)
# e.g. Ã: in some cases it clearly should be substituted with á, in others for ó
# until we figure out what is going on with the encoding, best remove documents with buggy characters
# Ã is the most common, it is likely to be present conditional on any buggy character being present
buggy <- texts[,"text"] %>% lapply(function(x) grepl("Ã", x)) %>% unlist 
# documents with buggy characters represent less than 2% of the documents
table(buggy)
# drop documents with buggy characters (107 out of 6123)
texts <- filter(texts, !buggy)
  
#============================================
# CLEAN COVARIATES
#============================================
# remove accents and other special characters
texts[,"type"] <- lapply(texts[,"type"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"subtype"] <- lapply(texts[,"subtype"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"title"] <- lapply(texts[,"title"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist
texts[,"address"] <- lapply(texts[,"address"], function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x)) %>% unlist

# fix mis-assigned type labels (this required manual checking)
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
texts <- texts %>% mutate(text = str_replace_all(text, "\\b\\w{1,3}\\b", ""))           # remove 1-3 letter words
texts <- texts %>% mutate(text = str_replace_all(text, "^ +| +$|( ) +", "\\1"))         # remove excess white space
texts <- filter(texts, text!="" & !is.na(text))                                         # drop tokens that were deleted as a result of preprocessing

# save
saveRDS(texts, paste0(in_path, "chavez_discourse_preprocessed.rds"))
