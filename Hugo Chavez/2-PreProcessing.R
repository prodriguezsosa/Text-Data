###########################################
# Task: pre-process output from Scrape.R
# Date begun: 10-18-2017
# Date last modified: 11-19-2017
############################################
rm(list=ls())
library(magrittr)
library(tidytext)
library(qdap)
library(stringr)
library(data.table)
library(plyr)
library(dplyr)
library(koRpus)

# load scraped documents
texts <- readRDS("/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/scraping/chavez_discourse.rds") 

##########################################
#
#
#
# BUGGY CHARACTERS
#
#
#
##########################################
# load buggy character table (source: http://www.i18nqa.com/debug/utf8-debug.html)
load("/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Inputs/buggy_character_conversion_table.Rdata")
# order by number of characters (such that Ã! is substituted before Ã and so on)
char_table$nchar <- nchar(char_table$Actual)
char_table <- char_table[order(-char_table$nchar),]
# NOTE: buggy characters don't seem map uniquely to non-buggy characters
# e.g. Ã: in some cases it clearly should be substituted with á, in others for ó
# until we figure out what is going on with the encoding, best remove documents with buggy characters
# Ã is the most common, it is likely to be present conditional on any buggy character being present
buggy <- grepl(pattern = "Ã", texts$text)
# documents with buggy characters represent less than 2% of the documents
table(buggy)
# drop documents with buggy characters
texts <- texts[!(buggy),]

##########################################
#
#
#
# CLEAN COVARIATES
#
#
#
##########################################
# remove buggy encoding (not really necessary if buggy docs are removed)
texts[,type:=sapply(type, function(x) mgsub(char_table$Actual,char_table$Expected, x))]
texts[,subtype:=sapply(subtype, function(x) mgsub(char_table$Actual,char_table$Expected, x))]
texts[,title:=sapply(title, function(x) mgsub(char_table$Actual,char_table$Expected, x))]
texts[,address:=sapply(address, function(x) mgsub(char_table$Actual,char_table$Expected, x))]
# remove accents from labels
texts[,type:=sapply(type, function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x))]
texts[,subtype:=sapply(subtype, function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x))]
texts[,title:=sapply(title, function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x))]
texts[,address:=sapply(address, function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x))]
# fix misassigned type labels
index <- which(texts$type %in% c("1:40 PM", "10:25 AM", "12:34 AM", "2:00 AM - 4:00 AM", "3:45 PM", "5:00 PM"))
texts[index, "type"] <- texts[index, "subtype"]
texts[index, "subtype"] <- texts[index, "address"]
texts[index, "address"] <- NA
# fix misassigned subtype labels
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
texts <- texts[!(subtype %in% c("Documentos", "Libros"))]
# correct mis-assgined columns (applies to tweets. Easier to do it here than write a scraper specific to tweets)
texts[subtype == "Twitter @chavezcandanga", "text"] <- texts[subtype == "Twitter @chavezcandanga", "address"]
texts[subtype == "Twitter @chavezcandanga", "address"] <- NA

##########################################
#
#
#
# CLEAN TEXT
#
#
#
##########################################
# perform non-word specific pre-processing (needs to be done on full texts)
texts[,text:=str_replace_all(text, "@[1-9]*[a-z,A-Z]*[1-9]*", " ")]                                              # remove @ mentions from tweets
texts[,text:=str_replace_all(text, "http*", " ")]                                                                # remove URLs
texts[,text:=str_replace_all(text, "RT", " ")]                                                                   # remove retweets
texts[,text:=str_replace_all(text, "#[a-z,A-Z]*"," ")]                                                           # remove hashtags
texts[,text:=str_replace_all(text, "\\[.*?\\]", " ")]                                                            # remove all text in [], these are descriptive (e.g. [risas])
texts[,text:=str_replace_all(text, "\\(.*?\\)", " ")]                                                            # remove all text in (), these are descriptive (e.g. (risas)
#word_prepro[,word:=sapply(word, function(x) mgsub(char_table$Actual,char_table$Expected, x))]                   # remove buggy encoding  (not really necessary if buggy docs are removed)
texts[,text:=sapply(text, function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x))]  # replace special characters
texts[,text:=tolower(text)]                                                                                      # lower case
#word_prepro[,word:=sapply(word, function(x) mgsub(c("ãº","aº"),c("a","a"), x))]                                 # replace additional special characters
texts[,text:=str_replace_all(text, "[^[:alpha:]]", " ")]                                                         # replace non-alpha characters
#word_prepro[,word:=str_replace_all(word, "([a|e|i|o|u])\\1+", "\\1")]                                           # remove repeated vowels
texts[,text:=str_replace_all(text, "([a-km-qs-z])\\1+", "\\1")]                                                  # remove repeated letters except (rr and ll)
texts[,text:=str_replace_all(text, "([a-km-qs-z])\\1+", "\\1")]                                                  # remove repeated letters except (rr and ll)
texts[,text:=str_replace_all(text, "\\b\\w{1,3}\\b", "")]                                                        # remove 1-3 letter words
texts[,text:=str_replace_all(text, "^ +| +$|( ) +", "\\1")]                                                      # remove excess white space
texts <- texts[!(text == "") | is.na(text)]                                                                      # drop tokens that were deleted as a result of preprocessing
# save
saveRDS(texts, "/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/preprocessing/chavez_discourse_preprocessed.rds")

##########################################
#
#
#
# LEMMATIZE
# 
#
#
##########################################
# convert to data_frame and add document id variable
texts <- data_frame(document = 1:nrow(texts), type = texts$type, subtype = texts$subtype, title = texts$title, date = texts$date, address = texts$address, text = texts$text)
# tokenize
tokens <- texts %>% unnest_tokens(word, text)
# assign an id by token type
tokens <- tokens %>% mutate(word_id = group_indices_(tokens, .dots="word"))
# keep a vector of unique token types to perform the preprocessing on
word_prepro <- data.table(unique(tokens[,c("word", "word_id")]))

# tag
vocab_tagged <- treetag(word_prepro$word, treetagger="manual", format="obj", TT.tknz=FALSE , lang="es", TT.options=list(path="/Users/pedrorodriguez/Documents/Programs/TreeTagger", preset="es"))
# extract lemmas
vocab_lemmas <- vocab_tagged@TT.res[vocab_tagged@TT.res$lemma!="<unknown>" & vocab_tagged@TT.res$lemma!="@card@",c("token", "lemma")]
# clean lemmas of special characters (lemmas are accented)
vocab_lemmas$lemma <- sapply(vocab_lemmas$lemma, function(x) chartr("ãâàèìòùáéíóöúüûñÀÈÌÒÙÁÉÍÓÚÑ", "aaaeiouaeioouuunAEIOUAEIOUN", x))
# convert to data.table
vocab_lemmas <- data.table(vocab_lemmas) %>% set_colnames(., c("word","lemma"))
# identify tokens with multiple lemmas
vocab_lemmas[, mult := grepl(pattern = "\\|",  x = lemma)]
# keep only tokens with a single lemma
vocab_lemmas <- vocab_lemmas[!(mult),]
vocab_lemmas[,mult:=NULL] 
# lower case
vocab_lemmas[,lemma:=tolower(lemma)] 
# merge with vocab
word_prepro_lemma <- left_join(word_prepro, vocab_lemmas, by="word") %>% data.table()
# substitute terms for lemmas when lemma is available
word_prepro_lemma[!(is.na(lemma)), "word"] <- word_prepro_lemma[!(is.na(lemma)), "lemma"]
# drop lemma
word_prepro_lemma[,lemma:=NULL]

# merge back
tokens <- tokens[c("document", "word_id")]
tokens_merge <- left_join(tokens, word_prepro_lemma, by="word_id")
# drop NAs (these are words that were removed as a result of preprocessing)
tokens_merge <- tokens_merge[!is.na(tokens_merge$word),]
# remove basic plurals not captured by lemmatizing
tokens_merge$word <- str_replace_all(tokens_merge$word, "s$", " ")
# remove excess white space
tokens_merge$word <- str_replace_all(tokens_merge$word, "^ +| +$|( ) +", "\\1")
# collpase text
tokens_merge <- ddply(tokens_merge, .(document), summarize, text=paste(word, collapse=" "))

# merge clean covariates and clean text
texts <- subset(texts, select = -text)
texts <- left_join(texts, tokens_merge[c("document", "text")], by="document")
texts <- texts[!(is.na(texts$text)),]
# create new document id
texts <- texts %>% mutate(document = as.integer(seq(1, nrow(.), 1))) 
# convert to data.table
texts <- data.table(texts)
# save
saveRDS(texts, "/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/preprocessing/chavez_discourse_preprocessed_lemma.rds")

######################################################## EXTRA ########################################################

##########################################
#
#
#
# COLLOCATIONS
# 
#
#
##########################################
library(text2vec)
texts <- readRDS("/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/preprocessing/chavez_discourse_preprocessed_lemma.rds") 
# tokenize
tokens <- texts %>% unnest_tokens(word, text)
# assign an id by token type
tokens <- tokens %>% mutate(word_id = group_indices_(tokens, .dots="word"))
# collapse all documents
txt <- paste(texts$text, collapse = " ")
# open space
rm(texts)
# iterator over tokens
it <- itoken(txt)
# create vocab
vocab <- create_vocabulary(it, stopwords = tokenizers::stopwords("es"))
# create model
#model <- Collocations$new(vocabulary = vocab, collocation_count_min = 100, pmi = 5, gensim = 500, lfmd = -25)
model <- Collocations$new(vocabulary = vocab)
model_fit <- model$fit(it, n_iter = 5)
model$collocation_stat
model_fit <- model_fit[order(model_fit$rank_gensim, model_fit$rank_pmi, model_fit$rank_lfmd),]
write.csv(model_fit, "/Users/pedrorodriguez/Dropbox/Research/WordEmbeddings/Venezuela/Output/collocations/candidate_collocations.csv")
