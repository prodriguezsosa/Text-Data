rm(list=ls())
#====================================
# LOAD LIBRARIES
#====================================
library(rvest)
library(XML)

#====================================
# SCRAPE LINKS
#====================================
# download links to discourses
url <- "http://rafaelcaldera.com/categoria-biblioteca/discursos/" # base url
doc <- htmlParse(url)
links <- xpathSApply(doc, "//h3//a/@href") # extract only links with h3 titles (does away with all other irrelevant links in the page)
free(doc)




