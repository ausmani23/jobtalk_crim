#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(tidyr)
require(rprojroot)

#set dirs
homedir<-find_root(
  criterion=has_file('crimtalk.RProj')
)
codedir<-file.path(homedir,"code")
setwd(codedir); dir()
source('dirs.R')

#extra packs
require(rvest)

#########################################################
#########################################################

# #cross-national prison pop
# #get from prison studies
# tmpurl<-"http://www.prisonstudies.org/highest-to-lowest/prison_population_total?field_region_taxonomy_tid=All"
# tmpdf<- tmpurl %>% 
#   read_html() %>%
#   html_nodes(xpath='//*[@id="views-aggregator-datatable"]') %>%
#   html_table(fill=F)
# tmpdf<-tmpdf[[1]]
# names(tmpdf)<-tolower(names(tmpdf))
# tmpdf$ranking<-NULL
# names(tmpdf)<-c(
#   "countryname",
#   "prisonpop",
#   "prisonrate",
#   "pretrial",
#   "pctfemale",
#   "pctforeign",
#   "occupancy"
# )
# head(tmpdf)
# 
# #save out
# setwd(datadir); dir()
# write.csv(
#   tmpdf,
#   'prisonrates_crossnational.csv',
#   row.names=F
# )
# 
# #########################################################
# #########################################################
# 
# #cross-natinoal homicide pop
# #from wikipedia, via UNDOC
# tmpurl<-"https://en.wikipedia.org/wiki/List_of_countries_by_intentional_homicide_rate"
# tmpdf<- tmpurl %>% 
#   read_html() %>%
#   html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]/tbody/tr/td[2]/table') %>%
#   html_table(fill=F)
# tmpdf<-tmpdf[[1]]
# names(tmpdf)<-tolower(names(tmpdf))
# names(tmpdf)<-c(
#   "countryname",
#   "region",
#   "subregion",
#   "homrate",
#   "homcount",
#   "datayear",
#   "source"
# )
# tmpdf<-tmpdf[!is.na(tmpdf$homrate),]
# 
# #save out
# setwd(datadir); dir()
# write.csv(
#   tmpdf,
#   'homrates_crossnational.csv',
#   row.names=F
# )
# 
# #########################################################
# #########################################################
# 
# #cross-national population, for homicide
# tmpurl<-"https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations)"
# tmpdf<- tmpurl %>% 
#   read_html() %>%
#   html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>%
#   html_table(fill=F)
# tmpdf<-tmpdf[[1]]
# names(tmpdf)<-c("rank","country","region","region2","pop16","pop17","chg")
# tmpdf$rank<-as.numeric(tmpdf$rank)
# tmpdf<-tmpdf[!is.na(tmpdf$rank),]
# tmpdf$country<-str_replace(tmpdf$country,"\\[.\\]","")
# 
# #save out
# setwd(datadir); dir()
# write.csv(
#   tmpdf,
#   'populations_crossnational.csv',
#   row.names=F
# )

#########################################################
#########################################################