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

#########################################################
#########################################################

#clean the ncvs data
#counts, serious violent victimization
#loop through each file
setwd(datadir); dir()
filenames<-c(
  'ncvs_count_1999to2009.csv',
  'ncvs_rate_1999to2009.csv',
  'ncvs_count_2010to2015.csv',
  'ncvs_rate_2010to2015.csv',
  'ncvs_rate_1993to1998.csv',
  'ncvs_count_1993to1998.csv'
)
fulldf<-lapply(filenames,function(thisf) {
  
  #thisf<-filenames[1]
  tmpdf<-read.csv(
    thisf,
    stringsAsFactors=F
  )
  
  #get yrs, var from filename
  yrs<-str_extract_all(thisf,"[0-9]{4}")[[1]] %>% as.numeric
  years<-paste0("y",yrs[1]:yrs[2])
  thisvar<-str_extract(thisf,"count|rate")
  
  #clean and loop through
  tmp<-tmpdf$race==""
  tmpdf$race[tmp]<-NA
  tmpdf$race<-na.locf(tmpdf$race)
  tmp<-tmpdf$income==""
  tmpdf$income[tmp]<-NA
  tmpdf$income<-na.locf(tmpdf$income)
  tmpseq.i<-1:nrow(tmpdf)
  mydf<-lapply(tmpseq.i,function(i) {
    #i<-1
    x<-apply(tmpdf[i,],1,identity)
    y<-x[!is.na(x) & x!="" & x!="!"]
    if(length(y)!=(length(years)+2)) stop(print(i))
    tmpdf<-data.frame(
      t(y)
    )
  }) %>% rbind.fill
  names(mydf)<-c(
    "race",
    "income",
    years
  )
  mydf$var<-thisvar
  
  #gather and append.. 
  mydf$race<-tolower(mydf$race)
  mydf$income<-tolower(mydf$income)
  mydf<-gather(
    mydf,
    "year",
    "value",
    years
  )
  mydf
}) %>% rbind.fill


fulldf<-spread(
  fulldf,
  var,
  value
)

#now we can conver tto numeric
tmp<-as.numeric(fulldf$count)
fulldf$count[is.na(tmp)]
fulldf$count<-tmp
tmp<-as.numeric(fulldf$rate)
fulldf$rate[is.na(tmp)]
fulldf$rate<-tmp
#get # of people n each category
fulldf$number<-10^3 *fulldf$count/fulldf$rate

#classify into poor and rich
fulldf$income<-str_replace_all(
  fulldf$income,
  ",|\\$",""
) 
fulldf$income %>% unique
fulldf$class<-"other"
tmp<-fulldf$income%in%c(
  "less than 7500",
  "7500 to 14999"
)
fulldf$class[tmp]<-"poor"
tmp<-fulldf$income%in%c(
  "15000 to 24999",
  "25000 to 34999",
  "35000 to 49999",
  "50000 to 74999"
)
fulldf$class[tmp]<-"middle"
tmp<-fulldf$income%in%c(
  "75000 or more"
)
fulldf$class[tmp]<-"rich"
#drop totals
tmp<-fulldf$class=="other"
fulldf<-fulldf[!tmp,]

tmplevels<-c(
  "hispanic",
  "non-hispanic black",
  "non-hispanic other",
  "non-hispanic white"
)
tmplabels<-c(
  "hispanic",
  "black",
  "other",
  "white"
)
fulldf$race<-factor(
  fulldf$race,
  tmplevels,
  tmplabels
)
setwd(datadir)

fulldf$year<-str_extract(fulldf$year,"[0-9]+") %>%
  as.numeric

write.csv(
  fulldf,
  "ncvs_summaries.csv",
  row.names=F
)

#########################################################
#########################################################

#( all cleaning should take place here
#do it later, if there's time.. )

#########################################################
#########################################################

#save out into dfs




