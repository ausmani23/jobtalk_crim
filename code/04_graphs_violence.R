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
require(data.table)

#set dirs
homedir<-find_root(
  criterion=has_file('crimtalk.RProj')
)
codedir<-file.path(homedir,"code")
setwd(codedir); dir()
source('dirs.R')

#set datadir
setwd(datadir); dir()
plotdfs<-list() #to save any

#########################################################
#########################################################

#set up graph prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
#load fonts
loadfonts(quiet=T) #register w/ pdf
loadfonts(device = "win",quiet=T) #register w/ windows
#fonts()
#get ghostscript, for tex output
gsdir<-file.path(
  "c:",
  "Program Files",
  "gs"
)
gsdir_full<-file.path(
  gsdir,
  dir(gsdir),
  "bin",
  "gswin64c.exe"
)
Sys.setenv(
  R_GSCMD = gsdir_full
)

setwd(codedir)
source('theme_black.R')

#initialize graphlist
gs.list<-list()

#########################################################
#########################################################

#over-time
tmpdfs<-list()
setwd(datadir); dir()

#histstat
tmpdf<-read.csv(
  'homrates_longitudinal_histstat.csv',
  stringsAsFactors=F,
  skip=2
)
names(tmpdf)<-tolower(names(tmpdf))
tmpcols<-!str_detect(names(tmpdf),"^x")
tmprows<-!is.na(tmpdf$year)
tmpdf<-tmpdf[tmprows,tmpcols]
names(tmpdf)<-c("year","homrate_census","homrate_eckberg","homs_reported")
#create harmonized series
tmpdf$homrate_eckberg<-as.numeric(tmpdf$homrate_eckberg)
tmpdf$homrate_census<-as.numeric(tmpdf$homrate_census)
tmpdf$homrate[tmpdf$year<=1932]<-tmpdf$homrate_eckberg[tmpdf$year<=1932]
tmpdf$homrate[tmpdf$year>1932]<-tmpdf$homrate_census[tmpdf$year>1932]
tmpdf$source<-"histstat"
tmpdfs[['histstat']]<-tmpdf[,c("year","homrate","source")]

#cdc
setwd(datadir); dir()
tmpdf<-read.csv(
  'homrates_longitudinal_cdc.txt',
  stringsAsFactors=F,
  sep="\t"
)
names(tmpdf)<-tolower(names(tmpdf))
tmprows<-!is.na(tmpdf$year)
tmpdf$homrate<-as.numeric(tmpdf$crude.rate)
tmpdf$source<-"cdc"
tmpcols<-c("year","homrate","source")
tmpdf<-tmpdf[tmprows,tmpcols]
tmpdfs[['cdc']]<-tmpdf[,c("year","homrate","source")]

#put them together, plot
tmpdf<-rbind.fill(tmpdfs)
allyrs<-min(tmpdf$year):max(tmpdf$year)
missyrs<-allyrs[!allyrs%in%tmpdf$year]
newdf<-data.frame(
  year=missyrs
)
tmpdf<-rbind.fill(
  tmpdf,
  newdf
)
tmporder<-order(tmpdf$year)
tmpdf<-tmpdf[tmporder,]
#ipolate
tmpdf$homrate<-zoo::na.approx(tmpdf$homrate)

#output this df for later use
setwd(datadir); dir()
write.csv(
  tmpdf,
  'homrates_ipolated.csv',
  row.names=F
)
homdf<-tmpdf #for below

#assign rise/fall dates
styrs<-c(1900,1933,1957,1993)
loopdf<-data.frame(
  period=c(1,2,3,4),
  type=c('rise','fall','rise','fall'),
  styear=styrs,
  endyear=c(styrs[2:4],max(tmpdf$year))
)
tmpseq.i<-1:nrow(loopdf)
tmpdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisrow<-loopdf[i,]
  thisyrs<-thisrow$styear:thisrow$endyear
  df<-tmpdf[tmpdf$year%in%thisyrs,]
  df$period<-thisrow$period
  df$type<-thisrow$type
  df
}) %>% rbind.fill

#make a couple of plots
myplots<-list(
  fall=c(2),
  fallrise=c(2,3),
  fallrisefall=c(2,3,4),
  risefallrisefall=1:4
)

for(i in 1:length(myplots)) {
  #i<-1
  myperiods<-myplots[[i]]
  tmp<-tmpdf$period%in%myperiods
  tmptitle<-paste0(
    "fig_homrates_",
    names(myplots)[i],
    ".pdf"
  )
  plotdf<-tmpdf[tmp,]
  tmplevels<-c("rise","fall")
  plotdf$type<-factor(
    plotdf$type,
    tmplevels,
    tmplevels
  )
  tmpcolors_nat<-c(
    orange,
    white
  )
  names(tmpcolors_nat)<-levels(plotdf$type)
  g.tmp<-ggplot(
    plotdf,
    aes(
      x=year,
      y=homrate,
      color=type,
      group=period
    )
  ) +
    geom_line(
      size=2
    ) +
    scale_color_manual(
      values=tmpcolors_nat,
      guide=F
    ) +
    xlab("") +
    xlim(range(tmpdf$year)) +
    ylab("Homicides per 100,000\n") +
    theme_black()
  tmpname<-tmptitle
  gs.list[[tmpname]]<-list(
    graph=g.tmp,
    filename=tmpname,
    width=8/1.5,
    height=6/1.5
  )
}


#########################################################
#########################################################

#over-time, just postwar increase
#many different measures
#homrate, census et al. 
#homrate, FBI
#violent crime rate, FBI
#property crime rate
#normalize to 1955 or so

#get fbi violent
#fbi
tmpdf<-read.csv(
  'homrates_longitudinal_fbi.csv',
  skip=8,
  stringsAsFactors=F
)
names(tmpdf)<-tolower(names(tmpdf))
tmpdf$year<-as.numeric(tmpdf$year)
tmprows<-!is.na(tmpdf$year)
tmpdf$homrate<-as.numeric(tmpdf$murder.and.nonnegligent.manslaughter.rate)
tmpdf$vcrt<-as.numeric(tmpdf$violent.crime.rate)
tmpdf$source<-"fbi"
tmpcols<-c("year","homrate","vcrt","source")
tmpdf<-tmpdf[tmprows,tmpcols]
tmpdf$source<-NULL
names(tmpdf)<-c("year","fbihom","fbivcrt")
tmpdf<-gather(
  tmpdf,
  var,val,
  fbihom:fbivcrt
)
tmpdfs[['fbiv']]<-tmpdf

#get fbi property
setwd(datadir); dir()
tmpdf<-read.csv(
  'fbidf_prop.csv',
  skip=4,
  stringsAsFactors=F
)
names(tmpdf)<-tolower(names(tmpdf))
tmpdf$year<-as.numeric(tmpdf$year)
tmpdf<-tmpdf[!is.na(tmpdf$year),]
names(tmpdf)<-c("year","fbipcrt")
tmpdf$fbipcrt<-as.numeric(tmpdf$fbipcrt)
tmpdf<-gather(
  tmpdf,
  var,val,
  fbipcrt
)
tmpdfs[['fbip']]<-tmpdf

#homdf
tmp<-homdf$source!="cdc" &
  !is.na(homdf$source)
homdf<-homdf[tmp,]
homdf$source<-NULL
names(homdf)<-c("year","cenhom")
homdf<-gather(
  homdf,
  var,val,
  cenhom
)

#put them together
plotdf<-rbind.fill(
  tmpdfs$fbiv,
  tmpdfs$fbip,
  homdf
)

#output
setwd(datadir)
write.csv(
  plotdf,
  "tab_longviolence.csv",
  row.names=F
)

#normalize
plotdf<-by(plotdf,plotdf$var,function(df) {
  df$val<-100 * df$val/df$val[df$year==1960]
  df
}) %>% rbind.fill


#censor by time to show rise
tmp<-plotdf$year%in%c(1955:1995)
plotdf<-plotdf[tmp,]
tapply(plotdf$val,plotdf$var,max) #at peak

plotdf$source<-str_extract(plotdf$var,"fbi|cen")
tmplevels<-c(
  "fbi",
  "cen"
)
tmplabels<-c(
  "FBI UCR",
  "Vital Stats"
)
plotdf$source<-factor(
  plotdf$source,
  tmplevels,
  tmplabels
)

plotdf$type<-str_extract(plotdf$var,"vcrt|pcrt|hom")
#this is for the loop
tmplist<-list(
  plotdf$type=="hom" & plotdf$source=="FBI UCR",
  plotdf$type%in%c("hom","vcrt") & plotdf$source=="FBI UCR",
  plotdf$type%in%c("hom","vcrt","pcrt") & plotdf$source=="FBI UCR"
)
tmplevels<-c(
  "vcrt",
  "pcrt",
  "hom"
)
tmplabels<-c(
  "Violent Crime",
  "Property Crime",
  "Homicide Rate"
)
plotdf$type<-factor(
  plotdf$type,
  tmplevels,
  tmplabels
)

#color
tmpcolors_pen<-c(
  red,
  blue,
  orange
)
names(tmpcolors_pen)<-
  levels(plotdf$type)

#linetyps
tmptypes<-c(
  "solid",
  "dashed"
)
names(tmptypes)<-
  levels(plotdf$source)

for(i in 1:length(tmplist)) {
  #i<-1
  tmp<-tmplist[[i]]
  g.tmp<-ggplot(
    plotdf[tmp,],
    aes(
      x=year,
      y=val,
      group=var,
      color=type,
      linetype=source
    )
  ) +
    geom_line(
      size=2
    ) +
    scale_color_manual(
      name="",
      values=tmpcolors_pen
    ) +
    scale_linetype_manual(
      values=tmptypes,
      guide=F
    ) +
    ylim(range(plotdf$val)) +
    xlim(range(plotdf$year)) +
    xlab("") +
    ylab("Level (1960=100)\n") +
    theme_black() +
    theme(
      axis.title.y = element_text(size=18),
      axis.text.x = element_text(size=18),
      axis.text.y = element_text(size=18),
      legend.position='bottom',
      legend.direction='horizontal'
    )
  tmpname<-paste0("fig_crimerise",i,".pdf")
  gs.list[[tmpname]]<-list(
    graph=g.tmp,
    filename=tmpname,
    width=8,
    height=6
  )
}

#plot


#make them show up in order





#########################################################
#########################################################

#cross-national
setwd(datadir); dir()
tmpdf<-read.csv(
  'homrates_crossnational.csv',
  stringsAsFactors=F
)
head(tmpdf)

tmp<-!is.na(tmpdf$countryname) &
  !is.na(tmpdf$homrate)
tmpdf<-tmpdf[tmp,]
nrow(tmpdf)
tmpdf$merge<-tolower(tmpdf$countryname)

#keep big countries
popdf<-read.csv(
  'populations_crossnational.csv',
  stringsAsFactors=F
)
popdf$merge<-tolower(popdf$country)

#not that many missing in merge, 
#ignore for now, since we restrict to big counties
tmp<-tmpdf$merge%in%popdf$merge
sum(tmp); sum(!tmp)
tmpdf$merge[!tmp]
tmp<-popdf$merge%in%tmpdf$merge
sum(tmp); sum(!tmp)
popdf$merge[!tmp]
tmpdf<-merge(
  tmpdf[,c("merge","countryname","homrate","region","subregion")],
  popdf[,c("merge","country","pop16")]
)
tmpdf$pop<-str_replace_all(tmpdf$pop16,"\\,","") %>% as.numeric
tmp<-tmpdf$pop>5*10^6 #countries w/ more than 5 million
tmpdf<-tmpdf[tmp,]

#split into developed, ROW, USA
setwd(metadir)
oecdcountries<-readLines('oecd_homcountries.txt')
tmpdf$group<-"row"
tmp<-tmpdf$countryname%in%oecdcountries
tmpdf$group[tmp]<-"oecd"
tmp<-tmpdf$countryname=="United States"
tmpdf$group[tmp]<-"usa"

#where does US rank, percentile wise
percentile<-ecdf(tmpdf$homrate[tmpdf$group=="row"])
percentile(tmpdf$homrate[tmpdf$group=="usa"]) #60th percnetile of all countries
percentile<-ecdf(tmpdf$homrate[tmpdf$group=="oecd"])
percentile(tmpdf$homrate[tmpdf$group=="usa"]) #bigger than all oecd countries
tmpdf$homrate[tmpdf$group=="usa"]/max(tmpdf$homrate[tmpdf$group=="oecd"]) #almost 3 times max
tmpdf$homrate[tmpdf$group=="usa"]/min(tmpdf$homrate[tmpdf$group=="oecd"]) #almost 19 times min

#save it
plotdfs[['crossnational_homrates']]<-tmpdf
setwd(filesdir)
write.csv(
  tmpdf,
  'homrates_countries.csv',
  row.names=F
)

plotdf<-tmpdf
tmplevels<-c(
  "usa","oecd","row"
)
tmplabels<-c(
  "USA","Developed","Rest"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplabels
)

tmpcolors_eel<-c(
  orange,
  white,
  white
)
names(tmpcolors_eel)<-
  levels(plotdf$group)

#make violinplot
ggplot(
  plotdf,
  aes(
    x=group,
    y=homrate
  )
) + 
  geom_violin() +
  scale_color_manual(
    name="",
    values=tmpcolors_eel,
    guide=F
  ) +
  theme_black() +
  scale_y_continuous(trans='log10') +
  xlab("") +
  ylab("Homicides per 100,000\n")

tmpname<-"fig_homrates_crossnational_nousa.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#make violinplot
g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=homrate
  )
) + 
  geom_violin() +
  geom_point(
    data=plotdf[plotdf$group=="USA",],
    aes(
      x=group,
      y=log(homrate)
    ),
    color=orange,
    fill=orange,
    size=10
  ) + 
  scale_color_manual(
    name="",
    values=tmpcolors_eel,
    guide=F
  ) +
  theme_black() +
  xlab("") +
  scale_y_continuous(trans='log10') +
  ylab("Homicides per 100,000\n")

tmpname<-"fig_homrates_crossnational_usa.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#########################################################
#########################################################

#cross-space
#state
setwd(datadir); dir()
tmpdf<-read.csv(
  'deathsfromassault_state.txt',
  stringsAsFactors=F,
  sep="\t"
)
names(tmpdf)<-tolower(names(tmpdf))
tmp<-tmpdf$state!=""
tmpdf<-tmpdf[tmp,]
tmpdf$homrate<-as.numeric(tmpdf$crude.rate)
tmpdf<-tmpdf[tmpdf$year==2016,]
tmpdf$state[is.na(tmpdf$homrate)] #five states can't be computed

df1<-tmpdf
df1$group<-"usa"
df1$countryname<-df1$state
df2<-plotdfs$crossnational_homrates
df2<-df2[df2$group!="usa",]
tmpvars<-c("countryname","homrate","group")
plotdf<-rbind.fill(
  df1[,tmpvars],
  df2[,tmpvars]
)

tmplevels<-c(
  "usa","oecd","row"
)
tmplabels<-c(
  "USA (States)","Developed","Rest"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplabels
)

tmpcolors_cat<-c(
  orange,
  white,
  white
)
names(tmpcolors_cat)<-
  levels(plotdf$group)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=homrate,
    color=group,
    fill=group
  )
) + 
  geom_violin() +
  scale_color_manual(
    name="",
    values=tmpcolors_cat,
    guide=F
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors_cat,
    guide=F
  ) +
  theme_black() +
  scale_y_continuous(trans='log10') +
  xlab("") +
  ylab("Homicides per 100,000\n")

tmpname<-"fig_homrates_states.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#county
setwd(datadir); dir()
tmpdf<-read.csv(
  'deathsfromassault_counties.txt',
  stringsAsFactors=F,
  sep="\t"
)
names(tmpdf)<-tolower(names(tmpdf))
tmp<-tmpdf$county!=""
tmpdf<-tmpdf[tmp,]
tmpdf$homrate<-as.numeric(tmpdf$crude.rate)
tmpdf<-tmpdf[!is.na(tmpdf$homrate),]
tmpdf<-by(tmpdf,tmpdf$county,function(df) {
  #df<-tmpdf[tmpdf$county=="Palm Beach County, FL" & !is.na(tmpdf$county),]
  df<-df[order(df$year),]
  data.frame(
    county=unique(df$county),
    county.code=unique(df$county.code),
    year=max(df$year),
    homrate=df$homrate[df$year==max(df$year)],
    population=df$population[df$year==max(df$year)],
    stringsAsFactors=F
  )
}) %>% rbind.fill

setwd(filesdir); dir()
write.csv(
  tmpdf,
  "homrates_counties.csv",
  row.names=F
)

#drop counties less than 10000
nrow(tmpdf)
tmpdf<-tmpdf[tmpdf$population>10^5,]
nrow(tmpdf) #226, dropping 4

df1<-tmpdf
df1$group<-"usa"
df1$countryname<-df1$county
df2<-plotdfs$crossnational_homrates
df2<-df2[df2$group!="usa",]
tmpvars<-c("countryname","homrate","group")
plotdf<-rbind.fill(
  df1[,tmpvars],
  df2[,tmpvars]
)

tmplevels<-c(
  "usa","oecd","row"
)
tmplabels<-c(
  "USA (Counties)","Developed","Rest"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplabels
)

tmpcolors_dog<-c(
  orange,
  white,
  white
)
names(tmpcolors_dog)<-
  levels(plotdf$group)


g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=homrate,
    color=group,
    fill=group
  )
) + 
  geom_violin() +
  scale_color_manual(
    name="",
    values=tmpcolors_dog,
    guide=F
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors_dog,
    guide=F
  ) +
  theme_black() +
  scale_y_continuous(trans='log10') +
  xlab("") +
  ylab("Homicides per 100,000\n")

tmpname<-"fig_homrates_counties.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)


#########################################################
#########################################################

#cross-group
#race
#raceXsex

setwd(datadir); dir()
tmpdf<-read.csv(
  'deathsfromassault_raceXsex.txt',
  stringsAsFactors=F,
  sep="\t"
)

names(tmpdf)<-tolower(names(tmpdf))
tmp<-tmpdf$race!=""
tmpdf<-tmpdf[tmp,]
names(tmpdf)<-c(
  "notes",
  "race",
  "race_code",
  "gender",
  "gender_code",
  "deaths",
  "population",
  "homrate",
  "homrate_min",
  "homrate_max"
)
tmpdf$homrate<-as.numeric(tmpdf$homrate)
tmpdf$homrate_min<-as.numeric(tmpdf$homrate_min)
tmpdf$homrate_max<-as.numeric(tmpdf$homrate_max)
tmpdf<-tmpdf[!is.na(tmpdf$homrate),]

#show race and raceXsex
tmpdf$race %>% unique
tmp<-str_detect(tmpdf$race,"Black") |
  tmpdf$race=="White"
tmpdf<-tmpdf[tmp,]

#trim
tmp<-tmpdf$gender==""
plotdf<-tmpdf[tmp,]

tmplevels<-c(
  "White",
  "Black or African American"
)
tmplabels<-c(
  "White",
  "Black"
)
plotdf$race<-factor(
  plotdf$race,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=race,
    y=homrate,
    ymin=homrate_min,
    ymax=homrate_max
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    position=position_dodge(width = 0.3),
    color=blue,
    fill=blue
  ) +
  geom_errorbar(
    color='white',
    width=0.1,
    size=0.5,
    position=position_dodge(width = 0.3)
  ) +
  theme_black() + 
  xlab("") +
  ylab("Homicide Victimization Rate per 100,000 \n")

tmpname<-"fig_homrates_race.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)


#for race and raceXclass,
#we need to rely on NCVS
#i use summaries rather than raw data
#b/c raw data are a mess
fulldf<-read.csv(
  "ncvs_summaries.csv",
  stringsAsFactors=F
)
#get raceXclass 2010-2015 averages
fulldf<-data.table(fulldf)
fulldf<-fulldf[
  ,
  list(
    rate=10^3 * sum(count)/sum(number)
  )
  ,
  by=c(
    "race",
    "class",
    "year"
  )
  ]

fulldf$group<-""
tmp<-fulldf$race=="black" & 
  fulldf$class=="poor"
fulldf$group[tmp]<-"Black, <15k"
tmp<-fulldf$race=="white" & 
  fulldf$class=="poor"
fulldf$group[tmp]<-"White, <15k"
tmp<-fulldf$race=="black" & 
  fulldf$class=="rich"
fulldf$group[tmp]<-"Black, >75k"
tmp<-fulldf$race=="white" & 
  fulldf$class=="rich"
fulldf$group[tmp]<-"White, >75k"
tmp<-fulldf$group!=""
fulldf<-fulldf[tmp,]


#1993-1998
plotdf<-fulldf[
  year%in%c(1993:1998)
  ,
  list(
    rate=mean(rate,na.rm=T)
  )
  ,
  by='group'
  ]

tmplevels<-c(
  "White, >75k",
  "Black, >75k",
  "White, <15k",
  "Black, <15k"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplevels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=rate
  )
) + 
  geom_bar(
    stat='identity',
    color=blue,
    fill=blue,
    width=0.3
  ) +
  theme_black() +
  xlab("") +
  ylab("Serious Violent Victimizations per 1,000")

tmpname<-"fig_vicrates_raceXclass_1993to1998.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#2010-2015
plotdf<-fulldf[
  year%in%c(2010:2015)
  ,
  list(
    rate=mean(rate,na.rm=T)
  )
  ,
  by='group'
  ]

tmplevels<-c(
  "White, >75k",
  "Black, >75k",
  "White, <15k",
  "Black, <15k"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplevels
)
g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=rate
  )
) + 
  geom_bar(
    stat='identity',
    color=blue,
    fill=blue,
    width=0.3
  ) +
  theme_black() +
  xlab("") +
  ylab("Serious Violent Victimizations per 1,000")

tmpname<-"fig_vicrates_raceXclass_2010to2015.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)


#make four loess plots,
#one for each of these

plotdf<-fulldf

tmplevels<-c(
  "White, >75k",
  "Black, >75k",
  "White, <15k",
  "Black, <15k"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplevels
)
tmpcolors_ostrich<-c(
  red,
  blue,
  orange,
  green
)
names(tmpcolors_ostrich)<-levels(plotdf$group)

fung<-function(g) {
  g + geom_smooth(
    se=F
  ) +
    theme_black() +
    scale_color_manual(
      name="",
      values=tmpcolors_ostrich
    ) +
    theme(
      legend.position='bottom',
      legend.direction = 'horizontal'
    ) +
    ylim(0,60) +
    xlab("") +
    ylab("Rate of Serious Violent Victimization, 1993-2015\n")
}

mylevels<-c("White, >75k")
g<-ggplot(
  plotdf[plotdf$group%in%mylevels,],
  aes(
    x=year,
    y=rate,
    group=group,
    color=group
  )
)
g.tmp<-fung(g)
tmpname<-"fig_vicrates_raceXclass_1.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)


mylevels<-c(
  mylevels,
  "Black, >75k"
)
g<-ggplot(
  plotdf[plotdf$group%in%mylevels,],
  aes(
    x=year,
    y=rate,
    group=group,
    color=group
  )
)
g.tmp<-fung(g)
tmpname<-"fig_vicrates_raceXclass_2.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)


mylevels<-c(
  mylevels,
  "White, <15k"
)
g<-ggplot(
  plotdf[plotdf$group%in%mylevels,],
  aes(
    x=year,
    y=rate,
    group=group,
    color=group
  )
)
g.tmp<-fung(g)
tmpname<-"fig_vicrates_raceXclass_3.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)


mylevels<-c(
  mylevels,
  "Black, <15k"
)
g<-ggplot(
  plotdf[plotdf$group%in%mylevels,],
  aes(
    x=year,
    y=rate,
    group=group,
    color=group
  )
)
g.tmp<-fung(g)
tmpname<-"fig_vicrates_raceXclass_4.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)




#########################################################
#########################################################


#output graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  print(
    paste0(
      "saving ",i," of ",length(this.sequence)
    )
  )
  thiselement<-gs.list[[i]]
  ggsave(
    filename="tmp.pdf",
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height
  )
  #embed font
  embed_fonts(
    file="tmp.pdf",
    outfile=thiselement$filename
  )
  file.remove(
    "tmp.pdf"
  )
  Sys.sleep(1)
}

