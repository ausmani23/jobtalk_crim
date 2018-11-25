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

#prisons

#over-time
setwd(datadir)
tmpdf<-read.csv(
  'incrates_subnationalstate.csv',
  stringsAsFactors=F
)
head(tmpdf)
tmp<-tmpdf$statename=="United States" &
  !is.na(tmpdf$statename)
plotdf<-tmpdf[tmp,]
head(plotdf)
setwd(datadir)

#get the point of inflection
#when inrates increase consistently for many years
#this turns out to be 27 consistent years of growth
tmp<-c(NA,diff(plotdf$incrt_t_jur))
tmp<-as.numeric(tmp>0)
y<-rle(tmp)
y$lengths
yrsin<-cumsum(y$lengths)[which(y$lengths==27) - 1] 
breakyear<-min(plotdf$year) + yrsin
plotdf$color<-"old"
plotdf$color[plotdf$year>=breakyear]<-"new"
#add duplicate year
newdf<-plotdf[plotdf$year==breakyear,]
newdf$color<-"old"
plotdf<-rbind.fill(newdf,plotdf)

tmpcolors_zebra<-c("white",orange)
names(tmpcolors_zebra)<-c("old","new")

#first
g.tmp<-ggplot(
  plotdf[plotdf$year<1973,],
  aes(
    x=year,
    y=incrt_t_jur,
    color=color
  )
) +
  geom_line(
    size=2
  ) +
  theme_black() +
  scale_color_manual(
    name="",
    values=tmpcolors_zebra,
    guide=F
  ) +
  xlim(
    min(plotdf$year),
    max(plotdf$year)
  ) + 
  ylim(
    min(plotdf$incrt_t_jur),
    max(plotdf$incrt_t_jur)
  ) +
  xlab("") +
  ylab("State and Federal Prisoners per 100,000\n")


tmpname<-"fig_prison_preturn.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#second
g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=incrt_t_jur,
    color=color
  )
) +
  geom_line(
    size=2
  ) +
  theme_black() +
  scale_color_manual(
    name="",
    values=tmpcolors_zebra,
    guide=F
  ) +
  xlim(
    min(plotdf$year),
    max(plotdf$year)
  ) + 
  ylim(
    min(plotdf$incrt_t_jur),
    max(plotdf$incrt_t_jur)
  ) +
  xlab("") +
  ylab("State and Federal Prisoners per 100,000\n")

tmpname<-"fig_prison_punturn.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#the actual rate, from impdf
setwd(datadir); dir()
plotdf<-read.csv(
  'impdf.csv',
  stringsAsFactors=F
)
tmp<-!is.na(plotdf$imprate_cp)
plotdf<-plotdf[tmp,]
roworder<-order(plotdf$year)
plotdf<-plotdf[roworder,]
plotdf$X<-NULL
tmp<-c(NA,diff(plotdf$imprate_cp))
tmp<-as.numeric(tmp>0)
y<-rle(tmp)
tmpind<-which(y$lengths==max(y$lengths[y$values==1],na.rm=T) & y$values==1) - 1
yrsin<-cumsum(y$lengths)[tmpind] 
breakyear<-min(plotdf$year) + yrsin
plotdf$color<-"old"
plotdf$color[plotdf$year>=breakyear]<-"new"
#add duplicate year
newdf<-plotdf[plotdf$year==breakyear,]
newdf$color<-"old"
plotdf<-rbind.fill(newdf,plotdf)

tmpcolors_zebra<-c("white",orange)
names(tmpcolors_zebra)<-c("old","new")

#restrict range
tmp<-plotdf$year>=1925
plotdf<-plotdf[tmp,]

g.tmp<-ggplot(
  plotdf[plotdf$year<1973,],
  aes(
    x=year,
    y=imprate_cp,
    color=color
  )
) +
  geom_line(
    size=2
  ) +
  theme_black() +
  scale_color_manual(
    name="",
    values=tmpcolors_zebra,
    guide=F
  ) +
  xlim(
    min(plotdf$year),
    max(plotdf$year)
  ) + 
  ylim(
    min(plotdf$imprate_cp),
    max(plotdf$imprate_cp)
  ) +
  xlab("") +
  ylab("Inmates per 100,000\n")

tmpname<-"fig_inmates_preturn.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=imprate_cp,
    color=color
  )
) +
  geom_line(
    size=2
  ) +
  theme_black() +
  scale_color_manual(
    name="",
    values=tmpcolors_zebra,
    guide=F
  ) +
  xlim(
    min(plotdf$year),
    max(plotdf$year)
  ) + 
  ylim(
    min(plotdf$imprate_cp),
    max(plotdf$imprate_cp)
  ) +
  xlab("") +
  ylab("Inmates per 100,000\n")

tmpname<-"fig_inmates_punturn.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)


#########################################################
#########################################################

#prisons

#cross-national
setwd(datadir); dir()
tmpdf<-read.csv(
  'prisonrates_crossnational.csv',
  stringsAsFactors=F
)
head(tmpdf)

tmp<-!is.na(tmpdf$countryname) &
  !is.na(tmpdf$prisonpop) & 
  !is.na(tmpdf$prisonrate)
tmpdf<-tmpdf[tmp,]

#sort into groups of big countries
setwd(metadir); dir()
oecd_countries<-readLines('oecd_countries.txt')
tmpdf$pop<-tmpdf$prisonpop/(tmpdf$prisonrate/100000)
tmpdf$bigcountries<-tmpdf$pop>5*10^6 #set threshold
#three groups
tmpdf$group<-"row"
#oecd
tmp<-tmpdf$countryname%in%oecd_countries
tmpdf$group[tmp]<-"oecd"
#us
tmp<-tmpdf$countryname=="United States of America"
tmpdf$group[tmp]<-"usa"
#drop all countries w/ less than X million
tmpdf$countryname[!tmpdf$bigcountries]
tmpdf<-tmpdf[tmpdf$bigcountries,]
tmpdf$countryname %>% unique
#which countries have about 150-180
tmp<-tmpdf$group=="oecd"
tmpdf[tmp,]

#save it
plotdfs[['crossnational']]<-tmpdf
#save out
setwd(filesdir)
write.csv(
  tmpdf,
  'incrates_countries.csv',
  row.names=F
)

tmplevels<-c(
  "usa","oecd","row"
)
tmplabels<-c(
  "USA","Developed","Rest"
)
tmpdf$group<-factor(
  tmpdf$group,
  tmplevels,
  tmplabels
)

tmpcolors_rhino<-c(
  orange,
  blue,
  green
)
names(tmpcolors_rhino)<-
  levels(tmpdf$group)

tmpsizes<-c(
  5,1,1
)
names(tmpsizes)<-
  levels(tmpdf$group)

#make violinplot
g.tmp<-ggplot(
  tmpdf,
  aes(
    x=group,
    y=prisonrate
  )
) + 
  geom_violin() +
  # geom_point(
  #   aes(
  #     color=group,
  #     size=group
  #   )
  # ) + 
  scale_color_manual(
    name="",
    values=tmpcolors_rhino,
    guide=F
  ) +
  scale_size_manual(
    name="",
    values=tmpsizes,
    guide=F
  ) +
  theme_black() +
  ylim(0,700) + 
  xlab("") +
  ylab("Prisoners per 100,000\n")

tmpname<-"fig_prison_crossnational_nousa.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#make violinplot
g.tmp<-ggplot(
  tmpdf,
  aes(
    x=group,
    y=prisonrate
  )
) + 
  geom_violin(
    color=white,
    fill=white
  ) +
  geom_point(
    data=tmpdf[tmpdf$group=="USA",],
    aes(
      x=group,
      y=prisonrate
    ),
    color=orange,
    fill=orange,
    size=10
  ) + 
  guides(color=F,size=F) +
  theme_black() +
  ylim(0,700) + 
  xlab("") +
  ylab("Prisoners per 100,000\n")

tmpname<-"fig_prison_crossnational_wusa.pdf"
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
#county

#cross-state, 2011 w/ ROW for comparison
setwd(datadir)
tmpdf<-read.csv(
  'incrates_subnationalstate.csv',
  stringsAsFactors=F
)
tmp<-!is.na(tmpdf$incrt_t_jur) &
  !tmpdf$state_alpha2%in%c("ST","FE","DC") &
  tmpdf$year==2011
tmpdf<-tmpdf[tmp,]
head(tmpdf)

#combine
df1<-plotdfs$crossnational
df2<-tmpdf
#we can apply an inflator to get appx matching across
usrate<-df1$prisonrate[df1$group=="usa"]
inflator<-usrate/df2$incrt_t_jur[df2$state_alpha2=="US"]
df2$prisonrate<-df2$incrt_t_jur * inflator
df2<-df2[!is.na(df2$state_alpha2=="US"),] #drpo us average
df2$countryname<-df2$state_alpha2
df2$group<-"usa"
plotdf<-rbind.fill(
  df1[,c("countryname","prisonrate","group")],
  df2[,c("countryname","prisonrate","group")]
)
head(plotdf)

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

tmpcolors_ali<-c(
  orange,
  white,
  white
)
names(tmpcolors_ali)<-levels(plotdf$group)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=prisonrate,
    color=group,
    fill=group
  )
) + 
  geom_violin() +
  scale_color_manual(
    values=tmpcolors_ali,
    name="",
    guide=F
  ) +
  scale_fill_manual(
    values=tmpcolors_ali,
    name="",
    guide=F
  ) +
  guides(size=F) +
  theme_black() +
  #ylim(0,700) + 
  xlab("") +
  ylab("Prisoners per 100,000\n")

tmpname<-"fig_prison_subnational_states.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

###cross-county, 2015
setwd(datadir); dir()
tmpdf<-read.csv(
  'incrates_subnationalcounty.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$year==2015 & 
  !is.na(tmpdf$total_prison_pop)
tmpdf<-tmpdf[tmp,]

#we also want raw pop rather than 1565
setwd(datadir); dir()
popdf<-read.csv(
  'population_county.csv',
  stringsAsFactors=F
)
tmp<-popdf$year==2010
popdf<-popdf[tmp,]
head(popdf)

#harmonize fips codes
popdf$county_fips<-as.character(popdf$county_fips)
tmplengths<-str_length(popdf$county_fips)
tmp<-tmplengths==1
popdf$county_fips[tmp]<-
  paste0("00",popdf$county_fips[tmp])
tmp<-tmplengths==2
popdf$county_fips[tmp]<-
  paste0("0",popdf$county_fips[tmp])
popdf$fips<-paste0(
  popdf$state_fips,
  popdf$county_fips
)
tmpdf$fips<-as.character(tmpdf$fips)
#about 2700 match, which is good enough
tmp<-popdf$fips%in%tmpdf$fips
sum(tmp); sum(!tmp)
popdf$fips[!tmp]
tmp<-tmpdf$fips%in%popdf$fips
sum(tmp); sum(!tmp)
tmpdf$fips[!tmp]

#calc incrate
tmpdf<-merge(
  popdf,
  tmpdf,
  by='fips'
)
tmpdf$incrate <- 
  10^5 * (tmpdf$total_prison_pop + tmpdf$total_jail_pop) /
  tmpdf$pop

#save out
setwd(filesdir)
write.csv(
  tmpdf,
  'incrates_counties.csv',
  row.names=F
)

#drop all counties w/ less than 100,000
roworder<-order(-tmpdf$pop)
tmpdf<-tmpdf[roworder,]
tmpdf<-tmpdf[tmpdf$pop>10^5,]
min(tmpdf$pop)
max(tmpdf$pop)


#combine
df1<-plotdfs$crossnational
df1<-df1[!df1$group=="usa",]
df2<-tmpdf
df2$countryname<-df2$county_name
df2$group<-"usa"
df2$prisonrate<-df2$incrate
tmpvars<-c("countryname","prisonrate","group")
plotdf<-rbind.fill(
  df1[,tmpvars],
  df2[,tmpvars]
)
head(plotdf); tail(plotdf)

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

tmpcolors_zoo<-c(
  orange,
  white,
  white
)
names(tmpcolors_zoo)<-levels(plotdf$group)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=prisonrate,
    color=group,
    fill=group
  )
) + 
  geom_violin() +
  scale_color_manual(
    values=tmpcolors_zoo,
    name="",
    guide=F
  ) +
  scale_fill_manual(
    values=tmpcolors_zoo,
    name="",
    guide=F
  ) +
  guides(size=F) +
  theme_black() +
  xlab("") +
  ylab("Prisoners per 100,000\n")

tmpname<-"fig_prison_subnational_counties.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#########################################################
#########################################################

#cross-group

#copied from wildeman and western, 2009
setwd(datadir); dir()
tmpdf<-read.csv(
  'ww2009_raceXclass.csv',
  stringsAsFactors=F
)
head(tmpdf)
tmpdf<-gather(
  tmpdf,
  generation,
  rate,
  X1945.1949:X1975.1979
)

#subset for race
tmp<-tmpdf$race=="white" & 
  tmpdf$ed=="total"
tmpdf$group[tmp]<-"White"
tmp2<-tmpdf$race=="black" & 
  tmpdf$ed=="total"
tmpdf$group[tmp2]<-"Black"
tmp<-tmp | tmp2 
tmp<-tmp & tmpdf$generation=="X1975.1979"
plotdf<-tmpdf[tmp,]

tmplevels<-c(
  "White",
  "Black"
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
    fill=blue,
    width=0.3,
    position=position_dodge(width = 0.3)
  ) +
  theme_black() +
  xlab("") + 
  ylab("p(Incarceration) for Men Born 1975-1979\n") +
  theme(
    legend.position='bottom',
    legend.direction = 'horizontal',
    axis.title.y = element_text(size=12), 
    axis.text.x = element_text(size=10)
  )

tmpname<-"fig_prison_race_ww.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#subset for raceXclass
tmp<-tmpdf$race=="white" & 
  tmpdf$ed=="somecollege"
tmpdf$group[tmp]<-"White, College"
tmp2<-tmpdf$race=="black" & 
  tmpdf$ed=="somecollege"
tmpdf$group[tmp2]<-"Black, College"
tmp3<-tmpdf$race=="white" & 
  tmpdf$ed=="dropouts"
tmpdf$group[tmp3]<-"White, HS Dropout"
tmp4<-tmpdf$race=="black" & 
  tmpdf$ed=="dropouts"
tmpdf$group[tmp4]<-"Black, HS Dropout"
tmp<-tmp | tmp2 | tmp3 | tmp4
tmp<-tmp & tmpdf$generation=="X1975.1979"
plotdf<-tmpdf[tmp,]

tmplevels<-c(
  "White, College",
  "Black, College",
  "White, HS Dropout",
  "Black, HS Dropout"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplevels
)

#race 
g.tmp<-ggplot(
  plotdf,
  aes(
    x=group,
    y=rate
  )
) +
  geom_bar(
    stat='identity',
    fill=blue,
    width=0.3,
    position=position_dodge(width = 0.3)
  ) +
  theme_black() +
  xlab("") + 
  ylab("p(Incarceration) for Men Born 1975 to 1979\n") +
  theme(
    legend.position='bottom',
    legend.direction = 'horizontal', 
    axis.title.y = element_text(size=12)
  )

tmpname<-"fig_prison_raceXclass_ww.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8/1.5,
  height=6/1.5
)

#done from nlsy, myself
#raceXclass
setwd(
  file.path(datadir,"nlsy")
); dir()
tmpdf<-read.csv(
  'nlsydata_extract.csv',
  stringsAsFactors=F
)
head(tmpdf)

# #also load weights
# weightsdf<-read.csv(
#   'weights.csv',
#   stringsAsFactors=F
# )
# #merge
# tmpdf<-merge(
#   tmpdf,
#   weightsdf
# )

#get weights at the end

#restriction:
# #ppl w/ valid weights in final year
# tmp<-tmpdf$weight!=0
# tmpdf<-tmpdf[tmp,]
# nrow(tmpdf) #abuot 7000
# #everyone's last interview was in 2015/2016, which is righ
# str_extract(tmpdf$arrest_lastinterview,"[0-9]{4}") %>% 
#   as.numeric %>% table

#code race/sex/ed
tmpdf$race_f<-NA
tmp<-tmpdf$race==1 
tmpdf$race_f[tmp]<-"black"
tmp<-tmpdf$race==2
tmpdf$race_f[tmp]<-"hispanic"
tmp<-tmpdf$race==4
tmpdf$race_f[tmp]<-"white"
tmpdf$ed_f<-NA
tmp<-tmpdf$highestdegree==0 &
  !is.na(tmpdf$highestdegree)
tmpdf$ed_f[tmp]<-"hsdrop"
tmp<-tmpdf$highestdegree%in%c(1,2) &
  !is.na(tmpdf$highestdegree)
tmpdf$ed_f[tmp]<-"hsgrad"
tmp<-tmpdf$highestdegree>2 &
  !is.na(tmpdf$highestdegree)
tmpdf$ed_f[tmp]<-"college"
table(tmpdf$ed_f)
tmp<-tmpdf$sex==1
tmpdf$sex_f[tmp]<-"male"
tmp<-tmpdf$sex==2
tmpdf$sex_f[tmp]<-"female"

#examine attrition
tmpdf$interviewyear<-str_extract(tmpdf$arrest_lastinterview,"[0-9]{4}") %>%
  as.numeric
tmpdf$age_lastinterview<-tmpdf$interviewyear - tmpdf$bday_y
sapply(13:30,function(x) sum(tmpdf$age_lastinterview>=x))
#based on this, I reason: i want everyone who was at least 25 at the time of their last interview
#it is more likely to include the dropouts, which probably raises p(arrest)
#it is less likely to give people time to be arrested, which probably reduces p(arrest)
tmp<-tmpdf$age_lastinterview>=25
tmpdf<-tmpdf[tmp,]

#get custom weights for these ID's
tmpweights<-paste0(tmpdf$pubid)
write(tmpweights,"tmpweights.txt")

#get custom weights for these dis
weightsdf<-read.csv(
  'weights.csv',
  stringsAsFactors=F
)
tmpdf<-merge(
  tmpdf,
  weightsdf
)

#create vars
tmpdf$arrested<-tmpdf$total_arrests>=1
tmpdf$incarcerated<-tmpdf$total_incarcerations>=1
weighted.se<-function(x,w) {
  #x<-tmpdf$arrested
  #w<-tmpdf$w
  xm<-weighted.mean(x,w)
  wts<-w/sum(w) #normalized weights
  wsd<-sqrt(sum(wts * (x - xm)^2))
  wsd/sqrt(length(x)) #se
}

#get raceXsexXed probabilities
tmpdf<-data.table(tmpdf)
plotdf<-tmpdf[
  !is.na(tmpdf$race_f) &
    !is.na(tmpdf$sex_f) & 
    !is.na(tmpdf$ed_f) &
    !is.na(tmpdf$arrested) & 
    !is.na(tmpdf$incarcerated)
  ,
  list(
    parrested_mean=weighted.mean(arrested,w),
    parrested_se=weighted.se(arrested,w),
    pincarcerated_mean=weighted.mean(incarcerated,w),
    pincarcerated_se=weighted.se(incarcerated,w),
    N=length(pubid)
  )
  ,
  by=c(
    "race_f",
    "sex_f",
    "ed_f"
  )
  ]

#reshape
plotdf<-gather(
  plotdf,
  var,
  val,
  parrested_mean:pincarcerated_se
)
plotdf$stat<-str_extract(plotdf$var,"mean|se")
plotdf$var<-str_replace(plotdf$var,"\\_mean|\\_se","")
plotdf<-spread(
  plotdf,
  stat,
  val
)

#subset
tmp<-plotdf$race_f=="white" & 
  plotdf$ed_f=="college" & 
  plotdf$sex_f=="male"
plotdf$x[tmp]<-"White, College"
tmp2<-plotdf$race_f=="black" & 
  plotdf$ed_f=="college" &
  plotdf$sex_f=="male"
plotdf$x[tmp2]<-"Black, College"
tmp3<-plotdf$race_f=="white" & 
  plotdf$ed_f=="hsdrop" & 
  plotdf$sex_f=="male"
plotdf$x[tmp3]<-"White, HS Dropout"
tmp4<-plotdf$race_f=="black" & 
  plotdf$ed_f=="hsdrop" & 
  plotdf$sex_f=="male"
plotdf$x[tmp4]<-"Black, HS Dropout"
tmp<-tmp | tmp2 | tmp3 | tmp4
plotdf<-plotdf[tmp,]

tmplevels<-c(
  "White, College",
  "Black, College",
  "White, HS Dropout",
  "Black, HS Dropout"
)
plotdf$x<-factor(
  plotdf$x,
  tmplevels,
  tmplevels
)

tmplevels<-c(
  "parrested",
  "pincarcerated"
)
tmplabels<-c(
  "Arrest",
  "Incarceration"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

tmpcolors2<-c(red,blue)
names(tmpcolors2)<-levels(plotdf$group)

#race 
g.tmp<-ggplot(
  plotdf,
  aes(
    x=x,
    y=mean,
    ymin=mean-2*se,
    ymax=mean+2*se,
    group=var,
    fill=var,
    color=var
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    position=position_dodge(width = 0.3)
  ) +
  geom_errorbar(
    color='white',
    width=0.1,
    size=1.25,
    position=position_dodge(width = 0.3)
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors2
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors2
  ) +
  theme_black() +
  xlab("") + 
  ylab("p(Event) for Men Age 30-35\n") +
  theme(
    legend.position='bottom',
    legend.direction = 'horizontal'
  )

tmpname<-"fig_prison_raceXclass_nlsy.pdf"
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

