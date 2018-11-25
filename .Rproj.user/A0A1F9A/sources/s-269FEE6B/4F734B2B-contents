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

#crimepun correlations
setwd(filesdir); dir()

#CROSS-NATIONAL
#calc correlations cross-nationally
homdf<-read.csv(
  'homrates_countries.csv',
  stringsAsFactors=F
)
incdf<-read.csv(
  'incrates_countries.csv',
  stringsAsFactors=F
)
homdf$countryname<-tolower(homdf$countryname)
incdf$countryname<-tolower(incdf$countryname)
#match countries
head(homdf); head(incdf)
tmp<-incdf$countryname%in%homdf$countryname
sum(tmp); sum(!tmp)
incdf$countryname[!tmp]
tmp<-homdf$countryname%in%incdf$countryname
sum(tmp); sum(!tmp)
homdf$countryname[!tmp]

#make a few name changes
tmplist<-list(
  c("hong kong  (china)","hong kong"),
  c("united kingdom: england & wales","united kingdom"),
  c("russian federation","russia"),
  c("united states of america","united states"),
  c("cote d'ivoire","ivory coast"),
  c("democratic republic of congo","congo"),
  c("myanmar (formerly burma)","myanmar"),
  c("republic of (south) korea","south korea")
)
for(i in 1:length(tmplist)) {
  tmp<-incdf$countryname==tmplist[[i]][1]
  incdf$countryname[tmp]<-tmplist[[i]][2]
}
tmpdf<-merge(
  homdf[,c("countryname","homrate")],
  incdf[,c("countryname","prisonrate")]
)

tmpdf$homrate_std<-scale(tmpdf$homrate)
tmpdf$prisonrate_std<-scale(tmpdf$prisonrate)
cor(tmpdf$homrate,tmpdf$prisonrate) #0.53
m.tmp<-lm(
  data=tmpdf,
  formula=prisonrate_std ~ homrate_std
)
summary(m.tmp)
m.log<-lm(
  data=tmpdf,
  formula=log(prisonrate) ~ log(homrate)
)
summary(m.log)

#find high residuals;
#this is diff to interpret,
#b/c counties should be CZ's
tmpdf$residual<-m.log$residuals
roworder<-order(tmpdf$residual)
tmpdf<-tmpdf[roworder,]
tmpdf$residrank<-1:nrow(tmpdf)
tmpdf$underpris<-1:nrow(tmpdf)
tmpdf$overpris<-nrow(tmpdf):1

#add highest/lowest 3 residual countries
tmpdf$cat<-"general"
tmpdf$cat[tmpdf$underpris%in%1:3]<-"underpris"
tmpdf$cat[tmpdf$overpris%in%1:3]<-"overpris"

tmplevels<-c("general","underpris","overpris")
tmpdf$cat<-factor(
  tmpdf$cat,
  tmplevels,
  tmplevels
)

tmpcolors_goat<-c(
  white,orange,orange
)
names(tmpcolors_goat)<-levels(tmpdf$cat)


g.tmp<-ggplot(
  tmpdf,
  aes(
    x=homrate,
    y=prisonrate,
    color=cat
  )
) +
  geom_point() + 
  geom_text(
    data=tmpdf[tmpdf$cat!="general",],
    aes(
      label=countryname
    ),
    position=position_jitter(width=0,height=0.3)
  ) +
  geom_smooth(
    method='lm',
    color=orange,
    se=F
  ) +
  scale_color_manual(
    values=tmpcolors_goat,
    guide=F
  ) +
  theme_black() +
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') +
  xlab("\nHomicides per 100,000") +
  ylab("Prisoners per 100,000\n") 


tmpname<-"fig_countrylm.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)

#########################################################
#########################################################

#CROSS-PLACE
setwd(filesdir); dir()
homdf<-read.csv(
  'homrates_counties.csv',
  stringsAsFactors=F
)
incdf<-read.csv(
  'incrates_counties.csv',
  stringsAsFactors=F
)

#match fips codes
names(homdf)<-c(
  "county_name",
  "fips",
  "year",
  "homrate",
  "pop"
)

#check match
tmp<-homdf$fips%in%incdf$fips
sum(tmp); sum(!tmp)
tmp<-incdf$fips%in%homdf$fips
sum(tmp); sum(!tmp)
#a bunch don't match, 
#but this is just approx, anyway

tmpdf<-merge(
  incdf[,c('county_name','fips','incrate')],
  homdf[,c('county_name','fips','homrate')],
  by='fips'
)

tmp<-!is.na(tmpdf$incrate) & !is.na(tmpdf$homrate)
tmpdf<-tmpdf[tmp,]

tmpdf$homrate_std<-scale(tmpdf$homrate)
tmpdf$incrate_std<-scale(tmpdf$incrate)
cor(tmpdf$homrate,tmpdf$incrate)
m.tmp<-lm(
  data=tmpdf,
  formula=incrate_std ~ homrate_std
)
summary(m.tmp)
m.log<-lm(
  data=tmpdf,
  formula=log(incrate) ~ log(homrate)
)
summary(m.log)

#find high residuals;
#this is diff to interpret,
#b/c counties should be CZ's
tmpdf$residual<-m.log$residuals
roworder<-order(tmpdf$residual)
tmpdf<-tmpdf[roworder,]
tmpdf$residrank<-1:nrow(tmpdf)
tmpdf$underpris<-1:nrow(tmpdf)
tmpdf$overpris<-nrow(tmpdf):1

#add highest/lowest 3 residual countries
tmpdf$cat<-"general"
tmpdf$cat[tmpdf$underpris%in%1:3]<-"underpris"
tmpdf$cat[tmpdf$overpris%in%1:3]<-"overpris"

tmplevels<-c("general","underpris","overpris")
tmpdf$cat<-factor(
  tmpdf$cat,
  tmplevels,
  tmplevels
)

tmpcolors_boo<-c(
  white,orange,orange
)
names(tmpcolors_boo)<-levels(tmpdf$cat)


g.tmp<-ggplot(
  tmpdf,
  aes(
    x=homrate,
    y=incrate,
    color=cat
  )
) +
  geom_point() + 
  geom_text(
    data=tmpdf[tmpdf$cat!="general",],
    aes(
      label=county_name.y
    ),
    position=position_jitter(width=0,height=0.3)
  ) +
  geom_smooth(
    method='lm',
    color=orange,
    se=F
  ) +
  scale_color_manual(
    values=tmpcolors_boo,
    guide=F
  ) +
  theme_black() +
  scale_x_continuous(trans='log2') +
  scale_y_continuous(trans='log2') +
  xlab("\nHomicides per 100,000") +
  ylab("Prisoners per 100,000\n") 

tmpname<-"fig_countylm.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)

########################################################
#########################################################

#test of granger causality w/ 
#over-time punitiveness
#and over-time crime
#(and also CR protest incidence)

setwd(datadir)
pundf<-read.csv(
  'fig_trends.csv',
  stringsAsFactors=F
)
vdf<-read.csv(
  'tab_longviolence.csv'
)
#add incarceration, too
setwd(datadir); dir()
incdf<-read.csv(
  'incrates_subnationalstate.csv',
  stringsAsFactors=F
)
tmp<-incdf$statename=="United States" &
  !is.na(incdf$statename)
incdf<-incdf[tmp,c("year","incrt_t_jur")]
incdf<-gather(
  incdf,
  var,val,
  incrt_t_jur
)

riotsdf<-haven::read_dta(
  'race_riot.dta'
)
riotsdf<-by(riotsdf,riotsdf$styr,function(df) {
  #df<-riotsdf[riotsdf$styr==56,]
  data.frame(
    var='riots',
    val=nrow(df[df$race1==1,]),
    year=paste0("19",unique(df$styr)) %>% as.numeric
  )
}) %>% rbind.fill

protestdf<-haven::read_dta(
  'Ethnic_Collect_Action.dta'
)
protestdf<-by(protestdf,protestdf$styr,function(df) {
  #df<-riotsdf[riotsdf$styr==56,]
  data.frame(
    var='protest',
    val=nrow(df[df$race1==1,]),
    year=paste0("19",unique(df$styr)) %>% as.numeric
  )
}) %>% rbind.fill

#quick plot of protests,riots/capiat
tmpvars<-c("year","var","val")
protestdf<-rbind.fill(
  riotsdf[,tmpvars],
  protestdf[,tmpvars]
)
setwd(datadir); dir()
popdf<-read.csv(
  'incrates_subnationalstate.csv',
  stringsAsFactors=F
)
tmp<-popdf$statename=="United States" &
  !is.na(popdf$statename)
popdf<-popdf[tmp,c("year","population_census")]
protestdf<-merge(
  protestdf,
  popdf
)
protestdf$val<-10^7*protestdf$val/protestdf$population_census
protestdf$population_census<-NULL

#combine these
pundf<-pundf[pundf$facet!="Conventional",]
pundf$var<-tolower(paste0(pundf$race,"_",pundf$dimension))
pundf$val<-pundf$mu
tmpvars<-c("year","var","val")
tmpdf<-rbind.fill(
  pundf[,tmpvars],
  vdf[,tmpvars],
  incdf[,tmpvars],
  protestdf[,tmpvars]
)

#which vars do I want?
tmpdf<-spread(
  tmpdf,
  var,
  val
)

#ipolate all vars
varnames<-names(tmpdf)[names(tmpdf)!="year"]
for(var in varnames) {
  tmpdf[[var]]<-na.approx(tmpdf[[var]],na.rm=F)
  tmpdf[[paste0("D.",var)]]<-c(NA,diff(tmpdf[[var]]))
}

#do granger tests
loopdf<-expand.grid(
  iv=varnames,
  dv=varnames,
  stringsAsFactors=F
)
loopdf2<-expand.grid(
  iv=paste0("D.",varnames),
  dv=paste0("D.",varnames),
  stringsAsFactors=F
)
loopdf<-rbind.fill(
  loopdf,
  loopdf2
)

tmpseq.i<-1:nrow(loopdf)
finaldf<-lapply(tmpseq.i,function(i) {
  #i<-1
  print(i)
  thisrow<-loopdf[i,]
  rootiv<-str_replace(thisrow$iv,"D\\.","")
  rootdv<-str_replace(thisrow$dv,"D\\.","")
  if(rootiv!=rootdv) {
    tmp<-!is.na(tmpdf[[thisrow$iv]]) & !is.na(tmpdf[[thisrow$dv]])
    df<-tmpdf[tmp,c(thisrow$dv,thisrow$iv)]
    m.tmp<-lmtest::grangertest(
      df[,thisrow$dv] ~ df[,thisrow$iv],
      order=3
    )
    thispval<-m.tmp$`Pr(>F)`[2]
    thisr<-cor(df[,thisrow$dv],df[,thisrow$iv])
    m.tmp2<-lmtest::grangertest(
      df[,thisrow$iv] ~ df[,thisrow$dv],
      order=3
    )
    thispval2<-m.tmp2$`Pr(>F)`[2]
  } else {
    thispval<-thispval2<-thisr<-NA
  }
  data.frame(
    iv=rootiv,
    dv=rootdv,
    indiff=str_detect(thisrow$iv,"D."),
    r=thisr,
    pval=thispval,
    pval2=thispval2,
    stringsAsFactors=F
  )
}) %>% rbind.fill

tmp<-!is.na(finaldf$pval)
finaldf<-finaldf[tmp,]

#apply pvals
get.pvals.class<-function(pvals) {
  y<-NA
  y[pvals<0.01]<-"at alpha=0.01"
  y[pvals>0.01 & pvals<0.05]<-"at alpha=0.05"
  y[pvals>0.05 & pvals<0.10]<-"at alpha=0.10"
  y[pvals>0.10]<-"not sig"
  return(y)
}
finaldf$pval.class<-get.pvals.class(finaldf$pval)
finaldf$pval2.class<-get.pvals.class(finaldf$pval2)

#look at things that 'granger-cause' black anxiety/punitive
tmp<-finaldf$dv%in%c("black_anxiety","black_punitive") &
  finaldf$pval.class!="not sig"
finaldf[tmp,]

#represent these results graphically
#correlations
tmp<-finaldf$dv%in%c(
  "white_anxiety",
  "white_punitive",
  "black_anxiety",
  "black_punitive"
) &
  !finaldf$indiff &
  finaldf$iv%in%c(
    "cenhom",
    "fbihom",
    "fbipcrt",
    "fbivcrt",
    "incrt_t_jur",
    "protest",
    "riots"
  )
plotdf<-finaldf[tmp,]

#add shape et al to plotdf
#add pval info to shape of point
plotdf$pval.shp<-NA
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
plotdf$pval.shp<-factor(
  plotdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c(
    "at alpha=0.01",
    "at alpha=0.05",
    "at alpha=0.10",
    "not sig"
  )
)
#tmpshapes
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(plotdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

plotdf$race<-str_extract(
  plotdf$dv,
  "black|white"
)
plotdf$dimension<-str_extract(
  plotdf$dv,
  "anxiety|punitive"
)

#violence/not violence, color it in
plotdf$violence<-"nv"
tmp<-plotdf$iv%in%c(
  "fbihom",
  "fbipcrt",
  "cenhom",
  "fbivcrt"
)
plotdf$violence[tmp]<-"v"
tmpcolors_you<-c(
  white,
  orange
)
names(tmpcolors_you)<-levels(plotdf$v)

tmplevels<-c(
  "anxiety",
  "punitive"
)
tmplabels<-c(
  "Anxiety",
  "Punitiveness"
)
plotdf$dimension<-factor(
  plotdf$dimension,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "black",
  "white"
)
tmplabels<-c(
  "Black",
  "White"
)
plotdf$race<-factor(
  plotdf$race,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "riots",
  "protest",
  "incrt_t_jur",
  "fbivcrt",
  "fbipcrt",
  "fbihom",
  "cenhom"
)
tmplabels<-c(
  "Riots",
  "Protests",
  "Incarceration",
  "Violent Crime",
  "Property Crime",
  "Homicides, FBI",
  "Homicides, CHS"
)
plotdf$iv<-factor(
  plotdf$iv,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=iv,
    y=r,
    color=violence,
    shape=pval.shp
  )
) +
  geom_point() +
  geom_hline(
    yintercept=0,
    color=white,
    linetype='dashed'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) + 
  scale_color_manual(
    values=tmpcolors_you,
    name="",
    guide=F
  ) +
  facet_wrap(
    race~ dimension
  ) +
  coord_flip() +
  theme_black() +
  xlab("") +
  ylab("\nCorrelation Coefficient") +
  guides(
    shape = guide_legend(override.aes= list(color = orange))
  )

tmpname<-"fig_violencecorrs.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)

#########################################################
#########################################################

#plot protests/riots, to give a sense of their shape
plotdf<-protestdf

tmplevels<-c(
  "riots",
  "protest"
)
tmplabels<-c(
  "Riots",
  "Protests"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

tmpcolors_riot<-c(
  red,
  blue
)
names(tmpcolors_riot)<-
  levels(plotdf$var)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=val,
    color=var,
    group=var
  )
) +
  geom_line(
    size=2
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors_riot
  ) +
  xlab("") +
  ylab("Events per 10 Million People\n") +
  theme_black() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

tmpname<-"fig_protest.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)

#########################################################
#########################################################

#plot protests/riots/crime and white public opinion
#this supports the point made in the correlations above
#the shape of this curve mirrors the crime rate, not protests..
setwd(datadir)
protestdf
pundf<-read.csv(
  'fig_trends.csv',
  stringsAsFactors=F
)
pundf$var<-paste0(
  pundf$race,"_",pundf$facet,"_",pundf$dimension
) %>% tolower
tmprows<-str_detect(pundf$var,"estimated") &
  pundf$dimension!="mistrust"
pundf$val<-pundf$mu
tmpcols<-c("year","var","val")
pundf<-pundf[tmprows,tmpcols]
vdf<-read.csv(
  'tab_longviolence.csv'
)

#merge
plotdf<-rbind.fill(
  protestdf,
  pundf,
  vdf
)
#common years
tmp<-plotdf$year%in%1950:2014 &
  !is.na(plotdf$val)
plotdf<-plotdf[tmp,]
plotdf<-by(plotdf,plotdf$var,function(df) {
  df$val<-scale(df$val)
  df
}) %>% rbind.fill

unique(plotdf$var)
tmp<-plotdf$var%in%c(
  "protest",
  "white_estimated_punitive",
  "black_estimated_punitive",
  "fbivcrt"
)
plotdf<-plotdf[tmp,]

tmplevels<-c(
  "white_estimated_punitive",
  "black_estimated_punitive",
  "protest",
  "fbivcrt"
)
tmplabels<-c(
  "Whites",
  "Blacks",
  "Protests",
  "Violence"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

tmpcolors_rook<-c(
  white,
  orange,
  blue,
  red
)
names(tmpcolors_rook)<-levels(plotdf$var)

plotdf$opinion<-str_detect(plotdf$var,"Whites|Blacks")
tmptypes<-c(
  'solid',
  'dotdash'
)
names(tmptypes)<-c(T,F)

#make four graphs
#white
#whiteblack
#whiteblackcrime
#whiteblackriots

tmpgraphs<-list(
  white=c("Whites"),
  whiteblack=c(
    "Whites",
    "Blacks"
  ),
  whiteblackcrime=c(
    "Whites",
    "Blacks",
    "Violence"
  ),
  whiteblackprotest=c(
    "Whites",
    "Blacks",
    "Protests"
  )
)

#get the lims
ylims<-by(plotdf,plotdf$var,function(df) {
  plotdf
  m.tmp<-loess(data=df,formula=val ~ year)
  range(predict(m.tmp,data.frame(year=1950:2014)),na.rm=T)
}) %>% unlist %>% range

for(thisgraph in names(tmpgraphs)) {
  #thisgraph<-"white"
  tmp<-plotdf$var%in%c(
    tmpgraphs[[thisgraph]]
  )
  g.tmp<-ggplot(
    plotdf[tmp,],
    aes(
      x=year,
      y=val,
      group=var,
      color=var,
      linetype=opinion
    )
  ) +
    stat_smooth(
      geom="line",
      size=1.5, 
      se=FALSE
    ) +
    scale_color_manual(
      name="",
      values=tmpcolors_rook
    ) +
    scale_linetype_manual(
      name="",
      values=tmptypes,
      guide=F
    ) +
    xlab("") +
    ylab("Normalized Level\n") +
    xlim(range(plotdf$year)) +
    ylim(ylims) +
    theme_black() +
    theme(
      legend.position='bottom',
      legend.direction='horizontal',
      legend.text = element_text(size=8)
    )
  tmpname<-paste0("fig_",thisgraph,".pdf")
  gs.list[[tmpname]]<-list(
    graph=g.tmp,
    filename=tmpname,
    width=6/1.5,
    height=6/1.5
  )
}

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
