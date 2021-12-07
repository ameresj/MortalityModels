library(dplyr)
library(tidyr)

### Download population data for Federal states
#tbl<-wiesbaden::download_csv('12411-0014', save=F)
#

# Daten des bayrischen Landesamts für Statistik
#https://www.statistikdaten.bayern.de/genesis//online/data?operation=table&code=12411-007s&levelindex=0&levelid=1619078564238

# Get population data and demography
lgl.pop<-read.csv2('./data/BayrischesLandesamtStatistik/12411-007s_Bayern.csv',
               encoding='latin1', skip=8, nrows=88,
              col.names=c('age_group','sum','male','female')) %>% 
  mutate(age_group= gsub("[^0-9.-]", "", sub('unter','-',recode(age_group,'unter 1'='0 bis unter 1' )) )) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='-') %>%  
  mutate( age_min=as.integer(age_min),age_max=as.integer(age_max)) %>% 
  pivot_longer(cols=c('male','female'),names_to='gender', values_to='population' ) %>% select(-'sum')
## introduce 10 year intervals
lgl.pop<-lgl.pop %>% mutate( age_group=cut( age_min, right=F, breaks=seq(0,100,10)
                                            ,labels=paste0(seq(0,90,10),'-', seq(0,90,10)+9) 
                                            )                           ) 
# interpolate missing data


# Get mortality data
lgl.mortality<-read.csv2("./data/BayrischesLandesamtStatistik/12621-002_20210420.csv",
                   encoding='latin1', skip=9, nrows=100,sep=';',
                   col.names=c('attained_age',
                               'male.life_expectancy',
                               'male.survivors',
                               'male.q_x',
                               'female.life_expectancy',
                               'female.survivors',
                               'female.q_x')) %>% 
  pivot_longer( cols=-attained_age) %>% 
  separate( col='name', into=c('gender','name'), sep='\\.') %>% 
  pivot_wider( names_from='name', values_from='value') %>% 
  mutate(attained_age= as.integer(gsub("[^0-9.-]", "", recode(attained_age,'unter 1 Jahr'='0' ))),
         gender=as.factor(gender)) %>% 
  merge(lgl.pop , by.x=c('attained_age','gender'), by.y = c('age_min','gender'), all.x=T) #merge with population


lgl_archive_url='https://www.lgl.bayern.de/gesundheit/infektionsschutz/infektionskrankheiten_a_z/coronavirus/karte_coronavirus/fallzahlen_archiv/'

date_min=as.Date('2020-04-02')
date_max=as.Date('2021-07-28')


filelist<-lapply( seq(date_min,date_max,by=1) ,
                  function(x){return(paste0(lgl_archive_url, format(x, '%Y%m%d'), '_todesfalle.csv'))} )
filelist<-filelist %>% append(lapply( seq(date_min,date_max,by=1) ,
                  function(x){return(paste0(lgl_archive_url, format(x, '%Y%m%d'), '_faelle_alter_geschlecht_tote.csv'))} ))
filelist<-filelist %>% append(lapply( seq(date_min,date_max,by=1) ,
                  function(x){return(paste0(lgl_archive_url, format(x, '%Y%m%d'), '_faelle_alter_geschlecht.csv'))} ))
filelist<-filelist %>% append(lapply( seq(date_min,date_max,by=1) ,
                                      function(x){return(paste0(lgl_archive_url, format(x, '%Y%m%d'), '_RB_coronazahlen.csv'))} ))
filelist<-filelist %>% append(lapply( seq(date_min,date_max,by=1) ,
                                      function(x){return(paste0(lgl_archive_url, format(x, '%Y%m%d'), '_LK_coronazahlen.csv'))} ))
filelist<-filelist %>% append(lapply( seq(date_min,date_max,by=1) ,
                                      function(x){return(paste0(lgl_archive_url, format(x, '%Y%m%d'), '_inzidenzAlter.csv'))} ))

# 20210401_variantenMW.csv
# 20210401_variantenLK_MW_karte.csv
# 20210401_variantenLK.csv
# 20210401_todesfalle.csv
# 20210401_prozente_todesfalle.csv
# 20210401_inzidenzAlter.csv
# 20210401_hinweise.csv
# 20210401_genesen.csv
# 20210401_faelle_meldedatum.csv
# 20210401_faelle_alter_geschlecht_tote.csv
# 20210401_faelle_alter_geschlecht.csv
# 20210401_entwicklung.csv
# 20210401_RB_coronazahlen.csv
# 20210401_LK_coronazahlen.csv

for (url in filelist){
  destination=paste0('./data/lgl_bayern/',basename(url))
  if (!file.exists(destination)){
    utils::download.file(url, destination, quiet=T)
  } else {
    if (length(grep('<!doctype html>', readLines(destination,n=20)))!=0 ){
      file.remove(destination)
    }
  }
}

# Parse death 
parse_lgl_deaths <-function(fl){
  #print(fl)
 return(
 read.csv2(fl,sep=';',encoding="latin1") %>% 
     select(- 'Todesfälle.insgesamt', -'divers') %>%  
  rename( 'male'='männlich', 'female'='weiblich', 'unkown'='unbekannt' , 
                      'age_group'='Altersgruppe') %>% head( -2) %>%
  mutate(age_group= gsub("[^0-9.-]", "", age_group) ) %>% 
   gather( 'gender', 'deaths', -age_group, factor_key=T ) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='-') %>% 
  mutate( age_min=as.integer(age_min),age_max=as.integer(age_max)) %>% 
  mutate(report_date=readr::parse_date( substr(basename(fl),1,8), format='%Y%m%d'))
 )
}

# Parse cases 
parse_lgl_cases <-function(fl){
  #print(fl)
  return(read.csv2(fl,sep=';',encoding="latin1") %>% 
  select(-'Gesamt') %>%  
  rename( 'male'='männlich', 'female'='weiblich', 'unkown'='unbekannt' , 
          'age_group'='Altersgruppe') %>% head( -2) %>%
  mutate(age_group= gsub("[^0-9.-]", "", age_group) ) %>% 
  gather( 'gender', 'cases', -age_group, factor_key=T ) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='-') %>% 
  mutate( age_min=as.integer(age_min),age_max=as.integer(age_max)) %>% 
  mutate(report_date=readr::parse_date( substr(basename(fl),1,8), format='%Y%m%d'))
  )
}

parse_lgl_incidence <-function(fl){
  #print(fl)
  
return(
  read.csv2(fl,sep=';',encoding="latin1") %>% 
    rename( 'week'='Meldewoche', 'year'='Meldejahr', 'age_group'='Altersgruppen.Inz' , 
            'cases'='Anzahl', 'incidence'='Inzidenz') %>% 
    filter(age_group!='Gesamt') %>%  
    mutate(age_group= gsub("[^0-9.-]", "", age_group) ) %>% 
    separate( col='age_group', into=c('age_min','age_max'), sep='-') %>% 
    mutate( age_min=as.integer(age_min),age_max=replace_na(as.integer(age_max),100)) %>% 
    mutate(report_date=readr::parse_date( substr(basename(fl),1,8), format='%Y%m%d')) 
)
}
lgl.deaths=lapply(   list.files('./data/lgl_bayern',
                                pattern='*_faelle_alter_geschlecht_tote.csv', full.names = T), 
                     parse_lgl_deaths) %>% dplyr::bind_rows()%>% 
            group_by(country='BAY')
lgl.cases=lapply(   list.files('./data/lgl_bayern',
                                pattern='*_faelle_alter_geschlecht.csv', full.names = T), 
                     parse_lgl_cases) %>% dplyr::bind_rows() %>% 
                group_by(country='BAY')
lgl.incidence=lapply(   list.files('./data/lgl_bayern',
                               pattern='*_inzidenzAlter.csv', full.names = T), 
                        parse_lgl_incidence) %>% dplyr::bind_rows()%>% 
              group_by(country='BAY')


# join
lgl.cases<-lgl.cases %>% merge(lgl.deaths, all=T) %>% mutate(country='BAY') %>%
  merge( separate(lgl.pop, col='age_group', into=c('age_min','age_max'), sep='-') %>%  
       mutate( age_min=pmin(as.integer(age_min),90)) %>%
     group_by(age_min,gender) %>% summarise_at( vars(population),sum), all=T) %>%
   mutate( population=replace_na(population, 1))


#fl<-list.files('./data/lgl_bayern',
#           pattern='*_RB_coronazahlen.csv', full.names = T)[1]
#read.csv2(fl,sep=';',encoding="latin1") %>% View()

### Helper functions
add_age_group<-function(df){
  return(df %>% mutate(age_group=as.factor(paste0(age_min,'-',age_min+9))))
}



# Combine country data
cases.total<-lgl.cases %>% 
  merge(fohm$cases,all=T) %>% 
  merge(ssi.cases,all=T) %>%  
    mutate(country=as.factor(country)) %>%  
     mutate( phase=as.factor( ifelse(report_date < as.Date('2020-09-01'),'first wave', 'second wave'))) %>% 
  mutate_at(vars(cases,deaths,icu,hospitalized), ~replace_na(.,0)) %>% mutate( age_min=pmin(age_min, 90)  )

#unisex
cases <-cases.total %>% group_by(age_min,age_max, report_date, country,phase) %>% 
   summarise_at( vars(cases,deaths,icu,population), function(x){sum(replace_na(x,0))} )
# history
cases<-cases %>% group_by(age_min,age_max, country) %>% arrange(report_date) %>%
        mutate( cases=replace_na(cases-lag(cases,default=0),0) ,
                deaths=replace_na(deaths-lag(deaths,default=0),0),
                icu=replace_na(icu-lag(icu,default=0),0)) %>% 
  filter(cases >=0) %>% filter(country!='DK')


cases %>%  group_by(age_min, country, report_date) %>% summarise_at( vars(cases),sum) %>%
  ggplot(aes(x=report_date,y=cases,colour=age_min)) + geom_line() + facet_wrap(~country)



cases %>%  filter(country=='BAY') %>% View()

cases %>%  group_by(age_min, country,phase) %>% summarise_at( vars(deaths),sum) %>%
  ggplot(aes(x=age_min,y=deaths,fill=country)) + geom_col(position='dodge') +facet_wrap(~phase)

-
cases %>%  group_by(age_min, country,phase) %>% summarise_at( vars(deaths,cases),sum) %>%
  ggplot(aes(x=age_min,y=deaths/cases,fill=country)) + geom_col(position='dodge') +facet_wrap(~phase)

cases %>% group_by( country,phase)%>% summarise_at( vars(deaths,cases),sum) %>%
  ggplot(aes(x=country,y=deaths/cases)) +geom_col() +facet_wrap(~phase)

cases %>%  group_by(age_min, country) %>% summarise_at( vars(cases),sum) %>%
  ggplot(aes(x=age_min,y=cases/sum(cases),fill=country)) + geom_col(position='dodge')


cases.total %>% group_by(country) %>% summarise(max(report_date))






#### derive death weight
# standard
hazard_weight <- cases.total %>% 
  group_by(country,age_min,age_max) %>% filter(report_date==max(report_date) )%>% 
  summarise_at(vars(cases,deaths,icu),sum) %>% ungroup() %>% 
   group_by(country) %>% mutate( deaths_weight=deaths/sum(deaths)*n(),  icu_weight=icu/sum(icu)*n()) %>%  data.table::data.table()
# relative to cases / admission rates
hazard_weight<-hazard_weight <- cases.total %>% add_age_group() %>%  
  mutate_at('age_group', ~recode(.,'90-99'='90+')) %>% 
  group_by(country,age_group)%>% filter(report_date==max(report_date) ) %>%
  summarise_at(vars(cases,deaths,icu,hospitalized),sum) %>% 
  mutate( CFR=deaths/cases, icuAR=(icu)/cases )  %>% ungroup()  %>% 
  group_by(country) %>% mutate( deaths_weight=CFR/sum(CFR)*n(),  icu_weight=icuAR/sum(icuAR)*n())  %>%  ungroup() %>% 
  #mutate( deaths_weight=deaths/cases,  icu_weight=icu/cases)  %>% 
  #mutate_at( vars(deaths_weight, icu_weight),  function(x){return(x/sum(x)*length(x))}) %>% 
  data.table::data.table()
# fix bavaria
hazard_weight[ country=='BAY', icu_weight:= filter( hazard_weight, country!='BAY') %>% 
                 group_by(age_group) %>% summarise( icu_weight= mean(icu_weight)) %>% 
                 ungroup() %>% mutate(  icu_weight=icu_weight/sum(icu_weight)*n()) %>% pull(icu_weight) ]
 
#hazard_weight[ country=='BAY', icu_weight:= filter( hazard_weight, country=='DK') %>% pull(icu_weight) ]

#
# TODO: incidence by age sepcific population
#
#



(cases.total  %>% group_by(country) %>% filter(report_date==max(report_date)) %>% 
  group_by(age_min,age_max,country) %>% summarise_at(vars(cases,deaths,population),sum) %>% 
  ggplot(aes(x=age_min,y=cases/population,fill=country)) + geom_col(position='dodge') +ylim(0,0.2)) 

cases.total %>% filter(country=='BAY')

(cases.total  %>% group_by(country) %>% filter(report_date==max(report_date)) %>% 
    group_by(age_min,country) %>% 
    mutate(age_min=pmin(age_min,90))  %>% 
    summarise_at(vars(cases,deaths,population),sum) %>% drop_na() %>% add_age_group() %>% 
    ggplot(aes(x=age_group,y=deaths/population*1e5,fill=country)) + geom_col(position='dodge') ) 


## Log scale zum Vergleich mit Bevölkerungssterblichkeit
#+ scale_y_continuous(trans = log2_trans(),
 #                    breaks = trans_breaks("log2", function(x) 2^x),
#                     labels = trans_format("log2", math_format(2^.x)))

### Rates per country
hazard_weight %>% ggplot(aes(x=age_group,y=CFR, fill=country)) + geom_col(position='dodge') 
hazard_weight %>% ggplot(aes(x=age_group,y=icuAR, fill=country)) + geom_col(position='dodge')+
  ylab('admission rate to intensive care')

### Vizualize hazard weight
hazard_weight %>% ggplot(aes(x=age_group, y=icu_weight, fill=country)) +geom_col(position='dodge')
hazard_weight %>% ggplot(aes(x=age_group, y=deaths_weight, fill=country)) +geom_col(position='dodge')




gw1<-hazard_weight %>% ggplot(aes(x=age_group, y=icu_weight, fill=country))  + theme(legend.position = c(0, 0.8))
gw2<-hazard_weight %>% ggplot(aes(x=age_group, y=deaths_weight, fill=country)) +geom_col(position='dodge')


grid.arrange(g1, arrangeGrob(g1, g1, ncol=2), nrow = 2)

plot_grid(p3, bottom_row, labels = c('A', ''), label_size = 12, ncol = 1)


#### Gini coefficient

fit.gini=mgcv::bam(  )

cases %>%    group_by( country ,report_date,age_min,age_max) %>%   
         summarise_at(vars(cases,deaths,icu),sum)  %>% 
          group_by( country ,age_min,age_max) %>% filter(age_min<99, report_date > as.Date('2020-09-01')) %>% 
        arrange((report_date)) %>% 
        mutate_at(vars(cases,deaths), function(x){return(pracma::movavg(x,n=3,type='w'))}) %>%
     group_by( country ,report_date) %>% mutate( cases_gini=reldist::gini( cases )) %>% 
          ggplot(aes(x=report_date, y=cases, color=country)) + geom_line() +facet_wrap(~age_min)
  


%>% mutate( cases =runner::runner(cases, f=sum, k=7,lag=0, na_pad=T),
                                         deaths =runner::runner(deaths, f=sum, k=21,lag=0, na_pad=T)) %>%
           group_by( country ,report_date) %>% mutate( cases_gini=reldist::gini( deaths )) %>% 
       ggplot(aes(x=report_date, y=cases_gini, color=country)) + geom_line() +
  ylab(' Gini by age groups')

  Gini

theme_set(  ggplot2::theme_minimal())

 
group_by(country,report_date) %>% 
  summarise_at( vars(none, cases_7day_deathw),sum) %>% 
  

  
  


cases %>%  merge( tibble(report_date=seq(date_min,date_max,by=1)), all=T) %>% 
  group_by( country ,report_date,age_min) %>% summarise_at(vars(cases),sum) %>%  ungroup() %>%  group_by(country,age_min) %>%  
  arrange(report_date) %>% mutate( cases_7day = cases +lag(cases,1) +lag(cases,2)+lag(cases,3)+lag(cases,4)+lag(cases,5)+lag(cases,6)) %>% 
  arrange(country, age_min,report_date) %>% View()
  ggplot(aes(x=report_date, y=cases, color=country,group=country)) + geom_line() +facet_wrap(~age_min)



   cases %>%  merge( tibble(report_date=seq(date_min,date_max,by=1)), all=T) %>% 
  group_by( country , age_min) %>% summarise_at(vars(cases),sum) %>%    
  arrange(report_date ,desc()) %>% mutate( cases_7day = cases +lag(cases,1) +lag(cases,2)+lag(cases,3)+lag(cases,4)+lag(cases,5)+lag(cases,6)) %>% 
  ungroup() %>% 
  ggplot(aes(x=report_date, y=cases, color=country)) + geom_line() +facet_wrap(~age_min)



cases %>% ggplot(aes(x=report_date, y=deaths, fill=age_min)) +geom_col() + facet_wrap(~country)

fill_na
cases %>%  filter(report_date= ggplot(aes(x=as.factor(age_min),y=deaths/sum(deaths),fill=country)) + geom_col(position='dodge')


cases %>% View()

library(ggplot2)




fohm$cases %>%  ggplot(aes(x=age_min,y=deaths/sum(deaths),fill=report_date)) + geom_col()
lgl.cases %>%  ggplot(aes(x=(age_min+age_max)/2,y=deaths/sum(deaths))) + geom_col()


#################### Mortality Comparison #####################################
library(data.table)
cfr <- lgl.cases %>% filter(country=='BAY') %>% 
  mutate( age_min=pmin(age_min,90)) %>% 
  group_by(age_min, gender)%>% filter(report_date==max(report_date) )  %>% filter( gender %in% c('male','female')) %>% 
  summarise_at(vars(cases,deaths),sum) %>% data.table::data.table()

# interpolate cases
cfr<-cfr %>% group_by(gender)  %>%
  summarise(              attained_age=seq(0,99),
    cases=interpolate_bin(age_min, cases, attained_age ),
             deaths=interpolate_bin(age_min, deaths, attained_age )  )  %>% ungroup()
# Merge with Mortality table
cfr<-lgl.mortality %>% merge( cfr, all.y=F, by=c('attained_age','gender'))
cfr<-cfr %>% mutate(cfr=deaths/cases)

cfr %>% ggplot(aes(x=attained_age,y=deaths,color=gender)) +geom_point()
cfr %>% ggplot(aes(x=attained_age,y=cases,color=gender)) +geom_point()
cfr %>% ggplot(aes(x=attained_age,y=deaths/cases,color=gender)) +geom_point()

View(lgl.mortality)
View(cases)
cfr$cases
cfr$age_min

###########
g1<-cfr %>% ggplot(aes(x=attained_age,y=cfr,color=gender)) +geom_line() 
   xlab('Alter') + ylab('Rohe Sterberate')
# Lebenserwartung
cfr %>% ggplot(aes(x=attained_age,y=life_expectancy,color=gender,fill=gender,group=gender)) +
                    geom_line()+geom_area(alpha=0.3,position='identity')+
  ggtitle('Fernere Lebenserwartung',subtitle='Restlaufzeit')

cfr


cfr<-cfr %>% mutate( years_at_risk=life_expectancy*cfr )
cfr %>% ggplot(aes(x=attained_age,y=life_expectancy*cfr,color=gender,fill=gender)) +
  geom_line()+geom_area(alpha=0.3,position='identity')

cfr %>% filter(attained_age>50) %>% summarise(pop=sum(population,na.rm=T))


# Comparison to natural mortality
cfr %>% ggplot(aes(x=attained_age, y=q_x*2/12, colour='\ninnerhalb von\n drei Monaten\n')) + geom_line()  +
  geom_line(aes(x=attained_age,y=cfr,color='\nnach einer \nCOVID-19\nInfektion') ,data=lgl.mortality) +
  facet_wrap(~gender, ncol=1)

+scale_y_continuous(trans = log10_trans(),
                                                 breaks = trans_breaks("log10", function(x) 10^x),
                                                 labels = trans_format("log10", math_format(10^.x)))+
  ggtitle('Die Wahrscheinlichkeit in Bayern zu sterben...')

########### Effect of Vaccines
cfr<-cfr %>% mutate( years_at_risk=cfr*life_expectancy)


# interpolate 
interpolate_bin <-function( bin_min, value, x){
  X=c(bin_min, tail(bin_min,n=1)+tail(diff(bin_min),n=1))
  #*(X[i]-X[i-1])
  Y=vector(length=length(X))
  Y[0]=0
  for(i in 2:length(Y)) Y[i]=Y[i-1]+value[i-1]
  #s=splinefun(X,Y,method="natural")
  #s=splinefun(X,Y,method="monoH.FC")
  s=splinefun(X,Y,method="hyman")
  
  value_i=s(x,deriv=1)
  return(value_i)
}

lgl.pop

cfr %>% ggplot( aes(x=attained_age, y=years_at_risk, fill=gender,colour=gender)) + geom_line()

lgl.mortality %>% ggplot( aes(x=attained_age, y=years_at_risk, fill=gender,colour=gender)) + geom_line()
lgl.mortality %>% ggplot( aes(x=attained_age, y=life_expectancy, fill=gender)) + geom_col(position='dodge')



cfr

cfr %>% ggplot(aes(x=attained_age,y=deaths/cases, color=gender)) + geom_line() + xlim(0,95) + ylim(0,0.5)

cfr
cfr %>% group_by(gender) %>% summarise_at('cases',sum)
sum(cfr$cases)

cases_pdf<-splinebins(d$age_min, d$cases, m = 80,numIterations = 50, monoMethod = "hyman") 

plot(cases_pdf$splinePDF(seq(0,100)))
d<-cfr %>% filter(gender=='male')

lgl.mortality <-lgl.mortality %>% merge( data.table( female=interpolate_bin(cfr[gender=='female']$age_min, cfr[gender=='female']$cfr, seq(0,100) ),
                                                     male=interpolate_bin(cfr[gender=='male']$age_min, cfr[gender=='male']$cfr, seq(0,100) ),
                                                     attained_age=seq(0,100)) %>% 
                                           pivot_longer( cols=c('male','female'),names_to='gender', values_to='cfr') )


lgl.mortality <-lgl.mortality %>% merge( data.table( female=interpolate_bin(cfr[gender=='female']$age_min, cfr[gender=='female']$cfr, seq(0,100) ),
                                                     male=interpolate_bin(cfr[gender=='male']$age_min, cfr[gender=='male']$cfr, seq(0,100) ),
                                                     attained_age=seq(0,100)) %>% 
                                           pivot_longer( cols=c('male','female'),names_to='gender', values_to='cfr') )



# Comparison to natural mortality
lgl.mortality %>% ggplot(aes(x=attained_age, y=q_x*2/12, colour='\ninnerhalb von\n drei Monaten\n')) + geom_line()  +
  geom_line(aes(x=attained_age,y=cfr,color='\nnach einer \nCOVID-19\nInfektion') ,data=lgl.mortality) +
  facet_wrap(~gender, ncol=1)+scale_y_continuous(trans = log10_trans(),
                                                 breaks = trans_breaks("log10", function(x) 10^x),
                                                 labels = trans_format("log10", math_format(10^.x)))+
  ggtitle('Die Wahrscheinlichkeit in Bayern zu sterben...')

lgl.mortality <-lgl.mortality %>% mutate( years_at_risk=cfr*life_expectancy )


cfr %>% ggplot(aes(x=attained_age, y=q_x, colour=gender)) + geom_line()  



