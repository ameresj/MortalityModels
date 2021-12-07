
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)


# Tidy SCB data
SCB=read.csv('data/BE0101AW_20210226-223546.csv',sep=';',skip=1, stringsAsFactors = FALSE) %>% 
  mutate(age:=as.integer( gsub('( )*year(s)*','',age)), gender=as.factor(sex)) %>% select(-sex) %>% 
  mutate_if(is.character, as.numeric)%>% gather(key='key',value='value', -c(gender,age))

### Separate and tidy data
SCB<-SCB %>% separate( col='key', into=c('attr','year'), sep='\\.(?=[0-9]{4})') %>% 
  mutate( attr=gsub('\\.\\.','',attr)) %>% 
  mutate(year=as.integer(year)) %>% spread( attr, value ) %>%
  mutate(gender=as.factor(recode_factor(gender, 'men'='male', 'women' ='female')))


g
##'(.*(?=.[0-9]{4}$))(?:.)(.*$)'
#regmatches(a,gregexpr('(.*(?=.[0-9]{4}$))(?:.)(.*$)',a))
#strsplit(a,regexec('([0123456789]*)', a))
#library(stringr)
#str_split(a,'.*' )

## Load estonia Data 
# 	Capsized and sank on 28 September 1994
estonia_passengers<-read.csv('data/estonia-passenger-list.csv') %>% 
  mutate(gender=as.factor(recode_factor(Sex, 'M'='male', 'F' ='female')), age=Age, year=1994) %>% 
  group_by( Survived, gender,age,year) %>% summarise ( passengers=n() ) 
  

# Years lost by life expectanty
estonia<-estonia_passengers %>% filter(Survived==0)  %>% merge(SCB  ,all.x=T) 

# Mean life years lost on estonia
estonia %>% summarise(  sum(passengers*Observed.expectation.of.life.at.age))



########### COVID ################
fhm<-read_excel('data/Folkhalsomyndigheten_Covid19_27022021.xlsx',
           sheet='Totalt antal per åldersgrupp')

fhm<-fhm %>% rename('age_group'="Åldersgrupp"         ,
                      'infections'=   "Totalt_antal_fall" ,
                      'icu_cases'="Totalt_antal_intensivvårdade" , 
                      'deaths'="Totalt_antal_avlidna" ) %>% 
  mutate( age_group=gsub('Ålder_','',age_group )) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='_') %>% 
    drop_na() %>% mutate( age_min=as.integer(age_min)) %>% 
  mutate(case_fatality_rate= deaths/infections, icu_risk=icu_cases/infections)

fhmi %>% ggplot(aes(x=age, y= log(deaths/infections))) + geom_line()





fhmi<-data.table( age=seq(0,100))
fhmi<-fhmi %>% mutate( deaths=interpolate_bin(fhm$age_min, fhm$deaths ,age   ),
                       infections=interpolate_bin(fhm$age_min, fhm$infections ,age),
                       icu_cases=interpolate_bin(fhm$age_min, fhm$icu_cases ,age))

DT <- fhmi %>% mutate(gender='male', year=2019) %>% 
    merge( (estonia_passengers %>% ungroup() %>% filter(Survived==0) %>% select(-Survived)), all = T  ) %>% 
       mutate_if(is.numeric, ~replace_na(., 0))%>% merge(SCB  ,all.x=T) 




DT %>% ggplot( aes(x=age,y=deaths, color=gender)) + geom_line() +geom_line(aes(x=age,y=passengers,color=gender))

# add expecetd 
DT %>% filter(age>2) %>%  mutate( exp_deaths=Probability.of.dyingper.mille.*infections/1e3 ) %>% 
    summarise( sum(exp_deaths)/sum(deaths))



bin_min=fhm$age_min
value=fhm$deaths

mean(diff(bin_min)
  'Ålder_10_19'

# Interpolate number of deaths from binned data
deaths=c(7,3,18,34,83,275,743,2707,5396,3560)/10
X=seq(0,100,10)

#s=splinefun(X,Y,method="natural")
#s=splinefun(X,Y,method="monoH.FC")
s=splinefun(X,Y,method="hyman")

Age=seq(X[1],tail(X,n=1),by=1)
DeathCount=s(Age,deriv=1)



covidSE=data.table( Age=Age, CovidDeaths=DeathCount, gender='male',year=1994 )

covidSE<-covidSE  

  
  merge(SCB  ,all.x=T) 

# Mean life years lost on estonia
covidSE %>% filter(age>60)  %>% summarise(  sum(CovidDeaths*Observed.expectation.of.life.at.age)) 

 
%>% merge(SCB  ,all.x=T)


