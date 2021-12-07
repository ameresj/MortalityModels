library(ISOweek)
library(rjson)

ch_path='./data/ch/bag_covid_19_data_json_28_April_2021/data/'


ch.cases=fromJSON( file=paste0(ch_path,'COVID19Cases_geoRegion_AKL10_w.json' ))%>% 
  rbindlist(fill=T)  %>% mutate_if(is.character, as.factor) %>% 
  rename( cases=entries,cases_total=sumTotal) %>% 
  select( -inz_entries, -inzsumTotal, -type, -type_variant,-freq, -prct )


ch.death=fromJSON(file= paste0(ch_path,'COVID19Death_geoRegion_AKL10_w.json' ))%>% 
            rbindlist(fill=T) %>% mutate_if(is.character, as.factor) %>% 
            rename( deaths=entries, deaths_total=sumTotal) %>% 
  select( -inz_entries, -inzsumTotal, -type, -type_variant,-freq, -prct )

ch.hosp=fromJSON(file= paste0(ch_path,'COVID19Hosp_geoRegion_AKL10_w.json' ))%>% 
  rbindlist(fill=T) %>% mutate_if(is.character, as.factor) %>% 
  rename( hospitalized=entries, hospitalized_total=sumTotal) %>% 
  select( -inz_entries, -inzsumTotal, -type, -type_variant,-freq, -prct )

ch.vaccine<-fromJSON(file= paste0(ch_path,'COVID19VaccDosesAdministered_AKL10_w.json' ))%>% 
  rbindlist(fill=T) %>% mutate_if(is.character, as.factor) %>% 
  rename(  vaccinated_total=sumTotal ) %>% 
  select(  -type, -type_variant,-freq, -prct , -per100Persons_mean7d, -mean7d,
           -per100Persons, -per100PersonsTotal, -pop)

ch.tests<-fromJSON(file= paste0(ch_path,'COVID19Test_geoRegion_w.json' ))%>% 
  rbindlist(fill=T) %>% mutate_if(is.character, as.factor) %>% 
  rename( tests=entries, tests_total=sumTotal) %>% 
  select( -inz_entries, -inzsumTotal, -type, -type_variant,-freq, -prct )

chdata<-ch.cases %>% merge(ch.death ) %>% merge(ch.hosp)  %>%
  rename( age_group=altersklasse_covid19, 
                   population=pop,
                   Region=geoRegion) %>%
  mutate(report_date=ISOweek2date( sub("(\\d{4})(\\d{2})", "\\1-W\\2-7", datum))) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep=' - ') %>% 
  select(-datum,-datum_unit, -datum_dboardformated ,
         -matches('timeframe'),-version) %>%
  mutate(country=as.factor('CH')) %>% 
  filter(age_min!='Unbekannt') %>%
  mutate(age_min=replace_na(as.integer(age_min),80), ## 80+
         age_max=replace_na(as.integer(age_max),100)) 
  
# Problem: largest age group is 80-100
