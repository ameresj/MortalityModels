# Download GeoJson NUTS data
#https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units/nuts

#install.packages(c("curl",'httr'))
install.packages('hmisc')
install.packages("xkcd", repos="http://R-Forge.R-project.org")

# We use the eurostat library
library(eurostat)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(xkcd)

# Population density
pop_density<-get_eurostat('demo_r_d3dens', time_format = "num",stringsAsFactors=TRUE)
area<-get_eurostat('demo_r_d3area', time_format = "num",stringsAsFactors=TRUE)   %>% rename( area=values) 
population<-get_eurostat('demo_r_pjangrp3', time_format = "num",stringsAsFactors=TRUE)  %>% rename( population=values) 

geodata  <- get_eurostat_geospatial(output_class = "df",   resolution = "60", 
                                    nuts_level = "all",  year = "2021") %>%  mutate_if(is.character, as.factor)

### 
nuts3<-geodata %>%  filter( LEVL_CODE==3) %>% select( CNTR_CODE,geo,NUTS_NAME,NUTS_ID) %>% unique()
####

DT<-nuts3 %>%  left_join((area %>% group_by(geo)) %>% summarise_at('area', mean)) %>%
       right_join( ( population  %>% filter( time==2019, age=='TOTAL', sex=='T') ), by='geo')
#group_by(geo) %>% summarise_at('population',sum)
population  %>% filter( time==2019, age=='TOTAL')
DT %>% mutate( density=population/area) %>% 
  group_by(CNTR_CODE) %>% 
  arrange( desc( density) , .by_group=TRUE) %>% 
   summarise( cs_pop=cumsum(population), cs_area=cumsum(area) ) %>% 
   mutate( density=(cs_pop/cs_area)) %>% 
  filter( CNTR_CODE %in% c('SE','FI','AT','DK','NO')) %>%
  ggplot(aes(x=cs_area, y=cs_pop, color=CNTR_CODE, group=CNTR_CODE)) + geom_line() + geom_point()

unique(area$geo) - unique(population$geo)

merge

population[ age='TOTAL',]

DT <-population %>% merge(  area %>%  select( c(geo,area)) ,by='geo') 

DT <- population %>%  left_join((area %>% group_by(geo) %>% summarise_at('area', max)) , by='geo')

DT %>% filter( time==2019, age=='TOTAL') %>% group_by(geo) %>%
   summarise_at(c('area','population'), sum) %>% filter(grepEurostatTOC())




pop_density %>% merge_eurostat_geodata(data = ., geocolumn = "geo", 
                                       resolution = "60",
                                       output_class = "df", all_regions = TRUE) 


############ With LAU ###########
EU_LAU<-read_excel('./data/EU-28-LAU-2019-NUTS-2016.xlsx',sheet='Combined') %>%
         mutate_if(is.character, as.factor) %>%
         mutate(DEGURBA)
EU_LAU = EU_LAU %>% rename(NUTS_ID=`NUTS 3 CODE`, AREA=`TOTAL AREA (m2)`) %>% data.table()
# Some countries area is given in km^2 instead of m^2
EU_LAU[COUNTRY %in% c('DE','DK','FI'), AREA:=AREA*1e6]

## Make Bavaria independent!
EU_LAU <- EU_LAU %>% filter( startsWith(as.character(NUTS_ID),'DE2')) %>% 
  mutate(COUNTRY='BAY') %>% bind_rows(.,EU_LAU)
EU_LAU <- EU_LAU %>% filter( startsWith(as.character(NUTS_ID),'DEG')) %>% 
  mutate(COUNTRY='THG') %>% bind_rows(.,EU_LAU)


EU_LAU %>% mutate( density=POPULATION/AREA) %>% 
  group_by(COUNTRY) %>% 
  arrange( desc( density) , .by_group=TRUE) %>% 
  summarise( cs_pop=cumsum(POPULATION), cs_area=cumsum(AREA) ) %>% 
  mutate( density=(cs_pop/cs_area)) %>% 
  filter( COUNTRY %in% c('BAY','SE','DK','FI','THG','CZ','AT')) %>%
  ggplot(aes(x=cs_pop, y=cs_area, color=COUNTRY, group=COUNTRY)) + geom_line() + coord_trans( y="log2")



### Urbanization level
EU_LAU %>% group_by(COUNTRY, DEGURBA) %>% summarise_at( 'POPULATION',sum) %>% 
  filter( COUNTRY %in% c('BAY','SE','DK','FI','THG','CZ','AT')) %>%
  ggplot(aes(fill=as.factor(DEGURBA), y=COUNTRY, x=POPULATION)) + 
  geom_bar(position="dodge", stat="identity")



https://www.lgl.bayern.de/gesundheit/infektionsschutz/infektionskrankheiten_a_z/coronavirus/karte_coronavirus/csv.htm?tabelle=tabelle4