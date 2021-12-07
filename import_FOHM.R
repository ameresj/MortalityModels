library(dbplyr)
library(DBI)
library(dplyr)
library(tidyr)
# Truthly hast mirrored FOHM data
# https://github.com/truthly/c19
# We can also access data via
#con <- DBI::dbConnect(RPostgres::Postgres(), 
#                      dbname = "c19", 
#                      host='c19.truthly.com',
#                      user='c19')
#DBI::dbSendQuery(con, "SELECT * FROM death;")



parse_fohm<-function(xls_file){ 
  print(xls_file)
xls_file<-as.character(xls_file)
# Get list of all sheet names
sheets <- readxl::excel_sheets(xls_file)

# Extract report_date from sheet named e.g. "FOHM 23 Apr 2020"
report_date <- readr::parse_date(
  sub("^FOHM ", "", sheets[grep("[0-9][0-9]? [A-Z][a-z][a-z] 202[0-9]",sheets)]),
  "%d %b %Y"
)


fohm.cases<-readxl::read_excel(xls_file,
                sheet='Totalt antal per åldersgrupp') %>%
  tibble::add_column( report_date)
fohm.cases<-fohm.cases %>% rename('age_group'="Åldersgrupp"         ,
                    'cases'=   "Totalt_antal_fall" ,
                    'icu'="Totalt_antal_intensivvårdade" , 
                    'deaths'="Totalt_antal_avlidna" ) %>% 
  mutate( age_group=gsub('Ålder_','',age_group )) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='_') %>% 
  drop_na() %>% mutate( age_min=as.integer(age_min), age_max=as.integer(age_max)) 


fohm.gender<-readxl::read_excel(xls_file,
                                sheet='Totalt antal per kön',
                                ) %>% 
  tibble::add_column( report_date) %>% 
  rename( gender=Kön, cases=Totalt_antal_fall,
                        icu_cases=Totalt_antal_intensivvårdade, 
                        deaths=Totalt_antal_avlidna) %>% 
   mutate(gender=recode_factor(gender, 'Man'='male', 'Kvinna'='female',
                               'Uppgift saknas'='unknown'))


fohm.region<-readxl::read_excel(xls_file, sheet='Totalt antal per region') %>% 
              tibble::add_column( report_date) %>% 
  rename(         'cases'=   "Totalt_antal_fall" ,
         'icu'="Totalt_antal_intensivvårdade" , 
         'deaths'="Totalt_antal_avlidna",
         'cases_per_100000'= 'Fall_per_100000_inv', 'region'='Region') %>% 
  mutate(region=as.factor(region))

return( list(cases=fohm.cases, gender=fohm.gender, region=fohm.region))
}



download_fohm <-function(){
  
  
  # Download FOHM archive data
  library(httr)
  req <- httr::GET("https://api.github.com/repos/truthly/c19/git/trees/master?recursive=1")
  httr::stop_for_status(req)
  filelist <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
  filelist<-paste0('https://github.com/truthly/c19/raw/master/',
                   grep("files/", filelist, value = TRUE, fixed = TRUE))  
  # save only if file does not exist yet
  for (url in filelist){
    destination=paste0('./data/fohm/',basename(url))
    if (!file.exists(destination)){
      utils::download.file(url, destination)
    }
  }
}

#### Load entire 
download_fohm()

#xls_file='data/Folkhalsomyndigheten_Covid19_27022021.xlsx'

filelist<-list.files('./data/fohm',full.names=TRUE)
fohm<-lapply(filelist, parse_fohm )

# combine list data
fohm<-purrr::transpose(fohm)
for ( x in names(fohm)){
  fohm[[x]]=dplyr::bind_rows(fohm[x]) %>% mutate(country='SE')
}


#### Load 

scb.pop<-read.csv('./data/scb/BE0101N1_20210513-223812.csv',sep=';',
                  skip=2,                  fileEncoding = "iso-8859-1",
                  stringsAsFactors = T) %>% 
  rename( age=ålder,
                    gender=kön, 
                    population=X2020, 
                    marital_status=civilstånd) %>% 
         mutate(gender=recode(gender, 'män'='male', 'kvinnor'='female'),
                marital_status=recode(marital_status,
                                      'ogifta'='single',
                                      'gifta'='married',
                                      'skilda'='divorced',
                                      'änkor/änklingar'='widowed')) %>% 
       mutate(age= as.integer(gsub("[^0-9.-]", "", age) )) %>% 
   mutate( age_min=as.numeric(as.character((cut( age, breaks=seq(0,100,10), right=F ,
                        labels=seq(0,90,10),include.lowest = T)))))
## add population
fohm$cases<-fohm$cases %>% merge( (scb.pop %>% group_by(age_min) %>% 
                         summarise_at(vars(population),sum) ), all.x=T ,by='age_min')

#reshape(kön=gender, )

  #library(runner)g
#library(ggplot2)
#fohm$cases %>%  ggplot(aes(x=age_min,y=deaths/sum(deaths))) + geom_col()
#fohm$cases %>%  ggplot(aes(x=age_min,y=icu/sum(icu))) + geom_col()

