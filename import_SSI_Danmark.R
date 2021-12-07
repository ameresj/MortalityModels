library(dplyr)
library(tidyr)
################################################################################
########################## Data from Denmark ###################################
# https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata 
#
#
danmark_tmp_zip<-'./data/ssi_danmark/data-epidemiologiske-rapport.zip'

# get overview over data
ssi_base_url <- "https://covid19.ssi.dk/overvagningsdata/download-fil-med-overvaagningdata"
html <- paste(readLines(ssi_base_url), collapse="\n")
urls <- unlist(stringr::str_extract_all(html,
              "https://files.ssi.dk/covid19/overvagning/data/data-epidemiologiske-rapport-[0-9]{8}-.{4}[-v2]*"))

urls <- unlist(stringr::str_extract_all(html,
                                        "https://files.ssi.dk/covid19/overvagning/dashboard/covid-19_dashboard_[0-9]{8}-.{4}[_v2]*"))
download_ssi_urls(urls)

download_ssi_urls<-function(urls){
# Download and unzip data
for (url in urls){
  datum=readr::parse_date( stringr::str_match(url,'[0-9]{8}')  ,format='%d%m%Y' )    
  exdir=paste0('./data/ssi_danmark/',datum)
  
  #readr::write_csv(data.frame( report_date=datum, download_date=date() ),
  #          paste0(exdir,'/date.csv')) 
  
    try_download <- try(
    utils::download.file(url ,danmark_tmp_zip ,quiet=T)
  )  
  if(is(try_download,"try-error")){
    print(paste0("ERROR: ", url))
  }else{
    #print(paste0("Downloaded: ", url))
    utils::unzip(danmark_tmp_zip , exdir=exdir, overwrite = TRUE)    
  }
  #remove.file(danmark_tmp_zip)
}       
}



#################################################################################
ssi_parse_cases <-function(file.cases){
  report_date=as.Date(stringr::str_match(file.cases,'[0-9]{4}-[0-9]{2}-[0-9]{2}')  )
  ssi.cases <- read.csv(file.cases,sep=';')
  ssi.cases<-ssi.cases %>% select( - Procent_positive) %>% 
  rename( 'cases'='Antal_bekræftede_COVID.19', 'tests'='Antal_testede', 
          'age_group'='Aldersgruppe') %>% head( -1) %>%
    mutate(cases=cases*1e3, tests=as.numeric(tests)*1e3) %>% 
  mutate(age_group= gsub("[^0-9.-]", "", age_group) ) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='-') %>% 
  mutate( age_min=as.integer(age_min),age_max=as.integer(age_max)) %>% 
  mutate(report_date=report_date)

 return(ssi.cases)
}

filelist<-list.files('./data/ssi_danmark', pattern='*Cases_by_age.csv',full.names=TRUE, recursive=T)
ssi.only_cases<-lapply(filelist, ssi_parse_cases)  %>% dplyr::bind_rows()

#################################################################################
ssi_parse_noegletal <-function(file.ngtl){
  
#report_date=as.Date(stringr::str_match(file.ngtl,'[0-9]{4}-[0-9]{2}-[0-9]{2}')  )
  ssi.ngtl<-read.csv(file.ngtl,sep=';',encoding="latin1") %>% 
  rename( 'cases'='Bekræftede.tilfælde', 'deaths'='Døde',
          'icu'='Indlagte.på.intensiv.afdeling',
          'hospitalized'='Indlagte',
          'report_date'='Dato',
          'age_group'='Aldersgruppe') %>% 
  mutate(age_group= gsub("[^0-9.-]", "", age_group) ) %>% 
  separate( col='age_group', into=c('age_min','age_max'), sep='-') %>% 
  mutate( age_min=as.integer(age_min),age_max=as.integer(age_max)) %>% 
  mutate(report_date=readr::parse_date(report_date,format='%d/%m/%Y'), 
         Region=as.factor(Region))
 return(ssi.ngtl)
}

filelist<-list.files('./data/ssi_danmark', pattern='*11_noegletal_pr_region_pr_aldersgruppe.csv',full.names=TRUE, recursive=T)
ssi.ngtl<-lapply(filelist, ssi_parse_noegletal)  %>% dplyr::bind_rows()

### Surprisingly ssi.cases and ssi.ngtl are fully consistens over cases
ssi.cases<-ssi.only_cases %>% merge( (ssi.ngtl %>%  group_by(age_min,age_max,report_date) %>% 
           summarise_at( vars(deaths,icu,hospitalized), sum) %>% ungroup()),
           by=c('age_min','age_max','report_date'),all=T) %>%
  mutate(country=as.factor('DK'))

ssi.cases %>%  group_by(age_min, country, report_date) %>% summarise_at( vars(cases),sum) %>%
  ggplot(aes(x=report_date,y=cases,colour=as.factor(age_min))) + geom_line() + facet_wrap(~country)

ssi.cases=ssi.ngtl%>%   mutate(country=as.factor('DK'))

#'home/ameresj/Dokumente/MortalityModels/data/ssi_danmark/2020-12-07/Cases_by_age.csv'


ssi.cases
#ssi.cases
  
  



    