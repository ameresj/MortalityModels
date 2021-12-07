#RKI Vergelich intensiv
#https://twitter.com/rki_de/status/1390596034187911170/photo/1
rki_icu_snap<-data.table(age_min=c(20,30,40,50,60,70,80),
#age_max=c(29,39,49,59,69,79,100),
icu_weight=c(0.009,0.03,0.082,0.218 ,0.319,0.266,0.076),
icu_cases=c(0.009,0.03,0.082,0.218 ,0.319,0.266,0.076)*4321,
country=as.factor('GER'))


# standard
hazard_weight <- cases.total %>% group_by(country) %>% filter(report_date==max(report_date) ) %>% ungroup() %>% 
  mutate(age_min=pmin(age_min,80)) %>% 
  group_by(country,age_min) %>% filter(report_date==max(report_date) )%>% 
  summarise_at(vars(cases,deaths,icu),sum) %>% ungroup() %>% 
  group_by(country) %>% mutate( deaths_weight=deaths/sum(deaths),  
                                icu_weight=icu/sum(icu)) %>%  data.table::data.table() %>% 
  bind_rows(rki_icu_snap) %>% 
  bind_rows(
    (chdata %>% filter(report_date==max(report_date) & Region=='CH' ) %>% 
       mutate(icu_weight=hospitalized_total/sum(hospitalized_total) ) %>% select(age_min,age_max,icu_weight,country))) %>% 
  group_by(country) %>%
  arrange(country,age_min) %>% 
  mutate(age_max= lead(age_min,default=100)-1) %>% filter(country!='BAY') 

theme_set(  ggplot2::theme_minimal())


gg3<-hazard_weight %>% rename(Land=country) %>%  ggplot(aes(x=paste0(age_min,'-',age_max),y=icu_weight ,fill=Land)) + 
  geom_col(position='dodge')+xlab('Altersgruppe') + ylab('Anteil an allen COVID Intensivfällen')+
  scale_y_continuous( labels=function(x) paste0(x*100,"%")) +
    scale_fill_manual(values=c('#c7042c','#fdb601','#88c1de','indianred4' ))+
  labs(title='Altersstruktur von COVID Patienten auf Intensivstation',
       subtitle='Basierend auf der Gesamtanzahl von Intensivfällen (für DE nur KW 18)',
       caption='Daten: RKI(7.5.), FOHM Schweden, BAG Schweiz, SSI Dänemark')+
  theme(legend.text=element_text('Land'),)

dpi=60*3
ggsave( filename='IntensivstationenVergleich.png', plot=gg3,
        width=1200/dpi, height=675/dpi, units='in', dpi=dpi)
