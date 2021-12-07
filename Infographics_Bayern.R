library(ggthemr)
library(runner)
library(gridExtra)



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



incw<-cases %>% add_age_group() %>% 
  mutate_at('age_group', ~recode(.,'90-99'='90+')) %>% 
  merge( tibble(report_date=seq(date_min,date_max,by=1)), all=T) %>% 
  group_by( country ,report_date,age_group) %>% 
  summarise_at(vars(cases,deaths,icu,population),sum) %>% 
  merge(hazard_weight %>% mutate(none=1.0) %>% 
          rename( death=deaths_weight, icu_cases=icu, icu=icu_weight  ) %>% 
          pivot_longer( c('none','death','icu'),  
                        names_to='hazard', values_to='weight'  ) %>%  select( 'country','age_group','weight','hazard'),all.y = F) %>% 
  ungroup() %>% 
  group_by(country,age_group, hazard) %>%  
  arrange(report_date) %>% mutate( cases_7day =runner::runner(cases, f=sum, k=7,lag=0, na_pad=F) ) %>% ungroup() %>% 
  mutate(incidence=cases_7day*weight, population=population*weight) %>% 
  group_by(country, report_date,hazard) %>% summarise_at( vars(incidence,population) ,sum) %>% 
  mutate(incidence_per_100K=incidence/population*1e5) %>% ungroup()


######################### Infographik Bavaria ##################################
#ggthemr('flat')
library(latex2exp)
theme_set(  ggplot2::theme_minimal())

g1<-incw %>% rename( Normalisierung=hazard) %>% filter(country=='BAY') %>%  filter(report_date>as.Date('2020-04-15') ) %>% 
  mutate(Normalisierung=recode(Normalisierung, 'death'='Tod (q)', 'icu'='Intensivmed.(p)', 'none'='    ohne \n(unverändert)') )%>% 
  ggplot(aes(x=report_date, y=incidence_per_100K, color=Normalisierung)) +
  geom_line() +   scale_color_manual(values=c('black','#00a786','red' )) + 
  theme(axis.title.x=element_blank(), axis.title.y=element_blank() ,
        plot.title=element_text(hjust=0.5,size=16),
        legend.box.margin = margin(t=0),
        legend.margin = margin(t=0))+
  annotate('text',x=as.Date('2020-05-15'), color='gray20',
           y=250, label=TeX("$ \\frac{ \\sum_{Alter}\\omega_{Alter} \\cdot Fälle_{Alter}}{\\sum_{Alter}\\omega_{Alter} \\cdot Bevölkerung_{Alter}  } $"),
           size=3)+
  annotate('text',x=as.Date('2020-12-15'),
           y=80, label=('Inzidenz\nunterschätzt Schäden'),fontface=3, color='gray20')+
  annotate('text',x=as.Date('2021-04-01'),
           y=250, label=('Inzidenz\nüberschätzt Schäden'),fontface=3, color='gray20')+
  ggtitle( '7-Tages Inzidenz in Bayern mit Normalisierungen nach altersabhängigem Risiko')

g2<-hazard_weight %>%    
  mutate(country=recode( country, 'BAY'='Bayern', 'DK'='Dänemark', 'SE'='Schweden'  ) ) %>% 
  mutate( deaths_weight=deaths_weight/10, icu_weight=icu_weight/10) %>% 
  pivot_longer( c('deaths_weight','icu_weight','CFR','icuAR'),  names_to='Risiko', values_to='weight'  ) %>% 
  mutate(Risiko= factor(Risiko, levels = c('deaths_weight','icu_weight','CFR','icuAR',ordered=T)) )%>% 
  mutate(Risiko=recode( Risiko, 'deaths_weight'='Altersgruppenanteil an Todesfällen', 
                        'icu_weight'='Altersgruppenanteil intensivmediz. Fällen'  ,
                        'CFR'='Todesfallrate', 'icuAR'='Rate intensivmediz. Fälle' )) %>% 
  ggplot(aes(x=age_group, y=weight, fill=country))+geom_col(position='dodge')  + 
  facet_wrap(~Risiko, scales='free_y')+
  xlab('Altersgruppe') +
  scale_y_continuous( labels=function(x) paste0(x*100,"%")) +
  scale_fill_manual(values=c('#88c1de','#c7042c','#fdb601' ))+
  theme(legend.position = 'right',
        panel.grid.major.x=element_blank(),
        axis.title.y=element_blank(),
        strip.text.x=element_text(size=13),
        legend.title=element_text( size=7,color = 'gray20'))+
  guides(fill=guide_legend(title='*Bayrische Intensivfälle\naus dänischen & schwedischen\n Daten geschätzt\n'))

{gg<-grid.arrange(g1, g2,heights=c(1.2,2))
  dpi=120
  ggsave( filename='InzidenzBayernRisikogewichtet.png', plot=gg,
          width=1200/dpi, height=675/dpi, units='in', dpi=dpi)
  }
#

hwtheme<-list( geom_col(position='dodge'),
               #xlab('Altersgruppe'),
               scale_y_continuous( labels=function(x) paste0(x*100,"%")),
               scale_fill_manual(values=c('#88c1de','#c7042c','#fdb601' )),
               theme(panel.grid.major.x=element_blank(),
                     axis.title.y=element_blank(),
                     axis.title.x=element_blank(),
                     strip.text.x=element_text(size=13),
                     legend.position = "none"))

hw<-hazard_weight %>%    
  mutate(country=recode( country, 'BAY'='Bayern', 'DK'='Dänemark', 'SE'='Schweden'  ) ) %>% 
  mutate( deaths_weight=deaths_weight/10, icu_weight=icu_weight/10) 

g2a<-hw %>% ggplot(aes(x=age_group, y=deaths_weight, fill=country))+ 
  ggtitle('Normierte Todesfallraten')  +
  geom_text(x=-Inf,y=Inf, hjust=0, vjust=1, color='gray20' , check_overlap = T, size=3,
            label=TeX('$\\omega_{\\Alter}^q = \\frac{\\hat{q}_{Alter} }{\\sum_{\\Alter} \\hat{q}_{Alter}}  $') )+
  hwtheme + coord_cartesian(ylim = c(0, 0.5), expand = T)

g2b<-hw %>% ggplot(aes(x=age_group, y=icu_weight, fill=country))+
  geom_text(x=-Inf,y=Inf, hjust=0, vjust=1, color='gray20' , check_overlap = T,  size=3,
            label=TeX('$\\omega_{\\Alter}^p = \\frac{\\hat{p}_{Alter} }{\\sum_{\\Alter} \\hat{p}_{Alter}}  $') )+
  ggtitle('Normierte Raten für Intensivfälle')+hwtheme + coord_cartesian(ylim = c(0, 0.5), expand = T)

g2c<-  hw %>% ggplot(aes(x=age_group, y=CFR, fill=country))+
  geom_text(x=-Inf,y=Inf, hjust=0, vjust=1, color='gray20' , check_overlap = T,  size=3,
            label= expression(hat('q')==frac('# Todesfälle ', '#'~ atop('bestätigte','Infizierte' ) )))+
  ggtitle('Todesfallrate')+hwtheme
g2d<-hw %>% ggplot(aes(x=age_group, y=icuAR, fill=country))+
  geom_text(x=-Inf,y=Inf, hjust=0, vjust=1, color='gray20' , check_overlap = T, size=3,
            label= expression(hat('p')==frac('# Intensivfälle ', '#'~ atop('bestätigte','Infizierte' ) )))+
  ggtitle('Rate Intensivfälle')+hwtheme

#library(patchwork)
#g2<- g2a + g2b & theme(legend.position = "right", legend.title = element_blank())
#g2+ plot_layout(guides = "collect")


#g2<-ggpubr::ggarrange(g2a, g2b, g2c, g2d, ncol=2, nrow=2)
g2_legend<-ggpubr::get_legend(g2a+theme(legend.position = 'right',
                                        #legend.title=element_blank(),
                                        legend.text = element_text(size=13),
                                        legend.title=element_text( size=7,color = 'gray20'))+
                                guides(fill=guide_legend(title='*Bayrische Intensivfallgewichte\naus dänischen & schwedischen\nDaten geschätzt\n')))
g2_legend<-ggpubr::as_ggplot(g2_legend)
gg<-grid.arrange(grobs=list(g1,g2a,
                            g2b,g2c,
                            g2d ,g2_legend,
                            ggpubr::text_grob(
                              label="\n\n
       **Unterschätzung des Risikos\n für Männer durch\n unisex-Transformation.\n
Daten: LGL & LfStaD Bayern\nFOHM & SCB Sverige\n SSI Danmark",size=6, hjust=0.5,
                              color='gray20') 
),
widths=c(2,2,0.6),
layout_matrix=rbind( c(1,1,1),
                     c(2,3,6),
                     c(4,5,7)),
heights=c(1.3,1,1)
)

{
  dpi=120
  ggsave( filename='InzidenzBayernRisikogewichtet.png', plot=gg,
          width=1200/dpi, height=675/dpi, units='in', dpi=dpi)
}


##################################################################################



#################### Mortality Comparison #####################################
library(data.table)
cfr <- lgl.cases %>% filter(country=='BAY') %>% 
  mutate( age_min=pmin(age_min,90)) %>% 
  group_by(age_min, gender)%>% filter(report_date==max(report_date) )  %>% filter( gender %in% c('male','female')) %>% 
  summarise_at(vars(cases,deaths),sum) %>% data.table::data.table()

# mortality into age groups
cfr<-lgl.mortality %>% mutate(age_min=attained_age) %>% add_age_group() %>% 
  group_by(age_group,gender) %>% summarise( life_expectancy=mean(life_expectancy),
                                            q_x=mean(q_x),
                                            population=sum(population)) %>% 
  merge( cfr %>% add_age_group(), all.y=F, by=c('age_group','gender'))  %>% 
  ungroup()
  
# Merge with Mortality table
cfr<-cfr %>% mutate(cfr=deaths/cases, years_at_risk=cfr*life_expectancy)



g1<-cfr %>% ggplot(aes(x=age_group, y=cfr, fill=gender))+
  geom_text(x=-Inf,y=Inf, hjust=0, vjust=1, color='gray20' , check_overlap = T,  size=3,
            label= expression(hat('q')==frac('# Todesfälle ', '#'~ atop('bestätigte','Infizierte' ) )))+
  ggtitle('Todesfallrate')+hwtheme

g2<-cfr %>% ggplot(aes(x=age_group, y=life_expectancy, fill=gender)) + geom_col(position='dodge')

g1
g2
g1<-cfr %>% ggplot(aes(x=age_group, y=years_at_risk, fill=gender)) + geom_col(position='dodge')

cfr %>% arrange(desc(years_at_risk)) %>% select(age_group,gender)
g1
+
  geom_text(x=-Inf,y=Inf, hjust=0, vjust=1, color='gray20' , check_overlap = T,  size=3,
            label= expression(hat('q')==frac('# Todesfälle ', '#'~ atop('bestätigte','Infizierte' ) )))+
  ggtitle('Todesfallrate')+hwtheme




cfr %>% ggplot(aes(x=age_group,y=cfr,fill=gender)) + geom_col()
