pacman::p_load(Distance,ggpubr,ggplot2,here,readxl,tidyverse,openxlsx,boot,dplyr,janitor)


myoporum <- read_excel("data/inventaire_myoporum.xlsx")%>%
  janitor::clean_names()%>%
  left_join(read_xlsx('data/Correspondance_ile.xlsx'))%>%
  select(-sssys_irnd,-has_errors,-interview_status,-assignment_id)

n_myoporum<- read_excel("data/inventaire_myoporum.xlsx",2)%>%
  mutate(circohp = if_else(circo50cm==850, 700, circohp))%>%
  janitor::clean_names()

spp_prox <- read_excel("data/inventaire_myoporum.xlsx",3)%>%
  janitor::clean_names()


carto <- read_excel("data/paramètres cartographie.xlsx")%>%
  janitor::clean_names()

surf<-read_excel("data/surface_totale.xlsx")%>%
  mutate(ile = recode(nom, "LIFOU"="Lifou","MARE"="Maré"))%>%
  select(-nom)%>%
  bind_rows(summarise(read_excel("data/surface_totale.xlsx"),
                      ile="Total",
                      surface=sum(surface)))%>%
  janitor::clean_names()


####Par zone d'étude############


data <- myoporum%>%
  left_join(carto)%>%
  left_join(n_myoporum)%>%
  mutate(Region.Label=as.factor(com_transect),
         Area=surface,
         Sample.Label=interview_key,
         Effort=longueur,
         object=row_number(),
  )%>%
  select(Region.Label,
         Area,
         Sample.Label,
         Effort,
         object,
         distance,
  )


conversion.factor <- convert_units("meter", "meter", "square meter")

distance_sampling_trans <- ds(data,key="hn", adjustment=NULL,
                        convert_units=conversion.factor)

summary(distance_sampling_trans)

results1 <- summary(distance_sampling_trans)

resultats_globaux_trans <-   results1$dht$individuals$summary
abondance_trans <- results1$dht$individuals$N
densite_trans<- results1$dht$individuals$D%>%
  mutate(Est_hm=Estimate*10^4/2,
         se_hm=se*10^4/2,
         lcl_hm=lcl*10^4/2,
         ucl_hm=ucl*10^4/2)%>%
  mutate(lcl_hm = ifelse(Label=="Kurine", 0, lcl_hm))%>%
  mutate(ucl_hm = ifelse(Label=="Kurine", 0, ucl_hm))

#####Par ile##############

data <- myoporum%>%
  left_join(carto)%>%
  left_join(n_myoporum)%>%
  mutate(Region.Label=ile,
         Sample.Label=interview_key,
         Effort=longueur,
         object=row_number(),
  )%>%
  group_by(ile)%>%
  mutate(Area=sum(surface))%>%
  ungroup()%>%
  select(Region.Label,
         Area,
         Sample.Label,
         Effort,
         object,
         distance,
  )

conversion.factor <- convert_units("meter", "meter", "square meter")

distance_sampling_ile <- ds(data,key="hn", adjustment=NULL,
                        convert_units=conversion.factor)

summary(distance_sampling_ile)

resultats_globaux_ile <-summary(distance_sampling_ile)$dht$individuals$summary
abondance_ile <- summary(distance_sampling_ile)$dht$individuals$N
densite_ile<- summary(distance_sampling_ile)$dht$individuals$D%>%
  mutate(Est_hm=Estimate*10^4/2,
         se_hm=se*10^4/2,
         lcl_hm=lcl*10^4/2,
         ucl_hm=ucl*10^4/2)

densmean<-densite_ile$Est_hm[3]

#####Graphe densité effectif#####

ggplot(densite_trans%>%
         left_join(read_xlsx("data/Correspondance_ile.xlsx"),
                   by=c("Label"="com_transect"))%>%
         dplyr::filter(Label!="Total"),aes(x=Label,y=Est_hm,ymin=0,ymax=Est_hm+1,96*se_hm,fill=ile))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=lcl_hm, ymax=ucl_hm), width=.2,
                position=position_dodge(.9))+
  facet_wrap(~ile,scales = "free_x")+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  geom_hline(yintercept=densite_ile$Est_hm[3], color="dark red")+
  geom_hline(yintercept=densite_ile$ucl_hm[3], color="dark red",linetype = "dashed" )+
  geom_hline(yintercept=densite_ile$lcl_hm[3], color="dark red",linetype = "dashed" )+
  annotate("text", x=5,y=90, label = paste("Moyenne totale :", round(densite_ile$Est_hm[3], 2),"individus/100m"),color="dark red")+
  labs(fill="",x="",y="Effectif Myoporum crassifolium / 100m de littoral")+
  theme(axis.text.x = element_text(angle = 65,hjust = 1))

ggsave(here("output/Histogramme des densité estimées de myoporum par 100m de littoral en fonction de chaque transect sur Maré et Lifou.png"),height = 7,width =8)

#####Estimation de biomasse#######

densite_biomasse_trans<-densite_trans%>%
  left_join(n_myoporum%>%
              left_join(myoporum%>%
                          mutate(Label=com_transect))%>%
              filter(!is.na(circohp)&!is.na(hauteur)&!is.na(circo50cm)&circohp!=0&circo50cm!=0&hauteur!=0)%>%
              mutate(biomasse_m3=pi*((1/3)*(circohp/1000/(2*pi)-0.005)^2*(hauteur-1.3)+(1/3)*((circo50cm/1000/(2*pi)-0.005)^2*(hauteur-0.5)-(circohp/1000/(2*pi)-0.005)^2*(hauteur-1.3))+(circo50cm/1000/(2*pi)-0.005)^2*0.7))%>%
              group_by(Label)%>%
              summarise(moy_biom_m3 = mean(biomasse_m3),
                        sd_value = sd(biomasse_m3),
                        n = n(),
                        se_biom_m3 = sd_value / sqrt(n),
                        med_biom_m3=median(biomasse_m3),
                        lcl_biom_m3=quantile(biomasse_m3, 0.025),  # IC à 95%
                        ucl_biom_m3=quantile(biomasse_m3, 0.975)))%>%
  mutate(moy_biom_m3=replace_na(moy_biom_m3,0),
         se_biom_m3=replace_na(se_biom_m3,0),
         n=replace_na(n,0))%>%
  mutate(dens_biom_m3_hm=moy_biom_m3*Est_hm)%>%
  mutate(lcl_dens_biom_m3_hm = lcl_hm*moy_biom_m3)%>%
  mutate(ucl_dens_biom_m3_hm = ucl_hm*moy_biom_m3)%>%
  
  filter(Label!="Easo plage" & Label!="siloam cëzy")%>%
  mutate(dens_biom_m3_hm = replace_na(dens_biom_m3_hm, 0))


densite_biomasse_ile<- densite_biomasse_trans%>%
  left_join(read_xlsx("data/Correspondance_ile.xlsx"),
            by=c("Label"="com_transect"))%>%
  filter(Label!="Total"& dens_biom_m3_hm!=0)%>%
  group_by(ile)%>%
  summarise(dens_biom_moy_m3_hm=mean(dens_biom_m3_hm),
            lcl_dens_biom_moy_hm=quantile(dens_biom_m3_hm, 0.025),
            ucl_dens_biom_moy_hm=quantile(dens_biom_m3_hm, 0.975))%>%
  bind_rows(summarise(densite_biomasse_trans%>%
                        left_join(read_xlsx("data/Correspondance_ile.xlsx"),
                                  by=c("Label"="com_transect"))%>%
                        filter(dens_biom_m3_hm!=0),
                      ile="Total",
                      dens_biom_moy_m3_hm=mean(dens_biom_m3_hm),
                      lcl_dens_biom_moy_hm=quantile(dens_biom_m3_hm, 0.025),
                      ucl_dens_biom_moy_hm=quantile(dens_biom_m3_hm, 0.975)))%>%
  left_join(surf%>%select(-code_com,-code_post))%>%
  mutate(biom_m3_tot_est=dens_biom_moy_m3_hm*2*surface/10000,
         lcl_biom_m3_tot=lcl_dens_biom_moy_hm*2*surface/10000,
         ucl_biom_m3_tot=ucl_dens_biom_moy_hm*2*surface/10000,)

#####Graphe densité estimée biomasse#######

ggplot(densite_biomasse_trans%>%
         left_join(read_xlsx("data/Correspondance_ile.xlsx"),
                   by=c("Label"="com_transect"))%>%
         dplyr::filter(Label!="Total"),aes(x=Label,y=dens_biom_m3_hm,ymin=ucl_dens_biom_m3_hm,ymax=lcl_dens_biom_m3_hm,fill=ile))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=ucl_dens_biom_m3_hm,ymax=lcl_dens_biom_m3_hm), width=.2,
                position=position_dodge(.9))+
  facet_wrap(~ile,scales = "free_x")+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  geom_hline(yintercept=densite_biomasse_ile$dens_biom_moy_m3_hm[3], color="dark red")+
  geom_hline(yintercept=densite_biomasse_ile$ucl_dens_biom_moy_hm[3], color="dark red",linetype = "dashed" )+
  geom_hline(yintercept=densite_biomasse_ile$lcl_dens_biom_moy_hm[3], color="dark red",linetype = "dashed" )+
  labs(fill="",x="",y="Biomasse estimée de Myoporum crassifolium en m3 pour 100m")+
  theme(axis.text.x = element_text(angle = 65,hjust = 1))

ggsave(here("output/Histogramme des biomasse estimée de duramen de myoporum par 100m de littoral en fonction de chaque transect sur Maré et Lifou.png"),height = 7,width =8)

wb <- openxlsx::createWorkbook()
addWorksheet(wb,"densite myoporum")
writeDataTable(wb,"densite myoporum",x = densite_ile)
addWorksheet(wb,"densite_biomasse_ile")
writeDataTable(wb,"densite_biomasse_ile",x = densite_biomasse_ile)
saveWorkbook(wb,here("output","resultats_myoporum.xlsx"),overwrite = T)

#####Graphique catégorie circo#####

myoporum_2023  <- n_myoporum%>%
  left_join(myoporum%>%select(interview_id,ile,com_transect))%>%
  mutate(classe_circo=cut(circohp,
                          include.lowest = TRUE,
                          right = T,
                          dig.lab = 4,
                          breaks = c(0, 100, 200, 300, 400, 500, 
                                     600, 700, 800, 900, 1000, 1100)
  ),
  classe_circo=as.numeric(as.factor(classe_circo))*100,
  classe_circo=ifelse(classe_circo==100,"De 0 à 100 mm",paste("De",as.numeric(classe_circo)-100,"à",as.numeric(classe_circo),"mm")),
  classe_circo = fct_relevel(classe_circo,
                             "De 0 à 100 mm", "De 100 à 200 mm","De 200 à 300 mm",
                             "De 300 à 400 mm", "De 400 à 500 mm","De 500 à 600 mm",
                             "De 600 à 700 mm", "De 700 à 800 mm", "De 800 à 900 mm",
                             "De 900 à 1000 mm","De 1000 à 1100 mm"))#%>%
#mutate(classe_circo=ifelse(com_transect == "xodre" & is.na(classe_circo), "De 0 à 100 mm",classe_circo))

plots_myop<-myoporum%>%
  left_join(myoporum_2023%>%
              group_by(com_transect)%>%
              count(classe_circo)%>%
              filter(is.na(classe_circo)==F)%>%
              pivot_wider(names_from=classe_circo,values_from=n))

counting_myoporum_2023 <- plots_myop%>%
  pivot_longer(c(`De 0 à 100 mm`:`De 1000 à 1100 mm`),names_to = "classe_circo")%>%
  filter(is.na(value)==F)#%>%
#group_by(ile,com_transect)%>%
#summarise(n=sum(classe_circo,na.rm = T))%>%
#mutate(value=n)%>%
#select(-c(n))

ggplot(counting_myoporum_2023%>%
         left_join(carto)%>%
         mutate(value=value/surface),
       aes(y=value*10^4/2,x=classe_circo))+
  geom_bar(stat = "identity",position = position_stack(),color="black",linewidth=0.1,alpha=0.6)+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()+ 
  theme(axis.text.x = element_text(vjust = 0.45,angle = 65))+
  facet_wrap(ile~com_transect)+
  #theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12))+
  labs(x="Circonférence à 1.3m",fill="",y="Densite de M. crassifolium par 100 m de littoral")#+
#theme(axis.title=element_text(size = 12),legend.text=element_text(size = 12))


ggsave(here("output/Densité par m de littoral de chaque classe de circonférence à 1,3m des myoporum pour Maré ET Lifou.png"),height = 8,width = 10)
