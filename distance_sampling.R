pacman::p_load(Distance,ggpubr,ggplot2,here,readxl,tidyverse,janitor)


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

distance_sampling <- ds(data,key="hn", adjustment=NULL,
                        convert_units=conversion.factor)

summary(distance_sampling)

results1 <- summary(distance_sampling)


resultats_globaux <-   results1$dht$individuals$summary

abondance <- results1$dht$individuals$N
densite<- results1$dht$individuals$D

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

distance_sampling <- ds(data,key="hn", adjustment=NULL,
                        convert_units=conversion.factor)

summary(distance_sampling)

densmean<-distance_sampling$dht$individuals$D$Estimate[[3]]*10^4/200


#####Graphe densité effectif#####

ggplot(densite%>%
         left_join(read_xlsx("data/Correspondance_ile.xlsx"),
                   by=c("Label"="com_transect"))%>%
         dplyr::filter(Label!="Total"),aes(x=Label,y=Estimate*10^4/200,ymin=0,ymax=(Estimate+se)*10^4/200,fill=ile))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=Estimate*10^4/200, ymax=(Estimate+se)*10^4/200), width=.2,
                position=position_dodge(.9))+
  facet_wrap(~ile,scales = "free_x")+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  geom_hline(yintercept=densmean, color="dark red")+
  annotate("text", x=5,y=0.35, label = paste("Moyenne totale :", round(densmean, 2),"individus/m"),color="dark red")+
  labs(fill="",x="",y="Effectif Myoporum crassifolium / m de littoral")+
  theme(axis.text.x = element_text(angle = 65,hjust = 1))

ggsave(here("output/Histogramme des densité estimées par m de littoral de myoporum en fonction de chaque transect sur Maré et Lifou.png"),height = 7,width =8)

n_myop_mare<-distance_sampling$dht$individuals$D$Estimate[[2]]*surf$surface[2]
n_myop_lifou<-distance_sampling$dht$individuals$D$Estimate[[1]]*surf$surface[1]
n_myop_ile<-tibble(ile=c("Lifou","Maré"),n_myop_tot=c(n_myop_mare,n_myop_lifou))

#####Graphe densité estimée biomasse#######

densite_biomasse_zone<-densite%>%
  left_join(n_myoporum%>%
              left_join(myoporum%>%
                          mutate(Label=com_transect))%>%
              filter(!is.na(circohp)&!is.na(hauteur))%>%
              mutate(biomasse=((1/3)*(hauteur-1.3)*100+50)*pi*(circohp/(2*pi)-0.5)^2)%>%
              group_by(Label)%>%
              summarise(biom_moy_cm3 = mean(biomasse)))%>%
  mutate(dens_biom_cm3_m2=biom_moy_cm3*Estimate)%>%
  mutate(se_biom=se*biom_moy_cm3)%>%
  filter(Label!="Easo plage" & Label!="siloam cëzy")%>%
  mutate(dens_biom_cm3_m2 = replace_na(dens_biom_cm3_m2, 0))

ggplot(densite_biomasse_zone%>%
         left_join(read_xlsx("data/Correspondance_ile.xlsx"),
                   by=c("Label"="com_transect"))%>%
         dplyr::filter(Label!="Total"),aes(x=Label,y=dens_biom_cm3_m2/10^2/200,ymin=0,ymax=(dens_biom_cm3_m2+se_biom)/10^2/200,fill=ile))+
  geom_bar(stat="identity", color="black",position=position_dodge())+
  geom_errorbar(aes(ymin=dens_biom_cm3_m2/10^2/200, ymax=(dens_biom_cm3_m2+se_biom)/10^2/200), width=.2,
                position=position_dodge(.9))+
  facet_wrap(~ile,scales = "free_x")+
  scale_fill_brewer(palette = "Set2")+
  theme_bw()+
  labs(fill="",x="",y="Biomasse estimée de Myoporum crassifolium en m3 par m")+
  theme(axis.text.x = element_text(angle = 65,hjust = 1))

ggsave(here("output/Histogramme des biomasse estimée de duramen de myoporum par m de littoral en fonction de chaque transect sur Maré et Lifou.png"),height = 7,width =8)

densite_biomasse_ile<- densite_biomasse_zone%>%
  left_join(read_xlsx("data/Correspondance_ile.xlsx"),
            by=c("Label"="com_transect"))%>%
  dplyr::filter(Label!="Total")%>%
  group_by(ile)%>%
  summarise(dens_biom_moy_m3_m=mean(dens_biom_cm3_m2)/10^2/200)%>%
  left_join(surf%>%select(-code_com,-code_post))%>%
  mutate(biom_m3_tot=dens_biom_moy_m3_m*200/10^4*surface)

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
       aes(y=value*10^4/200,x=classe_circo))+
  geom_bar(stat = "identity",position = position_stack(),color="black",linewidth=0.1,alpha=0.6)+
  scale_fill_brewer(palette = "Set1")+
  theme_bw()+ 
  theme(axis.text.x = element_text(vjust = 0.45,angle = 65))+
  facet_wrap(ile~com_transect)+
  #theme(strip.text.x = element_text(size = 12),strip.text.y = element_text(size = 12))+
  labs(x="Circonférence à 1.3m",fill="",y="Effectif de myoporum par m")#+
#theme(axis.title=element_text(size = 12),legend.text=element_text(size = 12))


ggsave(here("output/Densité par m de littoral de chaque classe de circonférence à 1,3m des myoporum pour Maré ET Lifou.png"),height = 8,width = 10)
