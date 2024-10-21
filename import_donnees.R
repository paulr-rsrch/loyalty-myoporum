if(!require(pacman))install.packages("pacman")

#pacman permet d'installer et d'activer tous ces packages
pacman::p_load(tidyverse,styler,haven,devtools,here,openxlsx,sjlabelled)

#il faut telecharger ce package depuis github
if(!require(susoapi))devtools::install_github("arthur-shaw/susoapi",upgrade = "never")

#activation du package
library(susoapi)

#identifiants du serveurs
set_credentials(
  server = "https://enquetes.iac.nc",
  workspace = "solveg",
  user = "solveg_api",
  password = "IACs0lvegMare"
)



questionnaires <- susoapi::get_questionnaires()


#l'id du questionnaire nous sert à indiquer celui qu'on veut exporter

for ( i in 1:3) {
  
  #boucle pour chaque version du questionnaire
  start_export(
    qnr_id = paste0("385c3450-3216-49fb-8a6d-28614c1c6fdd$" ,i) ,
    export_type = "STATA",
    interview_status = "All",
    include_meta = TRUE
  ) -> started_job_id
  
  
  
  Sys.sleep(10)
  
  #si pas de dossier data on le créee
  if(!dir.exists(here("data")))dir.create(here("data"))
  #un dossier par version
  if(!dir.exists(here(paste0("data/V", i))))dir.create(here(paste0("data/V", i)))
  

  Sys.sleep(10)
  #si deja un myoporum version zip on le supprime
  
  if(file.exists(here(paste0("data/V", i ,"/myoporum_", i,"_STATA_All.zip"))))file.remove(here(paste0("data/V",i,"/myoporum_",i,"_STATA_All.zip")))
  
  #on exporte le zip  
  get_export_file(
    job_id = started_job_id,
    path = here(paste0("data/V",i))
  )
  

  Sys.sleep(10)
  
  unzip(here(paste0("data/V",i,"/myoporum_",i,"_STATA_All.zip")),exdir = here(paste0("data/V",i)))
  
  Sys.sleep(10)
  
  
}

#pour chaque sous dossier on importe chaque fichier
#en lui attribuant un numero correspondant à la version
z <- 1

for ( z in 1:3) {
  
  #liste de tous les fichiers avec extension .dta dans le dossier data/dta
  files <-   Sys.glob( paste0("data/V",z,"/*.dta"))
  
  #on les importe tous dans une liste
  listOfFiles <- lapply(files, function(x) read_dta(here( x) ))
  
  for( i in 1:length(files)){
    a <- listOfFiles[[i]]
    a <- a%>%mutate_if(is.labelled,as_label)
    c <- str_remove(files[i],paste0("data\\/V",z,"\\/"))
    c <- str_remove(c,".dta")
    assign(paste0(c,"_V",z) ,a)
    
  }
}

#on fusionne et on vire les valeurs manquantes

n_myoporum <- n_myoporum_V1%>%
  bind_rows(n_myoporum_V2)%>%
  bind_rows(n_myoporum_V3)%>%
  filter(is.na(statut)==F)%>%
  select(-c(GPS_myop__Altitude,aubier,sufterrierre,densite,heuteurstrate))%>%
  select(-c(14:109))

myoporum <- myoporum_V1%>%
  bind_rows(myoporum_V2)%>%
  bind_rows(myoporum_V3)%>%
  select(-c(gpstransect__Altitude))


#on pivote n_myoporum pour avoir une ligne par spp_prox, en virant les valeurs manquantes
spprox <- n_myoporum%>%
  pivot_longer(starts_with("spp_prox"),names_prefix = "spp_prox__",
               names_to ="n_spp_prox",values_to = "spp_prox" )%>%
  filter(spp_prox!="##N/A##")


#on exporte tout dans un classeur excel

wb <- openxlsx::createWorkbook()
addWorksheet(wb,"myoporum")
writeDataTable(wb,"myoporum",x = myoporum)
addWorksheet(wb,"n_myoporum")
writeDataTable(wb,"n_myoporum",x = n_myoporum)
addWorksheet(wb,"spprox")
writeDataTable(wb,"spprox",x = spprox)
saveWorkbook(wb,here("data","inventaire_myoporum.xlsx"),overwrite = T)
write.csv(myoporum, file = here::here("data", "myoporum.csv"), row.names = FALSE)
write.csv(n_myoporum, file = here::here("data", "n_myoporum.csv"), row.names = FALSE)
