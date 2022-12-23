## Import ----
#install.packages("tidyverse")
library(tidyverse)
library(ggplot2)

## Importation des données ----
covid_19_datasets <- read_delim("https://www.data.gouv.fr/fr/datasets/r/8e5e70fa-c082-45e3-a7b8-20862711b142", 
                                delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

## Les données ----
View(covid_19_datasets)

## Essayer de prédire la ou les catégorie(s) d’âges qui sont plus réticentes à la vaccination OUI 5 ----
# DATA -> 
data_reticence = data.frame(covid_19_datasets)
new_data_reticence = data.frame() # Creation d'un nouveau dataFrame
colnames(data_reticence) # Aide pour avoir les noms des colonnes
#data_reticence = data_reticence[c("reg","clage_vacsi","jour","n_dose1_h","n_dose1_f","n_dose1_e","n_complet_h","n_complet_f","n_complet_e")]

# Ajout des éléments interessants ( Avec région )
new_data_reticence = data_reticence[c("reg","clage_vacsi","jour")]
new_data_reticence['dose1Somme'] = data_reticence['n_dose1_h'] + data_reticence['n_dose1_f'] + data_reticence['n_dose1_e']
new_data_reticence['nComplet'] = data_reticence['n_complet_h'] + data_reticence['n_complet_f'] + data_reticence['n_complet_e']
new_data_reticence['rappel1'] = data_reticence['n_rappel_h'] + data_reticence['n_rappel_f'] + data_reticence['n_rappel_e']
new_data_reticence['rappel2'] = data_reticence['n_2_rappel_h'] + data_reticence['n_2_rappel_f'] + data_reticence['n_2_rappel_e']
new_data_reticence['rappel3'] = data_reticence['n_3_rappel_h'] + data_reticence['n_3_rappel_f'] + data_reticence['n_3_rappel_e']


# Group By sur le DataFrame
new_data_reticence = new_data_reticence %>% 
  group_by(reg, clage_vacsi, month = lubridate::floor_date(jour, "month")) %>%
  summarise(across(c(dose1Somme, nComplet, rappel1, rappel2, rappel3), sum))

# Data sans les regions
new_data_reticence_sans_region = data_reticence[c("clage_vacsi","jour")]
new_data_reticence_sans_region['dose1Somme'] = data_reticence['n_dose1_h'] + data_reticence['n_dose1_f'] + data_reticence['n_dose1_e']
new_data_reticence_sans_region['nComplet'] = data_reticence['n_complet_h'] + data_reticence['n_complet_f'] + data_reticence['n_complet_e']
new_data_reticence_sans_region['rappel1'] = data_reticence['n_rappel_h'] + data_reticence['n_rappel_f'] + data_reticence['n_rappel_e']
new_data_reticence_sans_region['rappel2'] = data_reticence['n_2_rappel_h'] + data_reticence['n_2_rappel_f'] + data_reticence['n_2_rappel_e']
new_data_reticence_sans_region['rappel3'] = data_reticence['n_3_rappel_h'] + data_reticence['n_3_rappel_f'] + data_reticence['n_3_rappel_e']

new_data_reticence_sans_region = new_data_reticence_sans_region %>% 
  group_by(clage_vacsi, month = lubridate::floor_date(jour, "month")) %>%
  summarise(across(c(dose1Somme, nComplet, rappel1, rappel2, rappel3), sum))

# cree un total 
new_data_reticence_sans_region['total'] = new_data_reticence_sans_region['nComplet'] + new_data_reticence_sans_region['rappel1'] + new_data_reticence_sans_region['rappel2'] + new_data_reticence_sans_region['rappel3']

# Affichage du DataFrame
View(new_data_reticence_sans_region)
#View(new_data_reticence)

# Affichage graphique des vaccinations par population d'age

graph_reticence_sans_region <- new_data_reticence_sans_region #%>% 
  #filter(clage_vacsi %in% c("04","24","09","11","17","24","29","39","49","59"))

graph_reticence_sans_region %>%
  ggplot( aes(x=month, y=total, group=clage_vacsi, color=clage_vacsi)) +
  geom_line() +
  scale_color_viridis(discrete = TRUE) +
  ggtitle("Rétinence à la vacinnation")

# Affichage totals de la vaccination.    
    
graph_total_vac <- new_data_reticence_sans_region %>% 
    filter(clage_vacsi %in% c("0"))
    
graph_total_vac %>%
    ggplot( aes(x=month, y=total, group=clage_vacsi, color=clage_vacsi)) +
    geom_line()
  
# Prediction 
