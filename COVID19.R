# Import ----
install.packages("tidyverse")
library(readr)
library(tidyverse)

## Importation des données ----
covid_19_datasets <- read_delim("https://static.data.gouv.fr/resources/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/20221220-190045/vacsi-s-a-reg-2022-12-20-19h00.csv", 
delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)
                                                       
## Les données ----
View(covid_19_datasets)


## Les vaccins préférés par tranche d'âge OUI  1 ----
dataV = data.frame(covid_19_datasets)
dataV = dataV[c("reg","jour","vaccin")]
View(dataV)

## Prédire la gestion logistique(stocks, etc ) d’une campagnes  de vaccinations de l’année suivante sur chaque mois pour chaque région  OUI 2 ----

## Prédiction sur l’utilisation des vaccins ( Quel vaccin va être le plus utilisé ? ) OUI 3 ----

## Prédire s’il y’a une corrélation entre les campagnes de vaccination et l'apparition de nouveaux variants OUI 4 ----

## Essayer de prédire la ou les catégorie(s) d’âges qui sont plus réticentes à la vaccination OUI 5 ----
# DATA -> 
dataReticence = data.frame(covid_19_datasets)
newDataReticence = data.frame() # Creation d'un nouveau dataFrame
colnames(dataReticence) # Aide pour avoir les noms des colonnes
#dataReticence = dataReticence[c("reg","clage_vacsi","jour","n_dose1_h","n_dose1_f","n_dose1_e","n_complet_h","n_complet_f","n_complet_e")]

# Ajout des éléments interessants 
newDataReticence = dataReticence[c("reg","clage_vacsi","jour")]
newDataReticence['dose1Somme'] = dataReticence['n_dose1_h'] + dataReticence['n_dose1_f'] + dataReticence['n_dose1_e']
newDataReticence['nComplet'] = dataReticence['n_complet_h'] + dataReticence['n_complet_f'] + dataReticence['n_complet_e']

# Group By sur le DataFrame
newDataReticence = newDataReticence %>% 
  group_by(reg, clage_vacsi, month = lubridate::floor_date(jour, "month")) %>%
  summarize(across(c(dose1Somme, nComplet), sum))

# Affichage du DataFrame
View(newDataReticence)

# Prediction 
# Affichage graphique
