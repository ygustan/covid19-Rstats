# Import ----
library(readr)
covid_19_datasets <- read_delim("https://static.data.gouv.fr/resources/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/20221219-190045/vacsi-s-a-reg-2022-12-19-19h00.csv", 
delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)
                                                       
# Show the dataset ----
View(covid_19_datasets)


#Les vaccins préférés par tranche d'âge OUI  1 ----

#Prédire la gestion logistique(stocks, etc ) d’une campagnes  de vaccinations de l’année suivante sur chaque mois pour chaque région  OUI 2 ----

#Prédiction sur l’utilisation des vaccins ( Quel vaccin va être le plus utilisé ? ) OUI 3 ----

#Prédire s’il y’a une corrélation entre les campagnes de vaccination et l'apparition de nouveaux variants OUI 4 ----

#Essayer de prédire la ou les catégorie(s) d’âges qui sont plus réticentes à la vaccination OUI 5 ----