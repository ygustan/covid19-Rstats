## Import ----
install.packages("tidyverse")
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

## Importation des données ----
covid_19_datasets <- read_delim("https://www.data.gouv.fr/fr/datasets/r/8e5e70fa-c082-45e3-a7b8-20862711b142", 
                                delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

## Les données ----
View(covid_19_datasets)


## Prédire s’il y’a une corrélation entre les campagnes de vaccination et l'apparition de nouveaux variants OUI 4 ----
data_correlation = data.frame(covid_19_datasets)
new_data_correlation = data.frame() 
new_data_correlation = data_correlation[c("reg","clage_vacsi","jour")]
new_data_correlation['rappel1'] = data_correlation['n_rappel_h'] + data_correlation['n_rappel_f'] + data_correlation['n_rappel_e']
new_data_correlation['rappel2'] = data_correlation['n_2_rappel_h'] + data_correlation['n_2_rappel_f'] + data_correlation['n_2_rappel_e']
new_data_correlation['rappel3'] = data_correlation['n_3_rappel_h'] + data_correlation['n_3_rappel_f'] + data_correlation['n_3_rappel_e']

View(new_data_correlation)