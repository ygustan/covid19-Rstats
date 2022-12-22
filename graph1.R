## Import ----
install.packages("tidyverse")
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)

## Importation des données ----
covid_19_datasets <- read_delim("https://static.data.gouv.fr/resources/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/20221220-190045/vacsi-s-a-reg-2022-12-20-19h00.csv", 
                                delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

## Les données ----
View(covid_19_datasets)


## Evolution de la vaccination par sexe & projection OUI  1 ----
data_v = data.frame(covid_19_datasets)
colnames(data_v)
new_data_v = data_v[c("clage_vacsi","jour","couv_complet_h","couv_complet_f", "n_cum_complet_h","n_cum_complet_f")]

new_data_v = new_data_v %>% 
  group_by(clage_vacsi, month = lubridate::floor_date(jour, "month")) %>%
  summarise(across(c(couv_complet_h, couv_complet_f), max))


# Affichage
View(new_data_v)