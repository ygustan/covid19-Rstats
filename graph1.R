## Import ----
#install.packages("tidyverse")
install.packages("reshape2")
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(reshape2)

## Importation des données ----
covid_19_datasets <- read_delim("https://www.data.gouv.fr/fr/datasets/r/8e5e70fa-c082-45e3-a7b8-20862711b142", 
                                delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

## Les données ----
#View(covid_19_datasets)


## Evolution de la vaccination par sexe & projection OUI  1 ----
data_v = data.frame(covid_19_datasets)
colnames(data_v)
#new_data_v = data_v[c("clage_vacsi","jour","couv_complet_h","couv_complet_f", "n_cum_complet_h","n_cum_complet_f")]

new_data_v = data_v[c("clage_vacsi","jour")]
new_data_v["vaccin_h"] = covid_19_datasets["n_cum_complet_h"]+ covid_19_datasets["n_cum_rappel_h"] + covid_19_datasets["n_cum_2_rappel_h"] + covid_19_datasets["n_cum_3_rappel_h"]
new_data_v["vaccin_f"] = covid_19_datasets["n_cum_complet_f"] + covid_19_datasets["n_cum_rappel_f"] + covid_19_datasets["n_cum_2_rappel_f"] + covid_19_datasets["n_cum_3_rappel_f"]

new_data_v = new_data_v %>% 
  group_by(clage_vacsi, month = lubridate::floor_date(jour, "month")) %>%
  summarise(across(c(vaccin_h, vaccin_f), max))


## Affichage
View(new_data_v)


## Graphique 

graph1 <- new_data_v %>% 
  filter(clage_vacsi %in% c("0"))

ggplot(graph1, aes(x=month)) + 
  geom_line(aes(y = vaccin_h), color = "blue") + 
  geom_line(aes(y = vaccin_f), color="red") +
  labs(x = "Mois et Années", y = "Total", title = "Nombre total de vaccination par sexe")
