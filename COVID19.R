# Import ----
# install.packages("tidyverse")
# install.packages("rlang")
library(readr)
library(rlang)
library(lubridate)
library(dplyr)
library("Hmisc")
# library(tidyverse)
                                                       
## Importation des données ----
covid_19_datasets <- read_delim("https://www.data.gouv.fr/fr/datasets/r/8e5e70fa-c082-45e3-a7b8-20862711b142", 
delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)
View(covid_19_datasets)


# Show the dataset without lines with only zeros ----
covid_19_datasets_without_0s = data.frame(covid_19_datasets)
covid_19_datasets_without_0s <- covid_19_datasets_without_0s[!apply(covid_19_datasets_without_0s[-1:-3], 1, function(x) all(x == 0)), ]
View(covid_19_datasets_without_0s)


##Go through each row and determine if a value is zero
# row_sub = !apply(covid_19_datasets_without_0s, 1, function(row) all(row == 0 || row == 0.0))
# row_sub

## Les vaccins préférés par tranche d'âge OUI  1 ----
# dataV = data.frame(covid_19_datasets)
# dataV = dataV[c("reg","jour","vaccin")]
# View(dataV)


# covid_19_datasets_without_0s = covid_19_datasets_without_0s[row_sub,]
#Les vaccins préférés par tranche d'âge OUI  1 ----

## Prédire la gestion logistique(stocks, etc ) d’une campagnes  de vaccinations de l’année suivante sur chaque mois pour chaque région  OUI 2 ----

# General stuffs
# View(logistics_dataset)
logistics_dataset = data.frame(covid_19_datasets_without_0s)
logistics_dataset = logistics_dataset[, c("reg", "clage_vacsi", "jour", "n_dose1_h", "n_dose1_f","n_dose1_e")]
# tracemem(logistics_dataset) == tracemem(covid_19_datasets_without_0s)
total_of_doses_1_of_all_time = sum(logistics_dataset$n_dose1_h, logistics_dataset$n_dose1_f, logistics_dataset$n_dose1_e)
# logistics_dataset <- cbind(logistics_dataset, c("Total of 1 doses of all time For Men", total_of_doses_1_of_all_time))

#-------------- Get all doses of all periods ------------
total_of_doses_1_of_all_time

#---------- Specific stuffs --------------
#-------------- Get all doses of each month ------------
# Group by sum using dplyr
# num_of_days_on_each_month = days_in_month(as.Date(lubridate::floor_date(x=logistics_dataset$jour, unit="month"), "%Y-%m-%d"))
# num_of_days_on_each_month
logistics_dataset_n_1_doses_per_month <- logistics_dataset %>% mutate(month = format(jour, "%m"), year = format(jour, "%Y"), num_of_days_in_the_month = days_in_month(format(jour, "%Y-%m-%d"))) %>% group_by(reg, month, year) %>% 
  summarise(across(c("n_dose1_h", "n_dose1_f","n_dose1_e"), sum), .groups = 'drop')

logistics_dataset_n_1_doses_per_month <- logistics_dataset_n_1_doses_per_month %>% 
  group_by(reg, month) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(mean(n), min(n), max(n))

logistics_dataset_n_1_doses_per_month
# Convert tibble to df
logistics_dataset_n_1_doses_per_month_df <- logistics_dataset_n_1_doses_per_month %>% as.data.frame()

#-------------- Get all doses of each month ------------
# Calculate the total of doses for each month
logistics_dataset_n_1_doses_per_month_df["total_doses_per_month"] <- logistics_dataset_n_1_doses_per_month_df$n_dose1_h + logistics_dataset_n_1_doses_per_month_df$n_dose1_f + logistics_dataset_n_1_doses_per_month_df$n_dose1_e
View(logistics_dataset_n_1_doses_per_month_df)

logistics_dataset_n_1_doses_per_month_df %>% 
  group_by(reg, month) %>% 
  count() %>% 
  ungroup() %>% 
  summarise(mean(n), min(n), max(n))

# Divide by the number of days in the month in order to know the minimum doses needed to vaccinate the people
min_stock_per_day = logistics_dataset_n_1_doses_per_month_df["total_doses_per_month"] / logistics_dataset_n_1_doses_per_month_df["num_of_days_in_the_month"]
logistics_dataset_n_1_doses_per_month_df["min_stock_per_day"] = ceil(min_stocks_per_day)

View(logistics_dataset_n_1_doses_per_month_df)
#-------------- Get all doses of each year ------------
# test = logistics_dataset_n_1_doses_per_month_df %>% group_by(logistics_dataset_n_1_doses_per_month_df['month'].dt.year)['a'].agg(['sum', 'mean', 'max'])

logistics_dataset_n_1_doses_per_year_df <- logistics_dataset %>%
  mutate(month = format(jour, "%m"), year = format(jour, "%Y")) %>%
  group_by(year) %>%
  summarise(total_of_1_doses_per_year = sum(n_dose1_h, n_dose1_f, n_dose1_e))

logistics_dataset_n_1_doses_per_region_df <- logistics_dataset %>%
  mutate(year = format(jour, "%Y")) %>%
  group_by(reg, year) %>%
  summarise(total_of_1_doses_per_region = sum(n_dose1_h, n_dose1_f, n_dose1_e))

View(logistics_dataset_n_1_doses_per_year_df)
View(logistics_dataset_n_1_doses_per_region_df)
## Prédiction sur l’utilisation des vaccins ( Quel vaccin va être le plus utilisé ? ) OUI 3 ----

#Prédiction sur l’utilisation des vaccins ( Quel vaccin va être le plus utilisé ? ) OUI 3 ----

## Prédire s’il y’a une corrélation entre les campagnes de vaccination et l'apparition de nouveaux variants OUI 4 ----

## Essayer de prédire la ou les catégorie(s) d’âges qui sont plus réticentes à la vaccination OUI 5 ----
# DATA -> 
dataReticence = data.frame(covid_19_datasets_without_0s)
newDataReticence = data.frame() # Creation d'un nouveau dataFrame
# colnames(dataReticence) # Aide pour avoir les noms des colonnes
#dataReticence = dataReticence[c("reg","clage_vacsi","jour","n_dose1_h","n_dose1_f","n_dose1_e","n_complet_h","n_complet_f","n_complet_e")]

# Ajout des éléments interessants 
newDataReticence = dataReticence[c("reg","clage_vacsi","jour")]
newDataReticence['dose1Somme'] = dataReticence['n_dose1_h'] + dataReticence['n_dose1_f'] + dataReticence['n_dose1_e']
newDataReticence['nComplet'] = dataReticence['n_complet_h'] + dataReticence['n_complet_f'] + dataReticence['n_complet_e']

# Group By sur le DataFrame
# year_month = lubridate::year() 
View(newDataReticence)
newDataReticence = newDataReticence %>% 
  group_by(reg, clage_vacsi, month = format(as.Date(lubridate::floor_date(x=jour, unit="month"), "%Y-%m-%d"), "%Y-%m")) %>%
  summarize(across(c(dose1Somme, nComplet), sum))

# Affichage du DataFrame
View(newDataReticence)

# Prediction 
# Affichage graphique
