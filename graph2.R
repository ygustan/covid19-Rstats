# Import ----
library(readr)
library(rlang)
library(lubridate)
library(dplyr)
library(stats)
# library("Hmisc")
# library(tidyverse)

## Importation des données ----
covid_19_datasets <- read_delim("https://www.data.gouv.fr/fr/datasets/r/8e5e70fa-c082-45e3-a7b8-20862711b142", 
                                delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)
# View(covid_19_datasets)


# Show the dataset without lines with only zeros ----
covid_19_datasets_without_0s = data.frame(covid_19_datasets)
covid_19_datasets_without_0s <- covid_19_datasets_without_0s[!apply(covid_19_datasets_without_0s[-1:-3], 1, function(x) all(x == 0)), ]
# View(covid_19_datasets_without_0s)

## Prédire la gestion logistique(stocks, etc ) d’une campagnes  de vaccinations de l’année suivante sur chaque mois pour chaque région  OUI 2 ----

# General stuffs
# View(logistics_dataset)
logistics_dataset = data.frame(covid_19_datasets_without_0s)
# new_data_correlation <- new_data_correlation[!apply(new_data_correlation[-1:-3], 1, function(x) all(x == 0)), ]
# View(new_data_correlation)
#-------------- Get all doses of all periods ------------
logistics_dataset = logistics_dataset[, c("reg", "clage_vacsi", "jour", "n_dose1_h", "n_dose1_f","n_dose1_e")]
# tracemem(logistics_dataset) == tracemem(covid_19_datasets_without_0s)
logistics_dataset <- logistics_dataset %>%
  mutate(total_of_doses_1_of_all_time) %>% 
  # transmute(reg, clage_vacsi, jour, n_dose1_h, n_dose1_f, n_dose1_e) %>% 
  group_by(reg, clage_vacsi, jour, n_dose1_h, n_dose1_f, n_dose1_e) %>%
  summarise(total_of_1_doses_per_region_per_age_class_per_day = sum(n_dose1_h, n_dose1_f, n_dose1_e), .groups = 'drop')

logistics_dataset
logistics_dataset <- logistics_dataset %>%
  mutate_at(vars(n_dose1_h:n_dose1_f:n_dose1_e) , funs(P = ./logistics_dataset$total_of_1_doses_per_region_per_age_class_per_day * 100))

View(logistics_dataset)
# logistics_dataset <- cbind(logistics_dataset, c("Total of 1 doses of all time For Men", total_of_doses_1_of_all_time))

# total_of_doses_1_of_all_time

#---------- Specific stuffs --------------
#-------------- Get all doses of each month ------------
# Group by sum using dplyr
# num_of_days_on_each_month = days_in_month(as.Date(lubridate::floor_date(x=logistics_dataset$jour, unit="month"), "%Y-%m-%d"))
# num_of_days_on_each_month
logistics_dataset_n_1_doses_per_month <- logistics_dataset %>%
  mutate(num_of_days_in_the_month = days_in_month(format(jour, "%Y-%m-%d")), month = format(jour, "%m"), year=format(jour, "%Y")) %>%
  group_by(reg, month, year, num_of_days_in_the_month) %>% 
  summarise( across(c("n_dose1_h", "n_dose1_f","n_dose1_e"), sum), .groups = 'drop')


logistics_dataset_n_1_doses_per_month
# Convert tibble to df
logistics_dataset_n_1_doses_per_month_df <- logistics_dataset_n_1_doses_per_month %>% as.data.frame()

#-------------- Get all doses of each month ------------
# Calculate the total of doses for each month
logistics_dataset_n_1_doses_per_month_df["total_doses_per_month"] <- logistics_dataset_n_1_doses_per_month_df$n_dose1_h + logistics_dataset_n_1_doses_per_month_df$n_dose1_f + logistics_dataset_n_1_doses_per_month_df$n_dose1_e

# logistics_dataset_n_1_doses_per_month_df <- logistics_dataset_n_1_doses_per_month_df %>% 
#   group_by(reg, month) %>% 
#   count() %>% 
#   ungroup() %>% 
#   summarise(mean(n), min(n), max(n))

# View(logistics_dataset_n_1_doses_per_month_df)

# logistics_dataset_n_1_doses_per_month_df %>% 
#   group_by(reg, month) %>% 
#   count() %>% 
#   ungroup() %>% 
#   summarise(mean(n), min(n), max(n))

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
  summarise(
    total_of_1_doses_per_region = sum(n_dose1_h, n_dose1_f, n_dose1_e),
    # x_prop = prop.table(logistics_dataset_n_1_doses_per_region_df$total_of_1_doses_per_region),
    .groups = 'drop'
  )

# View(logistics_dataset_n_1_doses_per_year_df)
View(logistics_dataset_n_1_doses_per_region_df)

# Calculate the proportions of each element in the x column
# x_prop <- prop.table(logistics_dataset_n_1_doses_per_region_df$total_of_1_doses_per_region)

# x_prop
# Calculate the proportions of each element in the y column
# y_prop <- prop.table(df$y)

# Divide the x proportions by the y proportions to get the proportionality ratio
# ratio <- x_prop / y_prop

# View the ratio
# print(ratio)

# logistics_dataset_n_1_doses_per_region_df %>%
#   group_by(reg, year) %>%
#   summarise(ratio = sum(n_dose1_h, n_dose1_f, n_dose1_e) / sum(total_of_1_doses_per_region))
# summary(logistics_dataset$n_dose1_h)

# Plot
# plot(x=logistics_dataset_n_1_doses_per_region_df$year, y=logistics_dataset_n_1_doses_per_region_df$total_of_1_doses_per_region, main="Graphiques du nombre de doses par région par année", sub="subtitle",
#      xlab="Années", ylab="Nombre total de doses")
# Only find the numerics values in a dataframe
only_nums <- unlist(lapply(logistics_dataset[c("n_dose1_h_P", "n_dose1_f_P", "n_dose1_e_P")], is.numeric), use.names = FALSE)  
boxplot(logistics_dataset[, c("n_dose1_h_P", "n_dose1_f_P", "n_dose1_e_P")])

# logistics_dataset_n_1_doses_per_region_df %>%
#   ggplot(aes(x=year, xlab = "Années", y=total_of_1_doses_per_region, ylab = "Total de doses par region", group=reg, color=reg)) +
#   geom_line()

logistics_dataset_n_1_doses_per_month_df_2021 <- logistics_dataset_n_1_doses_per_month_df %>%
  filter(paste(year, month, sep = "-") %in% c("2021-01","2021-02","2021-03","2021-04","2021-05","2021-06","2021-07","2021-08","2021-09","2021-10", "2021-11", "2021-12"))

logistics_dataset_n_1_doses_per_month_df_2022 <- logistics_dataset_n_1_doses_per_month_df %>%
  filter(paste(year, month, sep = "-") %in% c("2022-01","2022-02","2022-03","2022-04","2022-05","2022-06","2022-07","2022-08","2022-09","2022-10", "2022-11", "2022-12"))

# logistics_dataset_n_1_doses_per_month_df
# View(logistics_dataset_n_1_doses_per_month_df)
# logistics_dataset_n_1_doses_per_month_df %>%
#   ggplot(aes(x=month, xlab = "Mois", y=total_doses_per_month, ylab = "Total de doses par Mois", group=reg, color=reg)) +
#   geom_line()

# month_date <- as.Date(logistics_dataset_n_1_doses_per_month_df$month, format = "%Y-%m")
# month_date
# logistics_dataset_n_1_doses_per_month_df$month <- as.Date(paste(logistics_dataset_n_1_doses_per_month_df$month, "01"), format = "%Y-%m-%d")
# logistics_dataset_n_1_doses_per_month_df$month <- as.Date(logistics_dataset_n_1_doses_per_month_df$month)
# Create the plot
# ggplot(data = logistics_dataset_n_1_doses_per_month_df, aes(x = month, y = total_doses_per_month, color = reg)) +
#   geom_line() +
#   scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
#   labs(x = "Mois", y = "Total de doses par Mois d chaque Année", color = month)

# Create the plot
View(logistics_dataset_n_1_doses_per_month_df_2021)
View(logistics_dataset_n_1_doses_per_month_df_2022)
logistics_dataset_n_1_doses_per_month_df_2021 %>%
  ggplot(aes(x = paste(year, month, sep = '-'), y = total_doses_per_month)) +
  geom_line() +
  scale_x_discrete(labels = c("Jan 2021", "Feb 2021", "Mar 2021", "Avr 2021", "Mai 2021", "Juin 2021", "Jul 2021", "Août 2021", "Sep 2021", "Oct 2021", "Nov 2021", "Dec 2021")) +
  labs(x = "Mois et Années", y = "Total de 1ers doses", title = " Total des doses par région et par mois de l'année 2021")

logistics_dataset_n_1_doses_per_month_df_2022 %>%
  ggplot(aes(x = paste(year, month, sep = '-'), y = total_doses_per_month)) +
  geom_line() +
  scale_x_discrete(labels = c("Jan 2022", "Feb 2022", "Mar 2022", "Avr 2022", "Mai 2022", "Juin 2022", "Jul 2022", "Août 2022", "Sep 2022", "Oct 2022", "Nov 2022", "Dec 2022")) +
  labs(x = "Mois et Années", y = "Total des 1ers doses", title = " Total des doses par région et par mois de l'année 2022")

