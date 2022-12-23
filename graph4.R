## Import ----
# install.packages("tidyverse")
install.packages("car")
install.packages("PerformanceAnalytics")
library(car)
library(readr)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(PerformanceAnalytics)

## Importation des données ----
covid_19_datasets <- read_delim("https://www.data.gouv.fr/fr/datasets/r/8e5e70fa-c082-45e3-a7b8-20862711b142", 
                                delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)

## Les données ----
# View(covid_19_datasets)

## Prédire s’il y’a une corrélation entre les campagnes de vaccination et l'apparition de nouveaux variants OUI 4 ----
data_correlation = data.frame(covid_19_datasets[, c("reg","clage_vacsi","jour", "n_rappel_h", "n_rappel_f", "n_rappel_e", "n_2_rappel_h", "n_2_rappel_f", "n_2_rappel_e", "n_3_rappel_h", "n_3_rappel_f", "n_3_rappel_e")])

new_data_correlation = data_correlation[c("reg","clage_vacsi","jour")]
# new_data_correlation['rappel1'] = rowSums(data_correlation[c("n_rappel_h", "n_rappel_f", "n_rappel_e")])
new_data_correlation['rappel1'] = data_correlation['n_rappel_h'] + data_correlation['n_rappel_f'] + data_correlation['n_rappel_e']
new_data_correlation['rappel2'] = data_correlation['n_2_rappel_h'] + data_correlation['n_2_rappel_f'] + data_correlation['n_2_rappel_e']
new_data_correlation['rappel3'] = data_correlation['n_3_rappel_h'] + data_correlation['n_3_rappel_f'] + data_correlation['n_3_rappel_e']
# new_data_correlation <- new_data_correlation[!apply(new_data_correlation[-1:-3], 1, function(x) all(x == 0)), ]
# View(new_data_correlation)
# Calculate the correlations between the columns
correlations <- cor(x = new_data_correlation[,c("rappel1", "rappel2", "rappel3")], y=new_data_correlation$rappel2)

correlations
# Correlation of Person
correlation1 <- cor(new_data_correlation$rappel1, new_data_correlation$rappel2)
correlation2 <- cor(new_data_correlation$rappel2, new_data_correlation$rappel3)
plot(new_data_correlation$rappel1, new_data_correlation$rappel2, main=paste("Correlation:", round(correlation1, 2)), xlab = "1er Rappel", ylab="2ème Rappel" )


# Plot the correlation using ggplot2
ggplot(new_data_correlation, aes(x=rappel1, y=rappel2)) +
  geom_point() +
  ggtitle(paste("Correlation:", round(correlation1, 2), 
                "(p-value:", round(correlation1, 2), ")"))

# Calculate Spearman's rank correlation coefficient between columns 'A' and 'B'
sperman_correlation_1 <- cor.test(new_data_correlation$rappel1, new_data_correlation$rappel2, method='spearman')

# Plot the correlation using the plot function
plot(new_data_correlation$rappel1, new_data_correlation$rappel2, main=paste("Correlation:", round(sperman_correlation_1$estimate, 2)))


# Create a scatterplot matrix
# scatterplotMatrix(data = new_data_correlation, x=correlations, smooth = TRUE, diag.panel = histogram)
chart.Correlation(new_data_correlation[,c("rappel1", "rappel2", "rappel3")], histogram = TRUE, method = "pearson")
