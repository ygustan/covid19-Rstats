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
correlations <- cor(x = new_data_correlation[,c("rappel1", "rappel3")], y=new_data_correlation$rappel2)
correlations1 <- cor(x = new_data_correlation[,c("rappel1", "rappel2")], y=new_data_correlation$rappel3)
correlations2 <- cor(x = new_data_correlation[,c("rappel2", "rappel3")], y=new_data_correlation$rappel1)

correlations
correlations1
correlations2
# Correlation of Person
correlation1 <- cor(new_data_correlation$rappel1, new_data_correlation$rappel2)
correlation1_corrected <- cor(log(new_data_correlation$rappel1), new_data_correlation$rappel2)
correlation2 <- cor(new_data_correlation$rappel2, new_data_correlation$rappel3)
plot(new_data_correlation$rappel1, new_data_correlation$rappel2, main=paste("Correlation:", round(correlation1_corrected, 2)), xlab = "1er Rappel", ylab="2ème Rappel" )

correlation1_corrected
# Plot the correlation using ggplot2
new_data_correlation %>%
  mutate(rappel1_log = log(rappel1)) %>%
  ggplot(aes(x=rappel1_log, y=rappel2, color=clage_vacsi)) +
  geom_point() +
  # scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Avr", "Mai", "Juin", "Jul", "Août", "Sep", "Oct", "Nov", "Dec")) +
  labs(x = "Rappel 1 transformé (log(rappel1))", y = "Rappels", title = "Correlations entre les rappels pour toutes les classes d'âge") +
  geom_smooth(method = "lm", se=TRUE, color="black")
  ggtitle(paste("Correlations :", round(cor(correlations, c(new_data_correlation$appel1, new_data_correlation$rappel2, new_data_correlation$rappel3)), 2),
                "(p-value:", round(cor(correlations, c(new_data_correlation$rappel1, new_data_correlation$rappel2, new_data_correlation$rappel3)), 2), ")"))

# Calculate Spearman's rank correlation coefficient between columns 'A' and 'B'
sperman_correlation_1 <- cor.test(new_data_correlation$rappel1, new_data_correlation$rappel2, method='spearman')
hendall_correlation_1 <- cor.test(new_data_correlation$rappel1, new_data_correlation$rappel2, method='kendall')
# sperman_correlation_1 <- cor.test(new_data_correlation$rappel1, new_data_correlation$rappel2, method='spearman')

sperman_correlation_1
hendall_correlation_1
# Plot the correlation using the plot function
plot(new_data_correlation$rappel1, new_data_correlation$rappel2, main=paste("Correlation:", round(sperman_correlation_1$estimate, 2)), xlab="Rappel 1", ylab="Rappel 2")

new_data_correlation %>%
  mutate(rappel1_log = log(rappel1)) %>%
  ggplot(aes(x=rappel1_log, y=rappel2, color=clage_vacsi)) +
  geom_point() +
  labs(x = "Rappel 1 transformé (log(rappel1))", y = "Rappels", title = "Correlations entre les rappels pour toutes les classes d'âge") +
  geom_smooth(method = "lm", se=TRUE, color="blue")
  ggtitle(paste("Correlation de Spearman enrete les rappels 1 et rappels 2:", round(sperman_correlation_1, 2),
              "(p-value:", round(sperman_correlation_1, 2),")"))
# Create a scatterplot matrix
# scatterplotMatrix(data = new_data_correlation, x=correlations, smooth = TRUE, diag.panel = histogram)
chart.Correlation(new_data_correlation[,c("rappel1", "rappel2", "rappel3")], histogram = TRUE, method = "pearson")
chart.Correlation(new_data_correlation[,c("rappel1", "rappel2", "rappel3")], method = "kendall")
chart.Correlation(new_data_correlation[,c("rappel1", "rappel2", "rappel3")], histogram = False, method = "spearman")
