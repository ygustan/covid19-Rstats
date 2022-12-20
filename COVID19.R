library(readr)
covid_19_datasets <- read_delim("https://static.data.gouv.fr/resources/donnees-relatives-aux-personnes-vaccinees-contre-la-covid-19-1/20221219-190045/vacsi-s-a-reg-2022-12-19-19h00.csv", 
delim = ";", escape_double = FALSE, na = "NA", trim_ws = TRUE)
                                                       
# Show the dataset
View(covid_19_datasets)


