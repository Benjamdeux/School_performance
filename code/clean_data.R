# 1. Packages et database -------------------------------------------------

library(tidyverse)
library(readr)
df_raw <- read_csv("raw_data/Student_performance_data _.csv")

View(df_raw)

# Nettoyage de la base de données et recodage des variables :
# * Genre, etchnicité, Education des parents, aide des parents, notes

df <- df_raw %>%
  mutate(Gender = Gender %>%
           as.character() %>%
           fct_recode("Male" = "0", "Female" = "1") %>%
           as.factor()) %>%
  mutate(Ethnicity = Ethnicity %>%
           as.character() %>%
           fct_recode(
             "Caucasian" = "0",
             "African American" = "1",
             "Asian" = "2",
             "Other" = "3") %>%
           as.factor()) %>%
  mutate(ParentalEducation = ParentalEducation %>%
           as.character() %>%
           fct_recode("None"= "0",
                      "High School" = "1",
                      "Some College" = "2",
                      "Bachelor's" = "3",
                      "Higher" = "4") %>%
           as.factor()) %>%
  mutate(ParentalSupport = ParentalSupport %>%
           as.character() %>%
           fct_recode('None' = '0',
                      'Low' = '1',
                      'Moderate' = '2',
                      'High' = '3',
                      'Very High' = '4') %>%
           as.factor()) %>%
  mutate(GradeClass = GradeClass %>%
           as.character() %>%
           fct_recode('A' = '0',
                      'B' = '1',
                      'C' = '2',
                      'D' = '3',
                      'E' =  '4') %>%
           as.factor())

# Sauvegarde de la base de donnée nettoyée dans le dossier clean_data
write.csv(df, file = 'clean_data/cleaned_data.csv', row.names = FALSE)



