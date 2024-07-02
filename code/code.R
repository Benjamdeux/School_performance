
# 1. Packages and data ----------------------------------------------------
Packages <- c('tidyverse','readr', 'questionr')
lapply(Packages,library, character.only = TRUE)
df <- read_csv("clean_data/cleaned_data.csv")

View(df)
summary(df)

df <- df %>%
  mutate(across(where(~all(unique(.[!is.na(.)]) %in% c("0","1"))),as.factor))


# 2. Descriptive statistics  -----------------------------------------------
 
summary(df)
df %>%
  select(!where(is.double)) %>%
  map(table) %>% 
  map(freq)

# * All students are between 15 ans 18 years old, with approximatively the same
# number of male and female students. Caucasians represent about half the sample.
# We can add that students are not found of extra-scolar activities (like : musics, sports,Volunteering)
# * Regarding parents, half of the population has high school level or superior
# and 40% of them consider their help to be as a high or very high level towards theirs kids.

# According to Class variables, StudyTimeWeekly and absences are quite symmetrical (comparison mean, median),
# as for GPA.



# 3. First link between GPA and variables ---------------------------------

# Cumulative distribution of GPA. 25% of students have a PGA of 1.15.
ggplot(df, aes(x = GPA)) +
  stat_ecdf(geom = "step", colour ='darkviolet', pad = FALSE) +
  theme_minimal()



# Create a table of two lists, representing all table combinations that can be interesting
# (eg : table(Gender,GradeClass) then table(Ethnicity,GradeClass))
df %>%
  select(-c('StudentID','Age','GPA')) %>%
  names() %>% 
  as.data.frame()%>%
  rename('Var1' = '.') %>%
  mutate(GradeClass = 'GradeClass')

# We map the table function over the two vectors simultaneously
table_Grade <- map2(Grade_class_case$Var1,
                    Grade_class_case$GradeClass,
                    ~ table(df[[.x]], df[[.y]])) %>%
  map(cprop)




