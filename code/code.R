
# 1. Packages and data ----------------------------------------------------
Packages <- c('tidyverse','readr', 'questionr', 'sandwich', 'lmtest', 'corrplot', 'magrittr',
              'psych','FactoMineR','GPArotation','randomForest')
lapply(Packages,library, character.only = TRUE)
df <- read_csv("clean_data/cleaned_data.csv")



source('code/functions.R')

df <- df %>%
  mutate(across(where(~all(unique(.[!is.na(.)]) %in% c("0","1"))),as.factor))

df_raw <- read_csv("raw_data/Student_performance_data _.csv")
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

# In mean and median, do we have a difference of PGA according to variables ?

# Regarding parents' variables
df %>%
  select(matches('Parental')) %>%
  map(Mean_per_variable)
# No clear conclusion for ParentalEducation
# For Parentalsupport, higher mean and higher median for Hig and Very High


# Regarding activities
df %>%
  select(where(is.factor)) %>%
  map(Mean_per_variable) %>%
  map(as.data.frame)
# We have a higher median (and mean) for students that practice an activity, except
# volunturing, also highe for tutoring

# For Absence and StudyTimeWeekly, we can't group_by() because so we split in two
df %>%
  mutate(Great_Absences = case_when(
         Absences > median(Absences) ~ '1',
         Absences <= median(Absences) ~ '0')) %>%
  group_by(Great_Absences) %>%
  summarise(Mean = mean(GPA),
         Median = median(GPA))
# For students above the Absences' mediian (Great_Absences == 1)
# GPA' mean and median are clearly under the level of those students that are less absent

df %>%
  mutate(Great_StudyTimeWeekly = case_when(
    StudyTimeWeekly > median(StudyTimeWeekly) ~ '1',
    StudyTimeWeekly <= median(StudyTimeWeekly) ~ '0')) %>%
  group_by(Great_StudyTimeWeekly) %>%
  summarise(Mean = mean(GPA),
            Median = median(GPA))

df %>%
  mutate(Great_StudyTimeWeekly = case_when(
    StudyTimeWeekly > median(StudyTimeWeekly) ~ '1',
    StudyTimeWeekly <= median(StudyTimeWeekly) ~ '0')) %>%
  ggplot() +
  aes(x = GPA, colour = Great_StudyTimeWeekly)  +
  stat_ecdf(geom = "step", pad = FALSE)

CumulativeGPA_Absence <- df %>%
  mutate(Great_Absences = case_when(
    Absences > median(Absences) ~ '1',
    Absences <= median(Absences) ~ '0'
  )) %>%
  ggplot() +
  aes(x = GPA, colour = Great_Absences)  +
  stat_ecdf(geom = "step", pad = FALSE) +
  labs(y = 'Cumulative probability', title = 'Cumulative distribution of GPA according to absence') +
  scale_color_discrete(name = "Important numbers of absence", labels = c("No", "Yes")) +
  theme(legend.position = "bottom") +
  theme_minimal() 

ggsave('CumulativeGPA_absence.png',width = 4, height = 4, path = 'graph')
# Coherent results for StudyTimeWeekly. Students that work during a longer period
# (Great_StudyTimeWeekly  == 1) as a higher mean and median in results.





# Create a table of two lists, representing all table combinations that can be interesting
# (eg : table(Gender,GradeClass) then table(Ethnicity,GradeClass))
Grade_class_case <- df %>%
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

chi <- map2(Grade_class_case$Var1,
                    Grade_class_case$GradeClass,
                    ~ chisq.test(df[[.x]], df[[.y]]))

# 4. Correlation's studying -----------------------------------------------
corr <-  df_raw %>%
  select(-c('StudentID', 'GPA')) %>% 
  {cor(.)}

corrplot(corr)
# No correlation between variables, two exceptions :
# A strong positive correlation between Absence and GradeClass
# Very small and negative correlations for GradeClass and Tutoring, ParentalSupport

bartlett.test(df_raw %>%
                select(-c('StudentID','GPA')))

PCA <- prcomp(corr, scale = TRUE)
summary(PCA)
screeplot(PCA, npcs = 10,  type="lines")
abline(h=1, col="red")
# We keep 9 axes
eig <-round((PCA$sdev)^2, digits = 2)
eig


# 5. Model ----------------------------------------------------------------
model1 <- lm(GPA ~ as.factor(ParentalSupport) + StudyTimeWeekly + Absences + as.factor(Gender) - 1 ,
             data = df_raw)
summary(model1)
coeftest(model1, vcov. = 'HC1')
absence_GPA_plot <- ggplot(df_raw_ft) +
  aes(x = Absences, y = GPA) +
  geom_point(shape = "circle",
             size = 0.75,
             colour = "#EF562D") +
  geom_smooth(span = 0.25) +
  labs(title = "GPA's evolution regarding Absence", caption = "Absences' increase seems to lead to a decrease of GPA") +
  theme_minimal()

ggsave('absence_GPA_plot.png',width = 5,height = 5, path = 'graph')


# 6. Forest Tree ----------------------------------------------------------

# We have to work from the raw version of data, to have numerical categorial values
Non_factors <- c('StudentID','Age','StudyTimeWeekly','Absences','GradeClass','GPA')
df_raw_ft <- df_raw %>%
  mutate(across(!all_of(Non_factors), as.factor)) %>%
  select(-c('StudentID','GradeClass'))

rf <- randomForest(GPA ~ ., data =df_raw_ft, proximity = TRUE )
# Default value of trees is 500
# Defaut value of variables (number of variables divided by 3 for regression)
summary(rf)





