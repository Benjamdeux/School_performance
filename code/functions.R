
Mean_per_variable <- function(var){
  df %>%
    group_by({{var}}) %>%
    summarise(Mean = mean(GPA),
              Median = median(GPA))
}
