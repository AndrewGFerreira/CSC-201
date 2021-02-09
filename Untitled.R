library(dplyr)


data_object %>%
  filter() %>%
  mutate[]

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

OK <- salary_potential[salary_potential$name == 'Oral Roberts University',]
mean(salary_potential$make_world_better_percent, na.rm = TRUE)
mean(salary_potential$stem_percent, na.rm = TRUE)
df1 <- salary_potential[salary_potential$make_world_better_percent >= 60 &
                          salary_potential$state_name == 'Colorado', ]

na.omit(df1)
df1 <- df1[ ! is.na(df1$rank),]

avg <- mean(salary_potential$early_career_pay)

salary_potential[salary_potential$early_career_pay >= avg,]

salary_potential$stem_percent < 25 
salary_potential$early_career_pay > avg

