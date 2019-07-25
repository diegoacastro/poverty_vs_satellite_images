library(tidyverse)

salary <- read_csv("input/model/average_salary_df.txt")
income <- read_csv("input/model/income_df.txt")
gdp <- read_csv("input/model/gdp_per_capita_df.txt")

salary_income = left_join(salary, income, by = c("city_code" = "code")) %>% 
  left_join(., gdp, by = c("city_code")) %>% 
  filter(!is.na(income))

cor_income_salary <- cor(salary_income$average_wage, salary_income$income)
cor_income_gdp <- cor(salary_income$gdp_per_capita, salary_income$income)
cor_salary_gdp <- cor(salary_income$gdp_per_capita, salary_income$average_wage)
