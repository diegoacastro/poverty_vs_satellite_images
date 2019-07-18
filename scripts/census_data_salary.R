### Created by: Diego Afonso de Castro
### Date: 16/07/2019
### Objective: extract 2017 salaries (of those that have jobs)

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# Import data -------------------------------------------------------------

### Source: https://sidra.ibge.gov.br/Tabela/6449

salary_data <- read_excel(path = "input/censo/tabela6449.xlsx",
                          sheet = 8,
                          skip = 4,
                          col_names = TRUE,
                          col_types = c(rep("text", 3), "numeric")) %>% 
  select(city_code = `Cód.`, city = `Município`, wage = `Salario(Mil)`)

people_data <- read_excel(path = "input/censo/tabela6449.xlsx",
                          sheet = 4,
                          skip = 4,
                          col_names = TRUE,
                          col_types = c(rep("text", 3), "numeric")) %>% 
  select(city_code = `Cód.`, city = `Município`, number_workers = `Pessoas_ocupadas`)

average_salary <- salary_data %>% 
  left_join(., people_data, by = "city_code") %>% 
  mutate(average_wage = round(wage * 1000 / number_workers, 3)) %>% 
  separate(col = city.x, sep = -4, into = c("city", "state")) %>% 
  mutate(state = str_remove_all(state, "[()]")) %>% 
  select(city_code, city, state, average_wage)


# Save data ---------------------------------------------------------------
# Write data frame file
write_csv(x = average_salary,
          path = "input/model/average_salary_df.txt",
          append = FALSE,
          col_names = TRUE)
