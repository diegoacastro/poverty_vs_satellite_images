### Created by: Diego Afonso de Castro
### Date: 07/07/2019
### Objective: extract GDP per capita data for Brazilian cities (2016)

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# Import data -------------------------------------------------------------

### Source: https://sidra.ibge.gov.br/Tabela/5938
### Source: https://sidra.ibge.gov.br/Tabela/6579

gdp_data <- read_excel(path = "input/censo/tabela5938.xlsx",
                       sheet = 1,
                       col_names = TRUE,
                       col_types = c(rep("text", 2), "numeric")) %>% 
  rename(city_code = `Cód.`, city = `Brasil e Município`, gdp = `2016`)

population_data <- read_excel(path = "input/censo/tabela6579.xlsx",
                         sheet = 1,
                         col_names = TRUE,
                         col_types = c(rep("text", 2), "numeric")) %>% 
  rename(city_code = `Cód.`, city = `Brasil e Município`, population = `2016`)

gdp_per_capita_data <- gdp_data %>% 
  left_join(., population_data, by = c("city_code", "city")) %>% 
  mutate(gdp_per_capita = round(gdp / population, 3)) %>% 
  separate(col = city, sep = -4, into = c("city", "state")) %>% 
  mutate(state = str_remove_all(state, "[()]")) %>% 
  select(city_code, city, state, gdp_per_capita)


# Save data ---------------------------------------------------------------
# Write data frame file
write_csv(x = gdp_per_capita_data,
          path = "input/model/gdp_per_capita_df.txt",
          append = FALSE,
          col_names = TRUE)
