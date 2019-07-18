# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# Correlation average income and gdp per capita (2010)

gdp_data <- read_excel(path = "input/censo/tabela5938_2010.xlsx",
                       sheet = 1,
                       col_names = TRUE,
                       col_types = c(rep("text", 2), "numeric")) %>% 
  rename(city_code = `Cód.`, city = `Município`, gdp = `PIB_mil(2010)`)

population_data <- read_excel(path = "input/censo/tabela1378_2010.xlsx",
                              sheet = 1,
                              col_names = TRUE,
                              col_types = c(rep("text", 2), "numeric")) %>% 
  rename(city_code = `Cód.`, city = `Mun.`, population = `Populacao(2010)`)

gdp_per_capita_data <- gdp_data %>% 
  left_join(., population_data, by = c("city_code", "city")) %>% 
  mutate(gdp_per_capita = round(gdp * 1000 / population, 3)) %>% 
  separate(col = city, sep = -4, into = c("city", "state")) %>% 
  mutate(state = str_remove_all(state, "[()]")) %>% 
  select(city_code, city, state, gdp_per_capita)


mean_income_data <- read_excel(path = "input/censo/tabela3548.xlsx",
                               sheet = 1,
                               col_names = TRUE,
                               col_types = c(rep("text", 5), "numeric"),
                               skip = 5) %>% 
  select(city_code = `Cód.`, city = `Município`, income = Total) %>% 
  separate(col = city, sep = -4, into = c("city", "state")) %>% 
  mutate(state = str_remove_all(state, "[()]"))


income_gdp <- left_join(mean_income_data, gdp_per_capita_data, by = c("city_code"))

a <- cor(income_gdp$income, income_gdp$gdp_per_capita, method = "pearson")
