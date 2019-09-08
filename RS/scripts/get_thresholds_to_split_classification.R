### Created by: Diego Afonso de Castro
### Date: 01/08/2019
### Objective: Get thresholds to label cities into two classes

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# GDP PER CAPITA ----------------------------------------------------------

### Source: https://sidra.ibge.gov.br/Tabela/5938
### Source: https://sidra.ibge.gov.br/Tabela/6579

gdp_data <- read_excel(path = "input/censo/tabela5938_all_cities.xlsx",
                       sheet = 1,
                       col_names = TRUE) %>% 
  select(city_code = `Cód.`, city = `Município`, gdp = `PIB_mil`)

population_data <- read_excel(path = "input/censo/tabela6579_all_cities.xlsx",
                              sheet = 1,
                              col_names = TRUE) %>% 
  select(city_code = `Cód.`, city = `Município`, population = `Populacao`)

gdp_per_capita_data <- gdp_data %>% 
  left_join(., population_data, by = c("city_code", "city")) %>% 
  mutate(gdp_per_capita = round(gdp / population, 3))

print(paste("Median of GDP per capita:", 
            median(gdp_per_capita_data$gdp_per_capita, na.rm = TRUE)))

print(paste("Mean of GDP per capita:", 
            mean(gdp_per_capita_data$gdp_per_capita, na.rm = TRUE)))


# Average Income ----------------------------------------------------------

### Source: https://sidra.ibge.gov.br/Tabela/3548 

mean_income_data <- read_excel(path = "input/censo/tabela3548_all_cities.xlsx",
                               sheet = 1,
                               col_names = TRUE,
                               skip = 6)

print(paste("Median of average income:", 
            median(mean_income_data$Total, na.rm = TRUE)))

print(paste("Mean of average income:", 
            mean(mean_income_data$Total, na.rm = TRUE)))


# Water index -------------------------------------------------------------

### Source: http://app4.cidades.gov.br/serieHistorica/#

data <- read_excel("input/SNIS/ConsolidadoMunicipio-2017_all_cities.xlsx")

data_nas <- apply(data, 2, function(x) sum(is.na(x)))

data_final <- data %>% 
  select(c(1, 2, 3, 7, 8, 9)) %>% 
  rename(city_code = names(.)[1],
         city = names(.)[2],
         state = names(.)[3],
         water_distribution_loss = names(.)[4],
         water_supply_index = names(.)[5],
         water_total_coliforms_index = names(.)[6]) %>% 
  na.omit() %>% 
  mutate(water_quality_index = 100 - water_total_coliforms_index,
         water_index = ((water_distribution_index + water_supply_index + water_quality_index)/3)/100) %>% 
  select(-c(water_total_coliforms_index, water_distribution_loss))

print(paste("Median of water index:", 
            median(data_final$water_index, na.rm = TRUE)))

print(paste("Mean of water index:", 
            mean(data_final$water_index, na.rm = TRUE)))
