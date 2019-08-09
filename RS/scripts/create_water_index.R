### Created by: Diego Afonso de Castro
### Date: 31/07/2019
### Objective: Create water index (2017)

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)


# Load and process data ---------------------------------------------------

data <- read_excel("input/SNIS/ConsolidadoMunicipio-2017.xlsx")

# check NAs
data_nas <- apply(data, 2, function(x) sum(is.na(x)))

data_final <- data %>% 
  select(c(1, 2, 3, 21, 22, 27)) %>% 
  rename(city_code = names(.)[1],
         city = names(.)[2],
         state = names(.)[3],
         water_distribution_loss = names(.)[4],
         water_supply_index = names(.)[5],
         water_total_coliforms_index = names(.)[6]) %>% 
  na.omit() %>% 
  mutate(water_quality_index = 100 - water_total_coliforms_index,
         water_index = ((water_distribution_loss + water_supply_index + water_quality_index)/3)/100) %>% 
  select(-water_total_coliforms_index)


# Save data ---------------------------------------------------------------

write_csv(x = data_final,
          path = "input/model/water_index.txt",
          append = FALSE,
          col_names = TRUE)


