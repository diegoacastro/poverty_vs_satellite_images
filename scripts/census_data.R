### Created by: Diego Afonso de Castro
### Date: 06/07/2019
### Objective: extract income data from Brazilian Census (2010)

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)
library(readxl)


# Import data -------------------------------------------------------------

### Source: https://sidra.ibge.gov.br/tabela/3548

mean_income_data <- read_excel(path = "input/censo/tabela3548.xlsx",
                               sheet = 1,
                               col_names = TRUE,
                               col_types = c(rep("text", 5), "numeric"),
                               skip = 5) %>% 
  select(code = `Cód.`, city = `Município`, income = Total) %>% 
  separate(col = city, sep = -4, into = c("city", "state")) %>% 
  mutate(state = str_remove_all(state, "[()]"))


# Save data ---------------------------------------------------------------
# Write data frame file
write_csv(x = mean_income_data,
          path = "input/model/income_df.txt",
          append = FALSE,
          col_names = TRUE)

# a <- read_csv(file = "input/model/income_df.txt")
