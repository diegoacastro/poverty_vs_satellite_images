### Created by: Diego Afonso de Castro
### Date: 17/07/2019
### Objective: Relate images names to cities and salaries

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/download_coordinates.txt")

income_data <- data.table::fread("input/model/income_avg_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code = code, income)



# Join DFs and get the mean radiance per city -----------------------------
income_files <- lights %>% 
  inner_join(., income_data, by = "city_code") %>% 
  mutate(image_files = ifelse(round(radiance * 100) <= 87, 
                              paste0("input/google_images/class1/", city_code, 
                                     "_", row_raster, "_", col_raster, "_",
                                     as.character(round(radiance * 100)), ".png"),
                              ifelse(round(radiance * 100) <= 1037,
                                     paste0("input/google_images/class2/", city_code, 
                                            "_", row_raster, "_", col_raster, "_",
                                            as.character(round(radiance * 100)), ".png"),
                                     paste0("input/google_images/class3/", city_code, 
                                            "_", row_raster, "_", col_raster, "_",
                                            as.character(round(radiance * 100)), ".png")))) %>% 
  select(city_code, income, image_files)



# Save file ---------------------------------------------------------------

data.table::fwrite(x = income_files,
                   file = "input/model/income_city_file.txt",
                   sep = ",",
                   dec = ".",
                   row.names = FALSE,
                   col.names = TRUE)
