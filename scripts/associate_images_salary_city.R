### Created by: Diego Afonso de Castro
### Date: 17/07/2019
### Objective: Relate images names to cities and salaries

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(tidyverse)


# Import data -------------------------------------------------------------
lights <- data.table::fread("input/model/download_coordinates.txt")

salary_data <- data.table::fread("input/model/average_salary_df.txt") %>% 
  filter(state == "RS") %>% 
  select(city_code, average_wage)


# Join DFs and get the mean radiance per city -----------------------------
salary_files <- lights %>% 
  left_join(., salary_data, by = "city_code") %>% 
  filter(!is.na(average_wage)) %>% 
  mutate(image_files = ifelse(radiance < 0.65, 
                              paste0("input/google_images/class1/", city_code, 
                                     "_", row_raster, "_", col_raster, "_",
                                     as.character(round(radiance * 100)), ".png"),
                              ifelse(radiance <= 2.55,
                                     paste0("input/google_images/class2/", city_code, 
                                            "_", row_raster, "_", col_raster, "_",
                                            as.character(round(radiance * 100)), ".png"),
                                     paste0("input/google_images/class3/", city_code, 
                                            "_", row_raster, "_", col_raster, "_",
                                            as.character(round(radiance * 100)), ".png")))) %>% 
  select(city_code, average_wage, image_files)



# Save file ---------------------------------------------------------------

data.table::fwrite(x = salary_files,
                   file = "input/model/salary_city_file.txt",
                   sep = ",",
                   dec = ".",
                   row.names = FALSE,
                   col.names = TRUE)
