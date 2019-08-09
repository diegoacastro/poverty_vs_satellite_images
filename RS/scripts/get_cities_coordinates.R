### Created by: Diego Afonso de Castro
### Date: 10/07/2019
### Objective: Generate file with cities coordinates

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(raster)
library(dplyr)


# Read shapefile ----------------------------------------------------------
shape_br <- shapefile("input/coordenadas/BR_Localidades_2010_v1.shp")


# Process data ------------------------------------------------------------
coordinates_br <- shape_br@data %>% 
  filter(NM_CATEGOR == "CIDADE") %>% 
  select(CD_GEOCODM, NM_MUNICIP, NM_UF, LONG, LAT)


# Save data ---------------------------------------------------------------
readr::write_csv(x = coordinates_br,
                 path = "input/model/coordinates_br.txt",
                 append = FALSE,
                 col_names = TRUE)
