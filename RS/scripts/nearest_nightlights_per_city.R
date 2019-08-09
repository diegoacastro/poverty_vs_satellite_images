### Created by: Diego Afonso de Castro
### Date: 11/07/2019
### Objective: get 100 nearest points to the city coordinates inside the 
###            city boundaries.

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(raster)


# Load night light x cities data and cities coordinates -------------------
nightlights <- fread("input/model/nightlights_per_city.txt",
                     colClasses = c(rep("numeric", 5), "character"))

rs_coords <- read.csv("input/model/coordinates_br.txt", 
                      encoding = "UTF-8",
                      colClasses = c(rep("character", 3), rep("numeric", 2))) %>% 
  filter(str_detect(CD_GEOCODM, "^43")) %>% 
  select(city_code = CD_GEOCODM, lon_city = LONG, lat_city = LAT) %>% 
  as.data.table()


# Get 100 nearest points --------------------------------------------------
setkey(nightlights, city_code)
setkey(rs_coords, city_code)

# Joining data.tables
nightlights <- rs_coords[nightlights]

# Drop cities that aren't in the rs_coords table
nightlights <- nightlights[!is.na(lon_city)]

# Get distances between night light point and cities coordinates
dist.euclidean <- function(lat, lat_city, lon, lon_city) {
  
  sqrt((lat - lat_city)^2 + (lon - lon_city)^2)
  
}

nightlights <- nightlights[, dist_to_city := mapply(dist.euclidean, 
                                                    lat, lat_city,
                                                    lon, lon_city)]
  
# Filter keeping only the first 100
nightlights <- nightlights[, rank_dist := frank(dist_to_city), by = city_code]
nightlights <- nightlights[rank_dist < 101]


# Save file with the final data table -------------------------------------
fwrite(nightlights, file = "input/model/download_coordinates.txt")

