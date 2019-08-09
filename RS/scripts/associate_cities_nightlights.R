### Created by: Diego Afonso de Castro
### Date: 06/07/2019
### Objective: associate each night light point to a city

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(raster)
library(data.table)
library(sp)


# Read Shape File ---------------------------------------------------------
rs_shape <- shapefile("input/shapes/RS/cities/43MUE250GC_SIR.shp")


# read nighttime lights data ----------------------------------------------
nightlights <- fread(file = "input/model/nightlights_df.txt")


# Process nighttime lights ------------------------------------------------

# keep only data inside the state box (limits)
nightlights <- nightlights[lat > rs_shape@bbox[2,1] & lat < rs_shape@bbox[2,2] 
                           & lon > rs_shape@bbox[1,1] & lon < rs_shape@bbox[1,2]]

# This step will associate night lights coordinates to cities
intersection <- extract(rs_shape, nightlights[, c('lon', 'lat')])
nightlights$city_code <- intersection$CD_GEOCODM 

# keep only night light values inside the state
nightlights <- nightlights[!is.na(city_code)]

# Write data table file
fwrite(x = nightlights,
       file = "input/model/nightlights_per_city.txt",
       sep = ";",
       dec = ".",
       row.names = FALSE,
       col.names = TRUE,
       quote = FALSE,
       append = FALSE)
