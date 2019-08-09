### Created by: Diego Afonso de Castro
### Date: 08/08/2019
### Objective: get 100 nearest points to the city coordinates inside the 
###            city boundaries for whole Brazil.

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------

library(raster)
library(data.table)
library(sp)


# Read Shape File ---------------------------------------------------------

br_shape <- shapefile("input/shapes/BR/BR_cities/BRMUE250GC_SIR.shp")


# read nighttime lights data ----------------------------------------------

pts <- fread(file = "input/model/nightlights_df.txt")
pts[, rn:=seq(.N)]
pts <- pts[, rn := as.character(rn)]


# keep only data inside the state box (limits)
pts <- pts[lat > br_shape@bbox[2,1] & lat < br_shape@bbox[2,2] 
           & lon > br_shape@bbox[1,1] & lon < br_shape@bbox[1,2]]

names_of_rows <- pts$rn

pts <- as.matrix(pts[, .(lon, lat)])

# key to join to the nightlights
dimnames(pts)[[1]] = names_of_rows

# Create spatial points object
pts <- SpatialPoints(pts)
pts@proj4string <- br_shape@proj4string

# Get points inside the cities boundaries
coords_to_city <- over(pts, br_shape)

# Convert row names into column
setDT(coords_to_city, keep.rownames = TRUE)[]
#coords_to_city <- coords_to_city[!is.na(CD_GEOCODM)]


# Load complete nightlights data set --------------------------------------

nl <- fread(file = "input/model/nightlights_df.txt")
nl[, rn1:=seq(.N)]


# Process the same way as pts ---------------------------------------------

# keep only data inside the state box (limits)
nl <- nl[lat > br_shape@bbox[2,1] & lat < br_shape@bbox[2,2] 
         & lon > br_shape@bbox[1,1] & lon < br_shape@bbox[1,2]]

# Clear env
rm(br_shape, pts, names_of_rows)
gc()


# Join coords_to_city and nl ----------------------------------------------

# as the order of the elements is the same, we only need to copy city_code col
nl <- cbind(nl, coords_to_city)

# double check if all row indices are the same (if 0, same)
sum(nl$rn1 != nl$rn)


# Keep only useful columns ------------------------------------------------

nl <- nl[, .(lon, lat, radiance, row_raster, col_raster, CD_GEOCMU)]
setnames(nl, "CD_GEOCMU", "city_code")


# keep only night light values inside Brazil ------------------------------

nl <- nl[!is.na(city_code)]

# Clean envi
rm(coords_to_city)
gc()


# Load city's centers coordinates -----------------------------------------

br_coords <- read.csv("input/model/coordinates_br.txt", 
                      encoding = "UTF-8",
                      colClasses = c(rep("character", 3), rep("numeric", 2))) %>% 
  select(city_code = CD_GEOCODM, lon_city = LONG, lat_city = LAT) %>% 
  as.data.table()


# Get 100 nearest points --------------------------------------------------

setkey(nl, city_code)
setkey(br_coords, city_code)

# Joining data tables
nl <- br_coords[nl]

# Drop cities that are not in the br_coords table
nl <- nl[!is.na(lon_city)]

# Get distances between night light point and cities coordinates
dist.euclidean <- function(lat, lat_city, lon, lon_city) {
  
  sqrt((lat - lat_city)^2 + (lon - lon_city)^2)
  
}

nl <- nl[, dist_to_city := mapply(dist.euclidean, lat, lat_city, lon, lon_city)]


# Keep only the first 100 nearest nightlights per city --------------------

nl <- nl[, rank_dist := frank(dist_to_city), by = city_code]

nl <- nl[rank_dist < 101]

radiance <- nl[, radiance]


# Write data table file ---------------------------------------------------

fwrite(x = as.data.frame(radiance),
       file = "input/model/near_nightlights_GMM.csv")
