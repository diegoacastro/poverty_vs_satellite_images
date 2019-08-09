### Modified by: Diego Afonso de Castro
### Source: https://github.com/Arshita27/Satellite_Imagery_ML/blob/master/scripts/get_coordinates.R
### Date: 05/07/2019
### Objective: get radiance values, latitude, longitude, row index, column index from 
###            night light images

### ------------------------------------------------------------------------ ###


# Libraries ---------------------------------------------------------------
library(raster)
library(data.table)

# Get image data ----------------------------------------------------------

# Read file
classificationfile <- raster("input/nightlight/SVDNB_npp_20160101-20161231_00N060W_vcm-orm-ntl_v10_c201807311200.avg_rade9.tif")

# Convert raster into data frame
nightlights_lat_lon <- raster::as.data.frame(classificationfile, xy = TRUE)

colnames(nightlights_lat_lon) <- c("lon","lat","radiance")

nightlights_lat_lon <- as.data.table(nightlights_lat_lon)

# Data table is arranged first by lat in decreasing order then lon in increasing order
n_rows_print <- as.numeric(nrow(nightlights_lat_lon))

for (i in c(1, 2, 3, n_rows_print-2, n_rows_print-1, n_rows_print)) {

print(rowColFromCell(classificationfile,
               extract(classificationfile,
                       SpatialPoints(cbind(nightlights_lat_lon$lon[i], 
                                           nightlights_lat_lon$lat[i])),
                       cellnumbers=TRUE)[1]))

}

# With this test we can see that, as expected, rows are first ordered and then cols
# Create data table following this pattern
n_rows <- dim(classificationfile)[1]
n_cols <- dim(classificationfile)[2]

dt_row_col <- CJ(1:n_rows, 1:n_cols)

colnames(dt_row_col) <- c("row_raster","col_raster")

head(dt_row_col, 3)
tail(dt_row_col, 3)

# We can see that the row and col indices match
# Join main data.table and row/col indices
dt_row_col <- dt_row_col[, Seq:=seq(.N)]
nightlights_lat_lon <- nightlights_lat_lon[, Seq:=seq(.N)]
nightlights_lat_lon <- nightlights_lat_lon[dt_row_col, on = .(Seq=Seq)]

# Drop key column
nightlights_lat_lon <- nightlights_lat_lon[, Seq := NULL]

# Remove objects not necessary
rm(classificationfile, dt_row_col, n_rows_print, n_rows, n_cols, i)

# Release memory space
gc()


# Process data ------------------------------------------------------------

## Brazil eastern limit longitude: -32.28
## Brazil southern limit latitude: -33.75
## We are using Tile 5 of nightlight which don't cover all Brazil, but it covers
## the states we are working with. So we don't need to worry about northern and
## western limits.

# Filter lon and lat (with some margin)
nightlights_lat_lon <- nightlights_lat_lon[lat > -34.0] 
nightlights_lat_lon <- nightlights_lat_lon[lon < -33.0] 

# Write data table file
fwrite(x = nightlights_lat_lon,
       file = "input/model/nightlights_df.txt",
       sep = ";",
       dec = ".",
       row.names = FALSE,
       col.names = TRUE,
       quote = FALSE,
       append = FALSE)