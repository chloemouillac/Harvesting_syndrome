# This script is to marge GBIF and OpenObs data into one database.

# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/2_Distribution/")

# Load packages :
library(data.table)
library(dplyr)
library(here)
library(terra)



#### Import data ####
# Import presence data :
OpenObs_data <- fread("processed_data/OpenObs_10km.csv") %>%
  subset(select=c(cdRef, latitude, longitude)) 

names(OpenObs_data) <- c("CD_REF", "decimalLatitude", "decimalLongitude")


GBIF_data <- fread("processed_data/GBIF_clean_10km.csv") 


# Import grid raster for reducing data to 1 obs/10km²
grid20 <- rast("raw_data/grid20KM.tif")


#### Bind GBIF and OpenObs data ####

sp_data <- rbind(subset(GBIF_data, select=-LB_NOM),OpenObs_data) # number of species = 9780


fwrite(sp_data, "processed_data/OpenObs+GBIF_10km_NOT_REDUCED.csv")



#### Reduce data (1/10km² cell) ####
reduce.pres.data <- function(data, grid_raster) {
  
  xy <- data.frame(decimalLongitude = data$decimalLongitude, decimalLatitude = data$decimalLatitude)
  
  filtered_xy <- dismo::gridSample(xy, 
                                   rast(grid_raster), n=1) #selects 1 point per grid cell, input grid needs to be a raster ! works only with a 2 column dataframe in entry (xy) or a SpatialPoints object
  
  colnames(filtered_xy) <- c("decimalLongitude","decimalLatitude") #added in the case of no data
  
  d <- data.frame(filtered_xy)
  
  if (length(d$decimalLongitude)<1) { #sometimes there are no obs...
    d_reduced <- data.frame(matrix(nrow=0, ncol=2))
    colnames(d_reduced) <- c("decimalLongitude","decimalLatitude")
  } else {
    d_reduced <- inner_join(data, filtered_xy, by=c("decimalLongitude","decimalLatitude"))%>%
      distinct(decimalLongitude,decimalLatitude, .keep_all= TRUE)
  }
  return (d_reduced)
}

# reduce data for each species
sp_data_reduced20 <- sp_data[0,] 

for (s in unique(sp_data$CD_REF)) {
  data <- sp_data[CD_REF==s,]

  sp_data_reduced20 <- rbind(sp_data_reduced20, 
                           reduce.pres.data(data, grid20), fill=TRUE)
  
}


#### Export reduced data ####
fwrite(sp_data_reduced20, "processed_data/OpenObs+GBIF_REDUCED_20km.csv")
