# This script is to calculate the coverage (in %) of each species within each French département.

# Set working directory :
directory <- here::here("2_Distribution")
setwd(directory)

# Load packages :
library(data.table)
library(dplyr)
library(terra)
library(sf)
library(exactextractr)
library(ggrepel)



#### Import data ####
# Import presence data :
sp_data20 <- fread("processed_data/OpenObs+GBIF_REDUCED_20km.csv")


# Import the delimitations of the départements
departements <- st_read("raw_data/departements_FR.shp") %>%  #upload the shapefile containing the boundaries of the departements
  #From : https://www.data.gouv.fr/fr/data/10msets/carte-des-departements-2-1/
  st_transform(4326) #to be sure of the projection system (here WGS84)


# Import grid raster for creating a presence raster of the species
grid20 <- rast("raw_data/grid20KM.tif")



#### Create a presence raster based on the presence data of the species ####
create.pres.raster <- function(grid_raster, data) {
  grid_raster[!is.na(grid_raster)] <- 0 #set background cells to 0
  
  if (length(data$decimalLongitude)>0) {
    xy <- cbind(decimalLongitude = data$decimalLongitude, 
                decimalLatitude = data$decimalLatitude)
    
    sp_raster <- terra::rasterize(xy, grid_raster) #create the presence raster
    sp_raster <- merge(sp_raster, grid_raster)
    sp_raster
    
  } else{
    sp_raster <- grid_raster
  }
}

# initialise empty lists to store the individual raster layers
raster_layers20 <- list()


# create a presence raster for each species
for (s in unique(sp_data20$CD_REF)) {
  data20 <- sp_data20[CD_REF==s,]
 
  raster_layers20 <-  c(raster_layers20,  # 1 layer / species
                        create.pres.raster(grid20, data20))
  
}

# combine all the raster layers into a single raster stack
sp_raster20 <- rast(raster_layers20)

# assign names to the layers based on the CD_REF values
names(sp_raster20) <- unique(sp_data20$CD_REF)




#### Number of observations per grid cell ####
background_20km <- grid20
background_20km[!is.na(background_20km)] <- 0

# Calculate the number of observations per grid cell :
xy <- data.frame(sp_data20$decimalLongitude, sp_data20$decimalLatitude)
counts_obs_20km <- cellFromXY(background_20km, xy) %>%
  table() # might take a few minutes to run

obs_20km <- background_20km
obs_20km[] <- 0
obs_20km[as.numeric(names(counts_obs_20km))] <- matrix(counts_obs_20km)

nb_obs_20km <- obs_20km + background_20km #to get the NA values of the background

plot(nb_obs_20km)

#Export :
writeRaster(nb_obs_20km, "processed_data/obs_20km.tif", overwrite=T)




#### Calculate species relative coverage per département ####
# Use a raster of 1s to calculate total department areas in km²
full_raster <- sp_raster20[[1]] * 0 + 1

# Precompute department areas once
dpt_area_df <- exact_extract(full_raster, departements, "sum", coverage_area = TRUE)/1e6
dpt_area_df <- data.frame(
  dpt_code = departements$code,
  dpt_name = departements$dpt,
  dpt_area = unlist(dpt_area_df))
calc.sp.cover <- function(raster, departements, dpt_area_df){
  
  # Replace NAs in species raster with 0
  raster[is.na(raster)] <- 0
  
  # Calculate species area per department (in km²)
  sp_area_list <- exact_extract(raster, departements, "sum", coverage_area = TRUE)/1e6
  
  # Combine into dataframe
  df <- data.frame(
    dpt_code = departements$code,
    dpt_name = departements$dpt,
    sp_area = unlist(sp_area_list)
  )
  
  # Merge with precomputed department area
  df <- merge(df, dpt_area_df, by = c("dpt_code", "dpt_name"))
  
  # Calculate species relative coverage (%)
  df$sp_relative_area <- ifelse(df$dpt_area > 0,
                                (df$sp_area / df$dpt_area) * 100, 0)
  
  df[df==0] <- NA
  df[is.na(df)] <- 0
  
  df
}


# calculate cover for each species using a loop
cov_df20 <- data.frame(matrix(nrow = 0, ncol=6))
names(cov_df20) <- c("dpt_code", "dpt_name", "dpt_area", "sp_area", "sp_relative_area", "CD_REF")

for (l in 1:length(names(sp_raster20))) {
  raster <- sp_raster20[[l]]
  rel_cov <- calc.sp.cover(raster, departements, dpt_area_df)
  rel_cov$CD_REF <- names(raster)
  cov_df20 <- rbind(cov_df20, rel_cov)
}

cov_df20 <- cov_df20 %>%
  mutate(sp_relative_area=ifelse(sp_relative_area>100, 100, sp_relative_area))

#Export :
fwrite(cov_df20, "processed_data/OpenObs+GBIF_RARITY_20km.csv")



