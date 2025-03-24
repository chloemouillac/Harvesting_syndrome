# This script is to download data from GBIF and clean it.

# Set working directory :
directory <- here::here("2_Distribution")
setwd(directory)

# Load packages :
library(rgbif)
library(data.table)
library(dplyr)
library(CoordinateCleaner)
library(countrycode)
library(rnaturalearthdata)
library(dismo)
library(terra)


#### Import data ####
# Import species list :
list_species <- read.csv(here::here("all_sp_corresp_codes.csv")) %>%
  subset(Regroupement!="Algue rouge" & 
           Regroupement!="Algue brune" & 
           Regroupement!="Lichens" &
           CD_REF==CD_NOM, 
         select=c(CD_REF, GBIF, LB_NOM)) %>% unique()


# Import grid raster : 
grid_raster <- rast("raw_data/grid10KM.tif") #raster to be used as grid template
#here France with 10KM grid cell (based on bioclim raster)



#### Get GBIF data ####
# #last done on 25 jul. 14:40
# gbif_download <- occ_download(type="and", pred_in("taxonKey", list_taxons),
#                                                   pred("country", "FR"),
#                                                   pred("hasCoordinate", TRUE),
#                                                   pred("occurrenceStatus","PRESENT"),
#                                                   pred_gte("year", 2000),
#                                                   pred_lte("coordinateUncertaintyInMeters", 10000), #10km
#                                                   pred("hasGeospatialIssue", FALSE),
#                                                   format = "SIMPLE_CSV",
#                                                   user = "chloe.mouillac",
#                                                   pwd = "cSnEq2AYshCWvMV",
#                                                   email ="chloe.mouillac@gmail.com")
#                               
# occ_download_wait(gbif_download) #to check if download is finished
# #wait for occ_download to be finished before executing next command !
# #sometimes lasts quite a few minutes, retry regularly to check !
# 
# # Export the GBIF data as csv :
# GBIF_data <- occ_download_get(gbif_download,"raw_data/GBIF") %>% #to retrieve a download from GBIF to your computer
#   occ_download_import() #this way is quite slow (~30 mins), it goes faster if done by GBIF account in browser

# raw_data/0035791-240626123714530.zip

# Import the GBIF data :
GBIF_data <- fread("raw_data/0035791-240626123714530.csv") %>%
  subset(select=c(species, countryCode, decimalLatitude, decimalLongitude, taxonKey))




#### Clean data ####
# Flag problems :
GBIF_data$countryCode <-  countrycode(GBIF_data$countryCode,
                                    origin =  'iso2c',
                                    destination = 'iso3c')

flags <- clean_coordinates(x = GBIF_data,
                           lon = "decimalLongitude",
                           lat = "decimalLatitude",
                           countries ="countryCode",
                           species = "species",
                           tests = c("urban", "capitals", "centroids", "equal", 
                                     "institutions", "outliers", 
                                     "seas", "zeros"))
  #also better filter observations in France with my own filter ?
  #no duplicates filtering : because it might delete obs of different species in the same location

# Exclude problematic records :
GBIF_data_clean <- GBIF_data[flags$.summary,]

# Join with species list to get CD_REF 
GBIF_data_clean <- GBIF_data_clean %>%
  left_join(list_species, join_by(taxonKey==GBIF)) %>%
  subset(select=c(CD_REF, LB_NOM, decimalLatitude, decimalLongitude)) %>%
  na.omit()




#### Export data ####
fwrite(GBIF_data_clean, "processed_data/GBIF_clean_10km.csv") #691 species (harvested ones)
