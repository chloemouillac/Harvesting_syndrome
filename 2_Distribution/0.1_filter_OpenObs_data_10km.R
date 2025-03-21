# This script is to filter the data downloaded from OpenObs https://openobs.mnhn.fr/archives 
# All of the data for plants was downloaded, as the file is huge, we read it in chunks, filter the chunks and bind them into a new dataframe
# Only the date, precision and origin of the data is filtered, all species are kept


# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/2_Distribution/")

# Import packages :
library(dplyr)
library(here)
library(reshape2)
library(stringr)
library(readr)
library(data.table)


# Path of the file to read in chunks :
filename <- "raw_data/extractINPN_plantes_21092023/extractINPN_plantes_21092023.csv" #species presence data

# Columns to keep during filtering :
colToKeep <- c("idSINPOccTax", #Names
               "libelleJeuDonnees",
               "fournisseurJeuDonnees", 
               "cdRef", 
               "statutObservation",
               "systemeCoordonnees", 
               "latitude", 
               "longitude", 
               "precisionGeometrieMetres",
               "precisionLocalisation",
               "objetGeoWKT", 
               "dateObservation")

N_colToKeep <- c(1,13,22,34,27,53,50,51,52,54,70,44) #Numbers

# Read the column names of the file to read in chunks
# This is to create an empty dataframe with the right column names
# The filtered data will progressively be appended in it
res <- read_csv(filename, col_select=all_of(colToKeep), n_max=0, show_col_types = FALSE)



#### Read the file in chunks ####
done = FALSE

#the filtered data must contain the following strings :
toMatch <- c("CEN ", 
             "CBN",
             "FCBN",
             "Relevés floristiques du protocole de l'Inventaire National Forestier de l'IGN",
             "ONF - Observations opportunistes et inventaires")

n=1 #initialise

while(!done) { #~70*10e5
  raw <- read_csv(filename, col_names=F, col_select=all_of(N_colToKeep), skip=n, n_max=10e6, show_col_types = FALSE)
  n = n+10e6
  names(raw) <- colToKeep
  
  raw <- raw %>% 
    subset(statutObservation=="Présent" & !
             (libelleJeuDonnees %in% c("Pl@ntNet automatically identified occurrences",
                                      "Observation.org, Nature data from around the World",
                                      "iNaturalist Research-grade Observations",
                                      "Pl@ntNet observations",
                                      "Données d'observations flore du réseau Tela Botanica (hors programmes spécifiques)",
                                      "Observations de BioObs.")))
    
    raw <- raw[grepl(paste(toMatch,collapse="|"), 
                     raw$libelleJeuDonnees),]
    
    res <- rbind(res, raw)
 
} #the loop will finish on it own with an error when it's gone through the file, I haven't yet found a way of setting a condition to not get this error



#### Filter date and precision ####
res <- res[res$dateObservation >= "2000-01-01",] #keep only obs after year 2000
res <- res[res$precisionGeometrieMetres<=10000,] #keep only obs with precision higher than 10km

# remove NAs :
res <- res[!is.na(res$libelleJeuDonnees),]

#### Export data ####
write_csv(res, "processed_data/OpenObs_10km.csv")




# Get the list of data suppliers
data <- read_csv("processed_data/OpenObs_10km.csv") #9777 species
jdd_name <- data.frame(unique(data$libelleJeuDonnees)) %>%
  na.omit()
write_csv(jdd_name, "processed_data/liste_jdd_gardés_OpenObs_10km.csv")

#and manually check which ones to keep
