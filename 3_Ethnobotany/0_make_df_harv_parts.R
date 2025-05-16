# This script is to produce a dataframe containing the list of harvested parts
# We want a dataframe with 1 column per plant part, 1 row per species
# and indicate if the part is harvested (1) or not (0) for the corresponding species 
# Input dataframe contains 2 columns : 1 with the species names and another with the list of uses, separated by commas


# Set working directory :
directory <- here::here("3_Ethnobotany")
setwd(directory)

# Import packages :
library(reshape2)
library(dplyr)
library(tidyr)


#### Import data ####
harvested_parts <- read.csv("raw_data/harvested_parts_grouped.csv") # table with list of harvested parts

#### Format the data ####

# Remove double spaces and trim leading/trailing spaces
harvested_parts$Partie_cueillie <- gsub("\\s+", " ", harvested_parts$Partie_cueillie) %>%  # replace multiple spaces with a single space
  trimws()  # Trim leading/trailing spaces


# Reshape table to have 1 column per harvested part :
harvested_parts_sep <- separate_rows(harvested_parts, Partie_cueillie, sep = ", ")

harvested_parts_sep_cast <- dcast(harvested_parts_sep, formula = 
                                   CD_REF_v17 + NOM_VALIDE ~ Partie_cueillie)


names(harvested_parts_sep_cast) <- c("CD_REF", "NOM_VALIDE", "buds", "bark", # transalate
                                     "leaves", "flowers", "fruit", "seed", 
                                     "seedling", "aboveground_part", 
                                     "underground_part", "whole_plant", "sap")

#### Export ####
write.csv(harvested_parts_sep_cast, "processed_data/harvested_detail.csv", row.names=F)
