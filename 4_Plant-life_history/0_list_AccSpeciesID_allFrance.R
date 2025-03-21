# This script is to get a list of AccSpeciesIds of the plant we will be working with
# to then extract the associated trait data from TRY

# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/4_Plant-life_history/")

# Import packages :
library(dplyr)
library(reshape2)
library(stringr)


#### Import data ####
# List of french species from vascular flora :
vascular_list <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv" )

# TRY species and IDs :
list_TRY_ID <- read.delim("raw_data/TRY/TryAccSpecies.txt") %>%
  select(c(AccSpeciesName, AccSpeciesID))


#### Join data to get available AccSpeciesIDs ####
join <- inner_join(list_TRY_ID, vascular_list, by=join_by("AccSpeciesName"=="LB_NOM")) #about 1100 species missing

# Specifically add the Acc_Species_IDs for harvested plants :
WHP <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv") %>%
  subset(!is.na(AccSpeciesID), select=c(CD_REF, AccSpeciesID)) %>%
  unique() %>%
  left_join(list_TRY_ID) %>% # to add AccSpeciesName
  inner_join(vascular_list, by="CD_REF") %>%
  select(CD_REF, AccSpeciesID, AccSpeciesName, FAMILLE)

join_final <- rbind(join, WHP) %>%
  unique()


#### Check which plant families are available in TRY ####
fams <- data.frame(FAMILLE=unique(vascular_list$FAMILLE))

for (i in fams$FAMILLE) {
  if (i %in% join_final$FAMILLE) {
    fams$inTRY <- "YES"
  } else {
    fams$inTRY <- "NO"
  }
}

# all of the families are represented in TRY !!


# Note : above 1000 species, TRY exports the entire DB


#### Export list if AccSpeciesID ####
write.csv(join_final, "raw_data/list_species_TRY_French_Flora.csv", row.names=F)
