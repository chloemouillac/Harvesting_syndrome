# This script is to associate a Raunkiaer biological type to each species of the French flora.


# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/4_Plant-life_history/")

# Import packages
library(dplyr)
library(here)
library(stringr)


#### Import data ####
# Import species list and associated codes :
list_species <- read.csv("raw_data/selected_species.csv") #list of selected species only
# list_species <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv") #all vascular flora

# Import Baseflor :
baseflor <- read.csv("raw_data/BaseFlor/baseflor.csv") %>%
  select(c(NOM_SCIENTIFIQUE, TYPE_BIOLOGIQUE, FORMATION_VEGETALE)) %>%
  na.omit()

baseflor <- baseflor[!grepl(" x ", baseflor$NOM_SCIENTIFIQUE),]
baseflor <- baseflor[!grepl("subsp.", baseflor$NOM_SCIENTIFIQUE),]
baseflor <- baseflor[!grepl("var.", baseflor$NOM_SCIENTIFIQUE),]

baseflor$LB_NOM <- word(baseflor$NOM_SCIENTIFIQUE, 1,2, sep=" ")


# Import CBNmed data :
cbnmed <- read.table("raw_data/CBNmed/export_trait_12122023_142247.txt", header=T) %>%
  subset(rang=="ES" & !(lib_type_biolo1==""), select=c(cd_ref, lib_type_biolo1)) %>%
  unique()



#### Join Baseflor to the species list ####
join_baseflor <- left_join(list_species, baseflor, by = "LB_NOM", 
                           relationship="many-to-many") %>% 
  select(c(CD_REF, LB_NOM, TYPE_BIOLOGIQUE, FORMATION_VEGETALE))





#### Join with the Raunkiaer group given by CBNmed ####
join_baseflor_and_CBNmed <- left_join(join_baseflor,
                                      cbnmed, 
                                      by=join_by(CD_REF==cd_ref)) %>%
  unique()


names(join_baseflor_and_CBNmed) <- c("CD_REF", "LB_NOM", "TYPE_BIO_baseflor", "FORMATION_baseflor", "TYPE_BIO_cbnmed")

join_baseflor_and_CBNmed$species_unique <- ifelse(!duplicated(join_baseflor_and_CBNmed$CD_REF), 1, 0)


#### Export ####
write.csv(join_baseflor_and_CBNmed, "processed_data/Raunkieaer_data_NOT_REVIEWED_ALL_FRANCE_SELECTED.csv", row.names=F)

# this data still needs to be manually reviewed to select the most appropriate Raunkiaer type (depending on sources, these aren't the same, and sometimes, more than one is given for a same species)
