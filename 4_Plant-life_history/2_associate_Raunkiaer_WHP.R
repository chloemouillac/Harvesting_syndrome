# This script is to associate a Raunkiaer biological type to each species of WHP.


# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/4_Plant-life_history/")

# Import packages
library(dplyr)


#### Import data ####
# Import species list and associated codes
list_species <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/all_sp_corresp_codes.csv")

# Import Baseflor
baseflor <- read.csv("raw_data/BaseFlor/baseflor.csv") %>%
  select(c(NOM_SCIENTIFIQUE, TYPE_BIOLOGIQUE, FORMATION_VEGETALE))

baseflor <- baseflor[!grepl("subsp.", baseflor$NOM_SCIENTIFIQUE),] # remove subspecies

baseflor$LB_NOM <- stringr::word(baseflor$NOM_SCIENTIFIQUE, 1,2, sep=" ")

#Import CBNmed data
cbnmed <- read.table("raw_data/CBNmed/export_trait_12122023_142247.txt", header=T) %>%
  select(c(cd_ref, lib_type_biolo1))
cbnmed <- cbnmed[!cbnmed$lib_type_biolo1=="",]



#### Join Baseflor to the species list ####
join_baseflor <- left_join(list_species, baseflor, by = "LB_NOM", 
                  relationship="many-to-many") %>% 
  select(c(CD_REF, NOM_VALIDE, TYPE_BIOLOGIQUE, FORMATION_VEGETALE))

# some species are not detailed in Baseflor, so I export the list of species missing a Raunkiaer type, and complete it manually, then reintegrate it
# not_associated <- total[is.na(total$TYPE_BIOLOGIQUE),] 
# # write.csv(not_associated, "raunkieaer_manquant_a_remplir.csv", row.names=F)

#add the missing data :
not_associated <- read.csv("raw_data/sp_raunkieaer_not_assoc.csv")
join_baseflor <- rbind(join_baseflor[!is.na(join_baseflor$FORMATION_VEGETALE),], not_associated) %>%
  unique()



#### Join with the Raunkiaer group given by CBNmed ####

join_baseflor_and_CBNmed <- left_join(join_baseflor,
                                      cbnmed, 
                                      by=join_by(CD_REF==cd_ref)) %>%
  unique()


names(join_baseflor_and_CBNmed) <- c("CD_REF", "NOM_VALIDE", "TYPE_BIO_baseflor", "FORMATION_baseflor", "TYPE_BIO_cbnmed")


#### Export ####
write.csv(join_baseflor_and_CBNmed, "processed_data/Raunkieaer_data_NOT_REVIEWED_WHP.csv", row.names=F)

# this data still needs to be manually reviewed to select the most appropriate Raunkiaer type (depending on sources, these aren't the same, and sometimes, more than one is given for a same species)
