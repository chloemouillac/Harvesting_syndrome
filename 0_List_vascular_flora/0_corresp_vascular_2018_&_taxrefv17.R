# This script is to update the names of the species in the 2018 French Vascular Red List 
# to correspond to TaxRef v17

# Set working directory :
directory <- here::here("0_List_vascular_flora")
setwd(directory)

# Import packages :
library(reshape2)
library(dplyr)
library(tidyr)
library(readr)


#### Import data ####
# Import TAXREF v17 :
TAXREFv17 <- read.delim(here::here("TAXREF_v17_2024/TAXREFv17.txt")) %>%
  subset((GROUP1_INPN=="Trachéophytes") &
           (REGNE=="Plantae"),
         select=c(CD_REF, CD_NOM, LB_NOM, FAMILLE)) %>%
  unique()


# Import 2018 list of french vascular flora :
vasc_2018 <- read.csv("raw_data/list_vascular_FR_2018.csv") %>%
  subset(Rang=="espece" & Categorie!="NAb", # remove NAb to remove species that have disappeared from France
         select=c(CD_REF, Nom.scientifique)) # 6070 obs with NA, 4982 without, env. 1000 NA


#### Join TAXREF and 2018 list ####
names(vasc_2018) <- c("CD_REF_vasc_2018", "Nom.scientifique_vasc_2018") # change column names to make the distinction between old and new names when joining with TaxRef

join <-  left_join(vasc_2018, TAXREFv17, by=join_by(CD_REF_vasc_2018==CD_NOM)) %>% # CD_NOM to get the correspondence
  subset(select=-c(LB_NOM, FAMILLE)) %>%
  left_join(TAXREFv17[TAXREFv17$CD_REF==TAXREFv17$CD_NOM,], # this keeps only the "valid" species
            by="CD_REF") %>% # join again with TAXREFv17 to get the right names and families corresponding to the latest CD_REFS
subset(select=-CD_NOM) %>%
  unique()


# Add a column "taxonomy" to inform which species have changed :
join$taxonomy <- "unchanged"
join$taxonomy[join$CD_REF != join$CD_REF_vasc_2018 & !is.na(join$CD_REF)] <- "name_changed" # 294 species changed taxonomy
join$taxonomy[is.na(join$CD_REF)] <- "species_removed" # 11 species disappeared
join$taxonomy[grepl(" x ", join$LB_NOM)] <- "species_to_hybrid" # 9 species became hybrids
join$taxonomy[grepl("subsp. ", join$LB_NOM)] <- "species_to_subspecies" # 26 species became subspecies
join$taxonomy[grepl("var. ", join$LB_NOM)] <- "species_to_variety" # 3 species became varieties


#### Export ####
# The table where we can see the correspondence between old and new names :
write.csv(join, "processed_data/corresp_vascular_2018_&_taxrefv17.csv", row.names=F)

# The basic list of French vascular flora which will be used throughout the rest of the analyses :
simple_join <- join %>%
  subset(taxonomy %in% c("unchanged", "name_changed", "species_to_hybrid"), select=c(CD_REF, LB_NOM, FAMILLE)) %>% # keep only species level
  unique()
write.csv(simple_join, here::here("list_vascular_v17.csv"), row.names=F)
