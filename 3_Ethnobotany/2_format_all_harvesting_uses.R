# This script is to format and group all of the uses collected from different databases into 1 typology.

# Set working directory :
directory <- here::here("3_Ethnobotany")
setwd(directory)

# Import packages :
library(reshape2)
library(dplyr)
library(here)
library(tidyr)


#reprendre jointures ci-dessous avec les CD_REF

#### Import data ####
# Import all databases for plant uses :
KEW_db <- read.csv("processed_data/uses_Kew.csv") %>%
  select(-binomial_acc_name)
CBNPMP <- read.csv("raw_data/Harvesting_lists/CBNPMP/parties_cueillies_usages_CBNPMP.csv") %>%
  select(c("CD_REF", "NOM_VALIDE", "USAGES"))
PFAF <- read.csv("raw_data/PFAF_scraped_uses_and_parts.csv") %>%
  select(c("CD_REF", "NOM_VALIDE", "Edible_uses", "Medicinal_uses", "Other_uses", "Special_uses"))
Chabert <- read.csv("raw_data/Harvesting_lists/Laurence_Chabert_1999_CBNmed/export_usages_12122023_143225.csv") %>%
  select(-c("cosmetique", "nb_biblio"))

# Import the table with correspondences for each use to my personal typology :
corresp <- read.csv("raw_data/uses_correspondences.csv") %>%
  select(c(USE, SELECTED_TYPOLOGY_ENG))

# Import species list :
list_species <- read.csv("Harvesting_syndrome/WHP_correpondence_table_v17.csv") %>%
  subset(Regroupement %in% c("Angiospermes", "Fougères", "Gymnospermes"), 
         select=c(CD_REF, NOM_VALIDE)) %>%
  unique()



#### KEW database ####
# Format to long dataframe :
numeric_to_presence <- function(column) {
  return(ifelse(column == 0, NA, colname))
}

# Apply the function to convert numeric values back into text :
for (i in 3:12) {
  colname <- names(KEW_db)[i]
  KEW_db[, i] <- numeric_to_presence(KEW_db[, i])
}

KEW_db <- melt(KEW_db, id.vars=c("NOM_VALIDE", "CD_REF")) %>% 
  select(-variable) %>%
  na.omit()

names(KEW_db) <- c("NOM_VALIDE", "CD_REF", "USAGES")

KEW_db$SOURCE <- "KEW"



#### CBNPMP database ####
CBNPMP <- separate_rows(CBNPMP, USAGES, sep = " / ") %>%
  separate_rows(USAGES, sep = "/")

CBNPMP <- CBNPMP[CBNPMP$USAGES != "",]

CBNPMP$SOURCE <- "CBNPMP"


#### Chabert database ####
Chabert <- melt(Chabert, id.vars=c("NOM_VALIDE", "CD_REF")) %>% 
  subset(value=="X", select=-value) %>%
  na.omit()

names(Chabert) <- c("NOM_VALIDE", "CD_REF", "USAGES")

Chabert$SOURCE <- "Chabert"



#### PFAF database ####
PFAF <- separate_rows(PFAF, Edible_uses, sep = ", ") %>%
  separate_rows(Medicinal_uses, sep = ", ") %>%
  separate_rows(Other_uses, sep = ", ") %>%
  separate_rows(Special_uses, sep = ", ") 

PFAF <- melt(PFAF, id.vars=c("NOM_VALIDE", "CD_REF")) %>% 
  select(-variable) %>%
  na.omit() %>%
  unique()

PFAF <- PFAF[PFAF$value != "",]

names(PFAF) <- c("NOM_VALIDE", "CD_REF", "USAGES")

PFAF$SOURCE <- "PFAF"



#### Bind all of the databases together ####
TOTAL <- rbind(KEW_db, CBNPMP, Chabert, PFAF) %>% #667 species covered
  left_join(corresp, join_by(USAGES==USE)) %>%
  inner_join(list_species) %>%
  select(-c(USAGES)) %>%
  na.omit() %>%
  unique()

write.csv(TOTAL, "processed_data/uses_detail_with_sources.csv", row.names=F)

# And make a table with uses in columns, and the number of different sources citing the use :
TOTAL_ct <- dcast(TOTAL, formula = NOM_VALIDE + CD_REF ~ SELECTED_TYPOLOGY_ENG)

write.csv(TOTAL_ct, "processed_data/uses_detail_count.csv", row.names=F)




#### Species missing a use ####
missing <- full_join(TOTAL, list_species, join_by(CD_REF,NOM_VALIDE)) %>%
  subset(is.na(SELECTED_TYPOLOGY_ENG), 
         select=c(CD_REF,NOM_VALIDE))

write.csv(missing, "processed_data/species_missing_uses.csv")
