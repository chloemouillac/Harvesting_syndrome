# This script is to format the data associated to each family (tip data) for plotting later.

# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/1_Phylogeny/")

# Load packages :
library(dplyr)
library(reshape2)


#### Import data ####
# Import list of french vascular flora:
vascular_list <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv" )

# Import list of wild harvested species :
harvested_list <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv") %>%
  subset(Regroupement %in% c("Angiospermes", "Fougères", "Gymnospermes"), 
         select=CD_REF) %>%
  unique()
harvested_list$harvested <- "harvested"


#### Full_join data ####
join <- full_join(vascular_list, harvested_list)

join$harvested[is.na(join$harvested)] <- "not_harvested" # now we have a column labeling each species as "harvested" or "not_harvested"


#### Calculate number of harvested/not harvested species per family ####
summar_join <- join %>%
  group_by(FAMILLE, harvested) %>%
  summarise(number_species = n()) %>%
  na.omit() %>%
  dcast(FAMILLE ~ harvested) # to have 3 columns : family, number of harvested species within that family, and number of not harvested

summar_join$harvested[is.na(summar_join$harvested)] <- 0 # change NAs to 0
summar_join$not_harvested[is.na(summar_join$not_harvested)] <- 0


#### Calculate percentage of harvested species per family ####
summar_join$percentage_harvested <- 100 * summar_join$harvested / (summar_join$harvested + summar_join$not_harvested)
summar_join$percentage_harvested <- round(summar_join$percentage_harvested, 1) # round percentage


#### Calculate total number of species per family ####
summar_join$total_sp <- summar_join$harvested + summar_join$not_harvested


summar_join <- subset(summar_join, select=-not_harvested) # remove the not_harvested column
names(summar_join)[2] <- "number_harvested" # change the name of column 2



#### Export ####
write.csv(summar_join, "processed_data/tip_data_for_tree.csv", row.names=F)
