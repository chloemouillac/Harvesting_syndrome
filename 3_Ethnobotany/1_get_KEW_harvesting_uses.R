# This script is to join my harvested species list with the Kew plant uses database
# And solve any synonym problems


# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/")

# Import packages :
library(reshape2)
library(dplyr)
library(here)


#### Import data ####
# Import Kew database for plant uses :
KEW_db <- read.delim("raw_data/Harvesting_lists/Kew_Useful_Plant_DB/useful_plant_processed_db.txt", comment.char="#")
KEW_db <- KEW_db[,-(13:22)]


# Import species list :
list_species <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv") %>%
  subset(Regroupement %in% c("Angiospermes", "Fougères", "Gymnospermes") &
           KEW_binomial_acc_name != "not_found" & #remove species that don't have a correspondence in the Kew database
           !is.na(KEW_binomial_acc_name), 
         select=c(CD_REF, NOM_VALIDE, KEW_binomial_acc_name)) %>%
  unique() # we end up with 632 species / 692


#### Extract the relevant data from the Kew database #### 
filter <- right_join(KEW_db, list_species, by=join_by(binomial_acc_name==KEW_binomial_acc_name)) %>%
  subset(select=-c(acc_name, binomial_acc_name))

filter <- filter[, c((ncol(filter)-1):ncol(filter), 1:(ncol(filter)-2))]


#### Export ####
write.csv(filter, "processed_data/uses_Kew.csv", row.names=F) #note: 59 species of Lescure list are not specified here
