# This script is to format the data downloaded from the TRY Plant Trait Database in order to calculate the CSR for each species, using the StrateFy method developed by Pierce et al. in 2016.
# Here, a mean value for each trait is done for each species, removing any outliers.


# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/4_Plant-life_history/")

# Import packages :
library(dplyr)
library(here)
library(reshape2)
library(rio)
library(stringr)


#### Import data ####
# Import the list of vascular flora :
vascular_list <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv") %>%
  select(LB_NOM, CD_REF, FAMILLE)

# Import the list of harvested species :
list_harv_species <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv") %>%
  subset(Regroupement %in% c("Angiospermes", "Gymnospermes", "Fougères"), select=CD_REF) %>%
  unique()
list_harv_species$harvested <- 1

# Import AccSpeciesID list of the French flora :
list_TRY_ID <- read.csv("raw_data/list_species_TRY_French_Flora.csv")


# Import the TRY trait data and filter it :
TRY_data <- readr::read_delim("raw_data/TRY/36998_01112024024943/36998.txt", delim = "\t", escape_double = FALSE, trim_ws = TRUE) %>%
  subset(!is.na(TraitID),
         select=c("AccSpeciesID", "AccSpeciesName", "TraitID", "StdValue", "UnitName"))

TRY_data <- TRY_data[TRY_data$AccSpeciesID %in% list_TRY_ID$AccSpeciesID,] # subset the TRY data to vascular flora
TRY_data <- left_join(TRY_data, list_TRY_ID, by=c("AccSpeciesID", "AccSpeciesName")) %>% #to get CD_REF and family
  left_join(vascular_list) %>% # to get LB_NOM
  na.omit()
  
# Import DivGrass trait data and filter it :
divgrass <- import("raw_data/DivGrass/20200602_Divgrass.rds") %>%
  subset(!is.na(TAXREF_SHORT))

vascular_list$LB_NOM_upper <- toupper(vascular_list$LB_NOM) # format names to match

divgrass_sub <- divgrass[divgrass$TAXREF_SHORT %in% vascular_list$LB_NOM_upper,] %>% # keep only species from the list
select(c(TAXREF_SHORT, LA, SLA, LDMC)) # keep only selected traits for CSR calculation

divgrass_sub <- right_join(vascular_list, divgrass_sub, join_by(LB_NOM_upper==TAXREF_SHORT)) %>%
  subset(select=-LB_NOM_upper) %>%
  unique() # remove any duplicates

DivGrass_data <- divgrass_sub
DivGrass_data$LDMC <- DivGrass_data$LDMC /1000 # original unit was wrong



#### Join TRY and DivGrass data ####
DivGrass_data <- melt(DivGrass_data, id=c("LB_NOM","CD_REF", "FAMILLE"), na.rm=T)
names(DivGrass_data) <- c("LB_NOM", "CD_REF", "FAMILLE", "TraitName", "StdValue")

DivGrass_data$TraitID[DivGrass_data$TraitName=="LDMC"] <- 47
DivGrass_data$TraitID[DivGrass_data$TraitName=="LA"] <- 3112
DivGrass_data$TraitID[DivGrass_data$TraitName=="SLA"] <- 3117

DivGrass_data$StdValue <- as.numeric(DivGrass_data$StdValue)

rawdata <- full_join(TRY_data, DivGrass_data) %>%
  select(c("LB_NOM", "CD_REF", "FAMILLE", "TraitID", "StdValue"))


#### Remove outliers ####

# Create an empty dataframe
rawdata_no_outlier <- data.frame(matrix(ncol=6, nrow=0))
colnames(rawdata_no_outlier) <- c("LB_NOM", "CD_REF", "FAMILLE", "TraitID", "StdValue", "UnitName")

# Calculate interquartile range and remove outliers
for (i in unique(rawdata$CD_REF)) {
  for (j in unique(rawdata$TraitID)) {
    data <- rawdata[rawdata$CD_REF==i
                    & rawdata$TraitID==j,]
    
    if (length(data$StdValue)>2) { # remove outliers ONLY when 3 values or more are available
      quartiles <- quantile(data$StdValue, 
                            probs=c(.25, .75), na.rm = T)
      IQR <- IQR(data$StdValue, na.rm=T)
      
      Lower <- quartiles[1] - 1.5*IQR
      Upper <- quartiles[2] + 1.5*IQR 
      
      data_no_outlier <- subset(data, data$StdValue > Lower & data$StdValue < Upper)
      
    } else {
      data_no_outlier <- data
    }
    
    rawdata_no_outlier <- rbind(rawdata_no_outlier, data_no_outlier)
  }}


write.csv(rawdata_no_outlier, "raw_data/raw_CSR_trait_data_all_France.csv", row.names = F)


#### Mean data for each trait and species ####

## LDMC : Leaf Dry Mass per Leaf Fresh Mass
data_LDMC <- subset(rawdata_no_outlier, TraitID==47, 
                    select=-TraitID) %>%
  group_by(CD_REF, LB_NOM, FAMILLE) %>%
  summarise(mean_LDMC=mean(StdValue))



## SLA : Specific Leaf Area
# 3115 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole excluded.
# 3116 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): petiole included.
# 3117 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA): undefined if petiole is in- or exclu.
# 3085 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) of leaf lamina.
# 3086 Leaf area per leaf dry mass (specific leaf area, SLA or 1/LMA) petiole, rhachis and midrib excluded.

# Mean 3115, 3116 and 3117 (petiole probably not a big difference)
data_SLA <- subset(rawdata_no_outlier, TraitID %in% c(3115, 3116, 3117), 
                   select=-TraitID) %>%
  group_by(CD_REF, LB_NOM, FAMILLE) %>%
  summarise(mean_SLA=mean(StdValue))



## LA : Leaf Area
# 3108 Leaf area (in case of compound leaves: leaf, petiole excluded)
# 3110 Leaf area (in case of compound leaves: leaf, petiole included)
# 3112 Leaf area (in case of compound leaves: leaf, undefined if petiole in- or excluded)

# 3109 Leaf area (in case of compound leaves: leaflet, petiole excluded)
# 3111 Leaf area (in case of compound leaves: leaflet, petiole included)
# 3113 Leaf area (in case of compound leaves: leaflet, undefined if petiole is in- or excluded)

# 3114 Leaf area (in case of compound leaves undefined if leaf or leaflet, undefined if petiole is in- or e.

# Mean the trait data, different traits depending on if the leaf is compound or not
compoundness <- read.csv("raw_data/simple_or_compound_ALL_species.csv")

data_LA_not_compound <- subset(rawdata_no_outlier,
                               LB_NOM %in% compoundness$LB_NOM[compoundness$Leaf=="simple"] &
                                 TraitID %in% c(3108, 3110, 3112, 3109, 3111, 3113, 3114),
                               select=-TraitID) %>%
  group_by(CD_REF, LB_NOM, FAMILLE) %>%
  summarise(mean_LA=mean(StdValue)) 

data_LA_compound <- subset(rawdata_no_outlier,
                           LB_NOM %in% compoundness$LB_NOM[compoundness$Leaf=="compound"] &
                             TraitID %in% c(3108, 3110, 3112), 
                           select=-TraitID) %>%
  group_by(CD_REF, LB_NOM, FAMILLE) %>%
  summarise(mean_LA=mean(StdValue))

data_LA <- rbind(data_LA_not_compound, data_LA_compound)



#### Bind data for LA, LDMC and SLA into a final tab ####
full_data <- full_join(data_LA, data_LDMC) %>%
  full_join(data_SLA) %>%
  full_join(vascular_list) %>% # full_join adds empty lines for missing species
  select(-LB_NOM_upper) %>%
  full_join(list_harv_species) %>%
  subset(!is.na(LB_NOM)) %>% # issue with Taraxacum dens-leonis CD_REF=125568, but normal, just remove it
  unique() 

full_data$harvested[is.na(full_data$harvested)] <- 0

full_data$mean_LDMC <- full_data$mean_LDMC *100 # necessary for calculations



#### Select species to represent France's vascular plant CSR ####
# Look at the number of species for which we can calculate CSR
# and see which species have enough data for the CSR :
for (i in 1:length(full_data$CD_REF)) {
  if (is.na(full_data$mean_LA[i]) | 
      is.na(full_data$mean_LDMC[i]) |
      is.na(full_data$mean_SLA[i])) {
    full_data$CSR_possible[i] <- 0
  } else {
    full_data$CSR_possible[i] <- 1
  }
}
length(full_data$harvested[full_data$harvested==1 & full_data$CSR_possible==1])

# Summarise :
summar <- full_data %>% 
  group_by(FAMILLE) %>%
  summarise(not_harvested=abs(n()-sum(harvested)), harvested=sum(harvested), 
            CSR_not_possible=abs(n()-sum(CSR_possible)), CSR_possible=sum(CSR_possible),
            )


summar$percentage_possible <- 100*summar$CSR_possible/(summar$CSR_possible + summar$CSR_not_possible)
summar$percentage_species <- 100*(summar$harvested + summar$not_harvested)/sum(summar$harvested + summar$not_harvested)

# Number of species I want per family :
summar$number_species <- ceiling(0.1*(summar$CSR_possible + summar$CSR_not_possible)) # we ideally want to have 10% of each family
summar$diff <- summar$CSR_possible-summar$number_species # shows for which families I can't complete the 10% goal



# Import the list of selected species : 
# The goal was to randomly choose 10% of the species in each family
# when there weren't enough species, we selected the maximum number available
selected <- read.csv("raw_data/selected_species.csv")



#### Export data ####

# Export the data for the total French flora :
full_data_no_NA <- na.omit(full_data) # first remove nas from species data :

full_data_France <- left_join(selected, full_data_no_NA) %>% # join with the list of selected species to filter data
  select(LB_NOM, CD_REF, mean_LA, mean_LDMC, mean_SLA)

write.csv(full_data_France, "processed_data/CSR_trait_data_SELECTED_ALL_FRANCE.csv", row.names = F)


# Export the data for wild harvested flora :
full_data_WHP <- left_join(list_harv_species, full_data_no_NA) %>% # join with the list of harvested species
  select(LB_NOM, CD_REF, mean_LA, mean_LDMC, mean_SLA) %>%
  na.omit() # remove species for which we couldn't calculate CSR

write.csv(full_data_WHP, "processed_data/CSR_trait_data_WHP.csv", row.names = F)


# this data must then be copy-pasted in the columns of the StrateFy.ods file, in order to get the CSR calculations
# Note : the Stratefy file uses commas as decimal separators

