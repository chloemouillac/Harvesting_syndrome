# This script is to get the proportion of each red list status for each type of protection / regulation in France.
# Done for wild-harvested and not harvested species.


# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/5_Conservation_status/")

# Import packages :
library(dplyr)
library(reshape2)
library(readr)


#### Import data ####
# Import the status database :
BDC_STATUTS_17 <- read_csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/BDC-Statuts-v17/BDC_STATUTS_17.csv") %>%
  subset(REGNE=="Plantae", select=c(CD_REF, CD_TYPE_STATUT, LB_TYPE_STATUT, REGROUPEMENT_TYPE, CODE_STATUT, LABEL_STATUT, NIVEAU_ADMIN)) %>%
  unique()

# Import list of vascular flora :
list_species <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv")


# Import the list of harvested species for which the data needs to be extracted :
list_harv_species <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv") %>%
  subset(select=CD_REF) %>%
  unique()

list_harv_species$harvested <- "harvested" #add this to identify the harvested plants


#### Get the number of species for each red list status and protection/regulation ####
# Join the status database with TAXREF :
join <- left_join(list_species, BDC_STATUTS_17)

# Filter to get the protections and regulations :
prot_reg <- join[join$CD_TYPE_STATUT %in% 
                   c("REGL", "PR", "PD", "PN"), ] %>%
  subset(NIVEAU_ADMIN != "Subdivision administrative", 
         select=c(CD_REF, LB_NOM, CD_TYPE_STATUT, NIVEAU_ADMIN)) %>% 
  #we keep only the regional and département protections/regulations
  unique()

prot_reg$NIVEAU_ADMIN[prot_reg$NIVEAU_ADMIN=="Ancienne région"] <-"Région"
prot_reg$NIVEAU_ADMIN[prot_reg$CD_TYPE_STATUT=="PN"] <-"État"

prot_reg$CD_TYPE_STATUT[prot_reg$CD_TYPE_STATUT=="REGL" & prot_reg$NIVEAU_ADMIN=="Département"] <- "PD"
prot_reg <- prot_reg[! (prot_reg$CD_TYPE_STATUT=="REGL" & prot_reg$NIVEAU_ADMIN!="Département"),] %>% 
  unique()

# Filter to get the red list national statuses :
LRN <- join[join$CD_TYPE_STATUT == "LRN", ] %>%
  # select(c(CD_REF, CODE_STATUT)) %>%
  select(c(CD_REF, LB_NOM, CODE_STATUT)) %>% 
  unique()

# Clean the red list data : choose one status when there are more than one :
LRN$CODE_STATUT <- as.factor(LRN$CODE_STATUT) %>%
  ordered(levels = c("EX","EW","RE","CR","CR*","EN","VU","NT","LC","DD","NE","NA")) #this ordering will allow to keep the higher statuses when eliminating duplicates
LRN <- arrange(LRN,LRN$CODE_STATUT)
LRN <- LRN[!duplicated(LRN$CD_REF),]


# Join the protections/regulations with the red list statuses
join_prot_reg_LRN <- full_join(prot_reg, LRN) %>%
  unique()

# Join with the harvested species list to identify harvested species :
join_prot_reg_LRN <- left_join(join_prot_reg_LRN, list_harv_species)


# Join with the TaxRef to have the not protected or regulated species :
join_prot_reg_LRN <- full_join(join_prot_reg_LRN, list_species) %>%
  unique()

join_prot_reg_LRN$CODE_STATUT[join_prot_reg_LRN$CODE_STATUT=="CR*"] <- "CR"
join_prot_reg_LRN$CODE_STATUT[is.na(join_prot_reg_LRN$CODE_STATUT)] <- "NE"


# Get the number of each red list status for each group of protection / regulation
summary <- join_prot_reg_LRN %>%
  group_by(NIVEAU_ADMIN, CODE_STATUT, harvested) %>%
  summarise(n = n())
summary$NIVEAU_ADMIN[is.na(summary$NIVEAU_ADMIN)] <- "no protection or regulation"
summary$harvested[is.na(summary$harvested)] <- "not harvested"



#### Get the proportion of species for each red list status and protection/regulation ####
# Transform numbers into percentages :

total_harvested_species <- sum(summary$n[summary$harvested=="harvested"])
total_not_harvested_species <- sum(summary$n[summary$harvested=="not harvested"])

# Get proportions, instead of numbers : 
summary$percentage <- NA


for (i in unique(summary$NIVEAU_ADMIN)) {
  
  #harvested :
  select_harv <- summary$NIVEAU_ADMIN==i & 
    summary$harvested=="harvested"
  
  summary$percentage[select_harv] <- 100*
    summary$n[select_harv]/sum(summary$n[select_harv])
  
  #not harvested :
  select_not_harv <- summary$NIVEAU_ADMIN==i & 
    summary$harvested=="not harvested"
  
  summary$percentage[select_not_harv] <- 100*
    summary$n[select_not_harv]/sum(summary$n[select_not_harv])
}


summary$percentage <- round(summary$percentage, digits=2)


# Get the proportion of species assessed by the IUCN (harvested species and entire vascular flora)
prop_IUCN_ALL <- 100* nrow(unique(subset(join_prot_reg_LRN, !CODE_STATUT %in% c("NE", "DD"), select=c(CD_REF)))) /
  length(unique(join_prot_reg_LRN$CD_REF))
  # 75% of all the flora is assessed

prop_IUCN_HARV <- 100* nrow(unique(subset(join_prot_reg_LRN, !CODE_STATUT %in% c("NE", "DD")
                                   & harvested == "harvested", select=CD_REF))) /
  nrow(unique(subset(join_prot_reg_LRN, harvested == "harvested", select=CD_REF))) 
  # 93.4% of all the flora is assessed


# Proportion of each IUCN status :
summar_IUCN_ALL <- join_prot_reg_LRN %>%
  subset(select=c(CD_REF, CODE_STATUT)) %>%
  unique() %>%
  group_by(CODE_STATUT) %>%
  summarise(n=n())

summar_IUCN_ALL$prop <- 100* summar_IUCN_ALL$n / sum(summar_IUCN_ALL$n)


summar_IUCN_HARV <- join_prot_reg_LRN %>%
  subset(harvested=="harvested", select=c(CD_REF, CODE_STATUT)) %>%
  unique() %>%
  group_by(CODE_STATUT) %>%
  summarise(n=n())

summar_IUCN_HARV$prop <- 100* summar_IUCN_HARV$n / sum(summar_IUCN_HARV$n)



# Proportion of species that are protected or regulated :

join_prot_reg_LRN$protreg <- NA
join_prot_reg_LRN$protreg[is.na(join_prot_reg_LRN$NIVEAU_ADMIN)] <- "no"
join_prot_reg_LRN$protreg[!is.na(join_prot_reg_LRN$NIVEAU_ADMIN)] <- "yes"

summar_protreg_ALL <- join_prot_reg_LRN %>%
  subset(select=c(CD_REF, protreg)) %>%
  unique() %>%
  group_by(protreg) %>%
  summarise(n=n())

summar_protreg_ALL$prop <- 100* summar_protreg_ALL$n / sum(summar_protreg_ALL$n)


summar_protreg_HARV <- join_prot_reg_LRN %>%
  subset(harvested=="harvested", select=c(CD_REF, protreg)) %>%
  unique() %>%
  group_by(protreg) %>%
  summarise(n=n())

summar_protreg_HARV$prop <- 100* summar_protreg_HARV$n / sum(summar_protreg_HARV$n)




#### Export ####
write.csv(summary, "processed_data/prop_status_prot-reg.csv", row.names=F)

