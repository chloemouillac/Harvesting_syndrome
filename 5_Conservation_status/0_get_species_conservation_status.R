# This script is to extract the conservation statuts, lists of protections and regulations for each species.
# This is done using the "Status database" from the INPN https://inpn.mnhn.fr/telechargement/referentielEspece/bdc-statuts-especes 


# Set working directory :
directory <- here::here("5_Conservation_status")
setwd(directory)

# Import packages :
library(dplyr)
library(readr)
library(reshape2)


#### Import data ####
# Import the status database :
BDC_STATUTS_17 <- read_csv(here::here("BDC-Statuts-v17", "BDC_STATUTS_17.csv"))

# Import the list of species for which the data needs to be extracted :
list_species <- read.csv(here::here("list_vascular_v17.csv"))



#### Filter the database ####
# To keep only the data for the species we want
BDC_filtered <- left_join(list_species, BDC_STATUTS_17, by=c("CD_REF", "LB_NOM", "FAMILLE"), relationship="many-to-many") %>%
  subset(select=c(LB_NOM, CD_REF, CD_TYPE_STATUT, LB_TYPE_STATUT, REGROUPEMENT_TYPE, CODE_STATUT, LB_ADM_TR, NIVEAU_ADMIN, FULL_CITATION, DOC_URL))



#### Extract data from the red lists ####
# Extract from the database the lines concerning the red lists :
BDC_LR <- as.data.frame(BDC_filtered[BDC_filtered$REGROUPEMENT_TYPE=="Liste rouge",])

# Clean the data : choose one status when there are more than one for a same region :
BDC_LR$CODE_STATUT <- as.factor(BDC_LR$CODE_STATUT) %>%
  ordered(levels = c("EX","EW","RE","CR","CR*","EN","VU","NT","LC","DD","NE","NA")) #this ordering will allow to keep the higher statuses when eliminating duplicates
BDC_LR <- arrange(BDC_LR,BDC_LR$CODE_STATUT)
BDC_LR <- BDC_LR[!duplicated(BDC_LR[,c("CD_REF","REGROUPEMENT_TYPE","LB_ADM_TR")]),]


# Regional red lists :
BDC_LRR <- subset(BDC_LR, CD_TYPE_STATUT=="LRR", select=c(CD_REF,CODE_STATUT,LB_ADM_TR)) %>%
  reshape(idvar = "CD_REF", timevar = "LB_ADM_TR", direction = "wide") #make the dataframe wide
names(BDC_LRR)[2:ncol(BDC_LRR)] <- substr(names(BDC_LRR)[2:ncol(BDC_LRR)], 13,100) #change column names


# World, and French red lists :
BDC_LR_monde_et_FR <- full_join(subset(BDC_LR, CD_TYPE_STATUT=="LRM", select=c(CD_REF,CODE_STATUT)),
                                subset(BDC_LR, CD_TYPE_STATUT=="LRN", select=c(CD_REF,CODE_STATUT)),
                                relationship = "many-to-many", by="CD_REF") %>%
  distinct(CD_REF, .keep_all= TRUE) #remove duplicates

colnames(BDC_LR_monde_et_FR)<-c("CD_REF","LRM","LRN") #rename the columns according to the list


# Join world and french red lists with the regional ones
BDC_LR_monde_et_FR <- left_join(list_species, BDC_LR_monde_et_FR, by="CD_REF") %>%
  full_join(BDC_LRR, by="CD_REF")




#### Extract protection data ####
BDC_PR <- as.data.frame(BDC_filtered[BDC_filtered$REGROUPEMENT_TYPE=="Protection",])%>%
  na.omit()

protection <- as.data.frame(table(BDC_PR$CD_REF))
colnames(protection) <- c("CD_REF","nombre_protections")

protection$CD_REF <- as.numeric(as.character(protection$CD_REF)) # to convert from factor to numeric, for the join
protection <- full_join(list_species, protection, by="CD_REF")
protection$nombre_protections[is.na(protection$nombre_protections)] <- 0 # change NAs into 0s for the number of protections



#### Extract regulation data ####
BDC_REGL <- as.data.frame(BDC_filtered[BDC_filtered$CD_TYPE_STATUT=="REGL" | BDC_filtered$CD_TYPE_STATUT=="REGLSO",]) %>%
  na.omit() # Réglementation et réglementation sans objet

reglementation <- as.data.frame(table(BDC_REGL$CD_REF))
colnames(reglementation) <- c("CD_REF","nombre_reglementations")

reglementation$CD_REF <- as.numeric(as.character(reglementation$CD_REF)) #to convert from factor to numeric, for the join
reglementation <- full_join(list_species, reglementation, by="CD_REF")
reglementation$nombre_reglementations[is.na(reglementation$nombre_reglementations)] <- 0 #change NAs into 0s for the number of reglementations



#### Export the data ####
synthese_statuts <- full_join(protection, reglementation, relationship = "many-to-many") %>%
  full_join(BDC_LR_monde_et_FR, relationship = "many-to-many")


write.csv(synthese_statuts,"processed_data/species_conservation_status.csv", row.names=F)
write.csv(BDC_REGL,"processed_data/detail_reglementation.csv", row.names=F)
write.csv(BDC_PR,"processed_data/detail_protection.csv", row.names=F)

