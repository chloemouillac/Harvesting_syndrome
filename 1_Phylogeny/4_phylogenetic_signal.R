# This script is to estimate the phylogenetic inertia of harvested plants within the french flora.

# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/1_Phylogeny/")

# Import packages :
library(dplyr)
library(phytools)   # For calculating phylogenetic signal
library(ape)        # For handling phylogenetic trees
library(caper)      # For comparative phylogenetics
library(tidytree)
library(geiger)



#### Import data ####
# Import the tree :
tree_data <- read.tree("processed_data/phylomaker_tree_all_species.tre") # made with list of french vascular flora


# Import list of uses :
uses_data <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/processed_data/uses_detail_count.csv") 
uses_data$LB_NOM <- ifelse(grepl("x. ", uses_data$NOM_VALIDE), #extract LB_NOM from NOM_VALIDE
                           word(uses_data$NOM_VALIDE, 1,3),
                           word(uses_data$NOM_VALIDE, 1,2))
uses_data <- select(uses_data, -c(NOM_VALIDE, CD_REF))

# Import species list :
# Import list of french vascular flora :
vascular_list <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv") %>%
  select(LB_NOM) %>%
  unique()


#### Join uses and species data ####
# to get species that are not in the uses dataframe
uses_join <- full_join(uses_data,vascular_list, by="LB_NOM")
uses_join[is.na(uses_join)] <- 0
uses_join$harvested <- apply(uses_join[,-7], 1, function(row) ifelse(any(row >= 1), 1, 0)) #uses_join[,-7] to remove the name column


#### Ensure species in the data and tree are matched ####
rownames(uses_join) <-  gsub(" ", "_", uses_join$LB_NOM) # set the species column as row names, and format to match phylomaker
uses_join <- select(uses_join, -LB_NOM) # drop species column for compatibility
uses_join[uses_join>0] <- 1 # transform data to binary
uses_join$LB_NOM <- rownames(uses_join)
matched_data <- geiger::treedata(tree_data, uses_join)

# Message d'avis :
# Dans geiger::treedata(tree_data, uses_join) :
#   The following tips were not found in 'phy' and were dropped from 'data':
# 	Pseudo-fumaria_lutea
# 	Taraxacum_dens-leonis
# 	Thesium_alpinum
# 	Thesium_corsalpinum
# 	Thesium_humifusum
# 	Thesium_humile
# 	Thesium_kyrnosum
# 	Thesium_linophyllon
# 	Thesium_pyrenaicum


#### Fritz and Purvis' D calculation (for binary traits) ####
# Harvested
D_result_harv <- phylo.d(data=as.data.frame(matched_data$data), phy=matched_data$phy, names.col=LB_NOM, binvar=harvested)
print(D_result_harv)

# Medical use
D_result_Med <- phylo.d(data=as.data.frame(matched_data$data), phy=matched_data$phy, names.col=LB_NOM, binvar=Medical.Therapeutic)
print(D_result_Med)

# Food use
D_result_Food <- phylo.d(data=as.data.frame(matched_data$data), phy=matched_data$phy, names.col=LB_NOM, binvar=Food.and.Beverages)
print(D_result_Food)

# Crafts use
D_result_Crafts <- phylo.d(data=as.data.frame(matched_data$data), phy=matched_data$phy, names.col=LB_NOM, binvar=Crafts)
print(D_result_Crafts)

# Ornamental use
D_result_Ornamental <- phylo.d(data=as.data.frame(matched_data$data), phy=matched_data$phy, names.col=LB_NOM, binvar=Ornamental.Plants)
print(D_result_Ornamental)
                           
# Cosmetics use
D_result_Cosmetics <- phylo.d(data=as.data.frame(matched_data$data), phy=matched_data$phy, names.col=LB_NOM, binvar=Cosmetics)
print(D_result_Cosmetics)


#### Create a dataframe with the results ####
total_results <- as.data.frame(matrix(nrow = 6, ncol=4))
names(total_results) <- c("Trait", "Destimate", "Pval1", "Pval0")

total_results$Trait <- c("Harvested", "Medical", "Food", "Crafts", "Ornamental", "Cosmetics")

total_results$Destimate <- c(D_result_harv$DEstimate,
                       D_result_Med$DEstimate,
                       D_result_Food$DEstimate,
                       D_result_Crafts$DEstimate,
                       D_result_Ornamental$DEstimate,
                       D_result_Cosmetics$DEstimate)

total_results$Pval1 <- c(D_result_harv$Pval1,
                                 D_result_Med$Pval1,
                                 D_result_Food$Pval1,
                                 D_result_Crafts$Pval1,
                                 D_result_Ornamental$Pval1,
                                 D_result_Cosmetics$Pval1)

total_results$Pval0 <- c(D_result_harv$Pval0,
                             D_result_Med$Pval0,
                             D_result_Food$Pval0,
                             D_result_Crafts$Pval0,
                             D_result_Ornamental$Pval0,
                             D_result_Cosmetics$Pval0)

#### Export ####
write.csv(total_results, "processed_data/fritz_and_purvis_stats.csv", row.names=F)
