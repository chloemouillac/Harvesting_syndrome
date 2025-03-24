# This script is to establish the list of species needed to build the phylogenetic tree.

# Set working directory :
directory <- here::here("1_Phylogeny")
setwd(directory)

# Import packages :
library(dplyr)
library(reshape2)
library(stringr)
# If V.PhyloMaker2 isn't installed run : devtools::install_github("jinyizju/V.PhyloMaker2")
library(V.PhyloMaker2)


#### Import data ####
# Import list of french vascular flora :
vascular_list <- read.csv(here::here("list_vascular_v17.csv"))


#### Create list of species to submit to Phylomaker ####
# this must be a dataframe with 3 columns, in this order : species, genus, family
list <- vascular_list
list$genus <- str_extract(list$LB_NOM, "\\b[A-Z][a-z]+\\b") # extracts the first word starting with a capital (here the genus)
list$species <- ifelse(grepl(" x ", list$LB_NOM), 
                       word(list$LB_NOM, 1,3), # extracts the first 3 words (here the full species name in the case of a hybrid)
                       word(list$LB_NOM, 1,2)) # extracts the first 2 words

list <- list %>%
  select(species, genus, FAMILLE)
names(list) <- c("species", "genus", "family")



##### Check if there are any correspondence issues by creating a tree #####
# and apply modifications if needed
tree <- phylo.maker(sp.list = list, tree = GBOTB.extended.TPL, nodes = nodes.info.1.TPL, scenarios = "S3")

# Output :
# [1] "Taxonomic classification not consistent between sp.list and tree."
# genus family_in_sp.list   family_in_tree
# 15          Adoxa       Viburnaceae        Adoxaceae
# 98   Arceuthobium         Viscaceae      Santalaceae
# 119        Asarum         Asaraceae Aristolochiaceae
# 514  Heliotropium   Heliotropiaceae     Boraginaceae
# 812      Phacelia   Hydrophyllaceae     Boraginaceae
# 935      Sambucus       Viburnaceae        Adoxaceae
# 1120     Viburnum       Viburnaceae        Adoxaceae
# 1125       Viscum         Viscaceae      Santalaceae
# 1131     Wigandia          Namaceae     Boraginaceae
# [1] "Note: 7 taxa fail to be binded to the tree,"
# [1] "Thesium_alpinum"     "Thesium_corsalpinum" "Thesium_humifusum"   "Thesium_humile"      "Thesium_kyrnosum"   
# [6] "Thesium_linophyllon" "Thesium_pyrenaicum"

# No big correspondence issue here, no changes needed.


#### Export ####
write.csv(list, "processed_data/list_for_phylomaker.csv", row.names=F)
