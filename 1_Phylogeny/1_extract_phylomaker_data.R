# This script is to extract the phylomaker data associated to our list of species
# We need the backbone data of the phylogeny (eg. the skeleton)


# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/1_Phylogeny/")

# Import packages :
library(V.PhyloMaker2)
library(dplyr)

#### Import data ####
# List with ALL of the species in the vascular flora, formated for phylomaker
list_ALL <- read.csv("processed_data/list_for_phylomaker.csv") 


#### Reduced list to 1 species / family ####
# to make a tree ending at the family level
list <- list_ALL %>%
  group_by(family) %>%
  summarise(family=first(family), species=first(species), genus=first(genus)) # keep only 1 species per family (189 families)
list <- list[,c(2,3,1)]


##### Create tree #####
# Tree ending at the family level (when plotting, the end nodes will be renamed with the family names)
tree <- phylo.maker(sp.list = list, tree = GBOTB.extended.TPL, nodes = nodes.info.1.TPL, scenarios = "S3")

# Tree ending at the species level
tree_ALL <- phylo.maker(sp.list = list_ALL, tree = GBOTB.extended.TPL, nodes = nodes.info.1.TPL, scenarios = "S3")


##### Export tree #####
write.tree(tree$scenario.3, "processed_data/phylomaker_tree_singles.tre")
write.tree(tree_ALL$scenario.3, "processed_data/phylomaker_tree_all_species.tre")
