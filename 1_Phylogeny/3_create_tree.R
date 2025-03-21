# This script is to plot the phylogenetic tree using ggtree.

# Set working directory :
directory <- here::here("1_Phylogeny")
setwd(directory)

# Import packages :
# If ggtree and ggtreeExtra aren't yet installed, run :
# if (!require("BiocManager", quietly = TRUE))
#   install.packages("BiocManager")
# BiocManager::install("ggtree")
# BiocManager::install("ggtreeExtra")
library(ggtree)
library(ggtreeExtra)
library(ggplot2)
library(V.PhyloMaker2)
library(dplyr)
library(here)
library(reshape2)
library(ggstar)
library(ggnewscale)


#### Import data ####
# Import the list of species used to build the tree : 
species_data <- read.csv("processed_data/list_for_phylomaker.csv")
species_data$species <- gsub(" ", "_", species_data$species) # format names to allow data joining 
# with Phylomaker names later

# Import the tree data :
tree_data <- read.tree("processed_data/phylomaker_tree_singles.tre") # made with list of vascular french flora

species_data <- subset(species_data, species %in% tree_data$tip.label) # subset the species data to keep only the species in the tree

tree_data$node.label <- paste0("N", 1:187) # name the nodes of the tree (for locating them)

# Import the tip data :
tip_data <- read.csv("processed_data/tip_data_for_tree.csv")
tip_data$FAMILLE <- gsub(pattern="aceae", replacement=".", tip_data$FAMILLE) # shorten family names for lighter plotting



#### Rename tips as families ####
for (i in(1:length(tree_data$tip.label))) {
  old_name_i <- tree_data$tip.label[i]
  tree_data$tip.label[i] <- species_data$family[species_data$species==old_name_i] %>%
    gsub(pattern="aceae", replacement=".") # to remove the "aceae" at the end of each name for lighter plotting
}


#### Plot tree with ggtree #####

# the 10 most harvested families :
# most_harv <-  c("Lamiaceae", "Ranunculaceae", "Boraginaceae", "Caprifoliaceae", "Ericaceae", "Violaceae", "Pinaceae", "Polygonaceae", "Asparagaceae", "Crassulaceae")
most_harv <-  c("Lami.", "Ranuncul.", "Boragin.", "Caprifoli.", "Eric.", "Viol.", "Pin.", "Polygon.", "Asparag.", "Crassul.")

# A few adjustments before plotting :
tip_data$number_harvested[tip_data$number_harvested==0]<-NA # convert zeros to NA for better plotting
tip_data$log_number_harvested <- log10(tip_data$number_harvested+0.2) # log transformation for better plotting and
# +0.2 to prevent negative values due to log

# Create a dataframe to set the place of the clade labels :
cladeda <- data.frame(nodeid = c(214, 205, 237, 244, 262, 289, 307, 316, 353, 349,358),
                      labs = c("Lamiids", "Campanulids", "Ericales", "Caryophyllales", "Fabids", "Malvids", "Saxifragales", "Monocots", "Gymnosperms", "Magnoliids", "Monilophytes"),
                      h=c(1,0,0.5,0.5,1,1,1,1,0,0,1),
                      o=c(21,4,8,6,3,3,3,5,3,4,35))

# Set the breaks in the colour scale (% of WHP species) :
cuts <- c(0, 5, 10, 20, 50, 100) 


# Plot :
plot_tree <- ggtree(tree_data, layout="fan",
                    branch.length="none", 
                    size=2) +
  geom_tiplab(aes(subset = ! label %in% most_harv), size=5, offset=0.1) +  # when attaching data, the first col must be the label of tips (name of col doesn't matter)  +
  geom_tiplab(aes(subset = label %in% most_harv),  size=10, offset=0.1, fontface=2) +
  
  geom_fruit(
    data=tip_data,
    geom=geom_bar,
    mapping=aes(
      x=log10(number_harvested+0.2), # +0.2 to not have problems with infinite value of log(0) 
      y=FAMILLE,
      fill=cut(percentage_harvested, breaks=cuts)),
    color="white",
    stat="identity",
    orientation="y",
    offset = 0.47,
    pwidth=1,
    grid.params = list(color="grey80", size=0.1),
    axis.params = list(
      axis="xy",
      text.size=10,
      text.angle=270,
      color="black",
      nbreak=4)
  ) +
  
  scale_fill_manual(name="% of WHP species", 
                    values=c('#e0ecf4', '#a6bddb', '#8c6bb1','#810f7c','#2d004b'),
                    labels=c("1-5", "5-10", "10-20", "20-50", "50-100"), na.translate=FALSE) +
  
  new_scale_fill() +
  
  geom_cladelab(
    data = cladeda,
    mapping = aes(node=nodeid, label=labs, colour=labs, fill="white", hjust=h, offset.text=o),
    angle = 0, offset = 45, horizontal=F,
    align = T, fontsize = 22,
    barsize = 10, show.legend=F) +
  
  scale_color_manual(values=c("Lamiids"='#a6cee3',"Campanulids"='#1f78b4',"Ericales"='#b2df8a',"Caryophyllales"='#33a02c',"Fabids"='#fb9a99',"Malvids"='#e31a1c',"Saxifragales"='#fdbf6f',"Monocots"='#ff7f00', "Gymnosperms"='#cab2d6', "Magnoliids"='#b15928',"Monilophytes"='#6a3d9a'))  +
  
  # geom_label2(aes(subset=!isTip, label=node), size=5, color="darkred", alpha=0.5) +
  
  new_scale_fill() +
  
  geom_hilight(data = cladeda,
               mapping=aes(node=nodeid, fill=labs), color="white",
               alpha = .3, show.legend=F) +
  
  scale_fill_manual(values=c("Lamiids"='#a6cee3',"Campanulids"='#1f78b4',"Ericales"='#b2df8a',"Caryophyllales"='#33a02c',"Fabids"='#fb9a99',"Malvids"='#e31a1c',"Saxifragales"='#fdbf6f',"Monocots"='#ff7f00', "Gymnosperms"='#cab2d6', "Magnoliids"='#b15928',"Monilophytes"='#6a3d9a')) +
  guides(fill = guide_legend(override.aes = list(size = 20))) +
  
  theme(legend.text = element_text(size=45), legend.title = element_text(size=50, face="bold"),
        legend.position = c(0.88, 0.44)) +
  
  annotate(geom="text", x=118, y=0.25, label="Number of WHP species (log10)", size=18, fontface =2) +
  annotate(geom="segment", x=68, xend=80, y=0.25, yend=0.25, linewidth=1,
           colour="grey20") 


plot_tree

# Note : there can't be a total nb of sp=0, so there should be no blank circles


#### Export ####
# png(file = "plots/phylo.png", width = 23000, height = 21000, res=400)
# plot_tree
# dev.off()

pdf(file = "plots/phylo.pdf", width = 50, height = 48)
plot_tree
dev.off()

