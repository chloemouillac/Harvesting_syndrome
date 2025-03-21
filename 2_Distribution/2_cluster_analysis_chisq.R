# This script is to do a cluster analysis of the species composition of the French départements.
# The goal is to produce groups based on similar species composition.

# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/2_Distribution/")

# Import packages :
library(dplyr)
library(here)
library(sf)
library(tidyr)
library(terra)
library(vegan)
library(ggplot2)
library(gridExtra)
library(cluster)
library(factoextra)
library(fpc)


#### Import data ####
# Import abundance/rarity data :
# and keep only data for wild harvested plants :
list_harvested <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/WHP_correpondence_table_v17.csv") %>%
  subset(REGNE=="Plantae", select=CD_REF)


abund_data20_WHP <- read.csv("processed_data/OpenObs+GBIF_RARITY_20km.csv") %>%
  subset(CD_REF %in% list_harvested$CD_REF)

abund_data20_ALL <- read.csv("processed_data/OpenObs+GBIF_RARITY_20km.csv")


# Read the delimitations of the French départements :
departements <- read_sf("raw_data/departements_FR.shp") %>%
  st_sf() #convert to sf object



####### Initiate a loop ########
list_abund_data <- list(abund_data20_WHP,abund_data20_ALL)

names(list_abund_data) <- c("20_WHP", "20_ALL")


# Loop :

df_silhouette_eval <- as.data.frame(matrix(nrow=0, ncol=5))
names(df_silhouette_eval) <- c("sil_width", "transformation", "distance", 
                               "n_clusters", "data")

df_eval <- as.data.frame(matrix(nrow=0, ncol=9))
names(df_eval) <- c("transformation", "distance", "n_clusters", "data", 
                    "dunn_index", "silhouette_width", "pearson_gamma", 
                    "within_ss", "entropy_value")




  
for (i in 1:length(list_abund_data)) {  # loop over different datasets
    
    data <- list_abund_data[[i]]
    name <- names(list_abund_data)[i]
    
    
    #### Make long dataframe to wide ####
    # One column per species with the départements as rows
    data <- data %>%
      subset(select=c(dpt_code, CD_REF, sp_relative_area)) %>%
      unique() %>%
      pivot_wider(names_from = CD_REF, values_from = sp_relative_area, values_fill = 0)
    
    
    
    #### Start the clustering ####
    ##No transfo
    abund_data <- as.matrix(data[,-1]) #remove cols with dpt codes
    rownames(abund_data) <- data$dpt_code
    
    # Removing species with zero abundance in all samples
    abund_data_cleaned <- abund_data[, colSums(abund_data) > 0] # actually doesn't change anything to clustering
    
    
    ### Calculate distance matrix
    dist <- vegdist (abund_data_cleaned, method = "chisq")

    
    ### Calculate Ward's algorithm
    clus_ward <- hclust (dist, method = 'ward.D2')
    
    
    
    #### Try different numbers of clusters ####
    list_cluster_results <- list()

    
    for (nclu in 3:10) {
      # "Cut the tree" - to which groups individual samples belong?
      clus_ward_cut <- cutree (clus_ward, k = nclu)
      
      
      # To plot tree :
      # clus_in_dendro <- unique(clus_ward_cut[clus_ward$order]) # make sure to know which box is which cluster!
      # plot(clus_ward, cex = .5) # argument cex reduced the size of the dendrogram leaf labels to make them readable
      # rect.hclust (clus_ward, k = nclu, border = clus_in_dendro)
      # legend ('topleft', legend = paste ('Cluster', 1:nclu), col = 1:nclu, pch = 22, bty = 'n')
      
      
      # Get the list of clusters :
      clusters <- as.data.frame(clus_ward_cut)
      
      clusters$departement <- rownames(clusters)

      list_cluster_results[[nclu]] <- clusters

      
      
      # Export :
      write.csv(clusters, paste0("processed_data/clustering/", nclu, 
                                 "clusters_chisq_no_transfo_",
                                 name, ".csv"), row.names = F)
      

    }
  
    
    
    
    
    #### Plotting #### 
    list_plots <- list()

    
    for (nclu in 3:10) {
      
      clusters <- list_cluster_results[[nclu]]
      
      departements_clus <- full_join(departements, clusters, 
                                     by=join_by("code"=="departement"))
      
      departements_clus$clus_ward_cut <- as.factor(departements_clus$clus_ward_cut)
      
      
      plot_clus <- ggplot() +
        geom_sf(data = departements_clus, 
                aes(fill=clus_ward_cut), colour="white")+
        ggtitle(label="Clustering (chisq, no transformation)", 
                subtitle=paste0("k=", nclu, " - Data= ", name)) +
        scale_fill_manual(name="Cluster", values=c('#117733','#CC6677','#44AA99','#DDCC77','#332288','#88CCEE', '#882255','#AA4499',"black",'#999933')) +
        theme(axis.text.x=element_blank(),
              axis.ticks.x=element_blank(),
              axis.text.y=element_blank(),
              axis.ticks.y=element_blank(),
              panel.background = element_blank())
      
      list_plots <- c(list_plots,list(plot_clus))
      
    }
    
    
    # Export plots :
    png(filename=paste0("processed_data/clustering/plots/clustering_chisq_", 
                        name,"_no_transfo.png"),
        width = 1094, height = 900)
    grid.arrange(grobs = list_plots) # need to explicitly define grobs
    dev.off()
    
 
}   
