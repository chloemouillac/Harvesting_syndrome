# This script is to produce graphs illustrating the distribution of wild-harvested plants.


# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/2_Distribution/")

# Import packages :
library(ggplot2)
library(terra)
library(sf)
library(dplyr)
library(ggpubr)


#### Import data ####
# Import the delimitations of the French départements :
departements <- read_sf("raw_data/departements_FR.shp") %>%
  st_sf() #convert to sf object



#### !!! Heatmap : number of different species / département ####
data_summary_per_sp <- read.csv("processed_data/nb_sp_per_dpt_ALL_20.csv")


data_summary_per_sp_map <- full_join(departements,data_summary_per_sp, by="dpt")

data_summary_per_sp_map$nb_sp <- as.numeric(data_summary_per_sp_map$nb_sp)


plot_sp_tot <- ggplot() +
  geom_sf(data = data_summary_per_sp_map$geometry, aes(fill=data_summary_per_sp_map$nb_sp), color="white", lwd=0.7)+
  scale_fill_distiller(type="seq", palette="Reds", direction=1) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size=21),
        legend.text = element_text(size=22),
        legend.position = c(0.15,0.35),
        legend.key.height = unit(1, "cm")) +
  labs(fill="Number of plant
species")
plot_sp_tot

png(file = "plots/div_sp_tot.png", width = 1317, height = 1040)
plot_sp_tot
dev.off()


#### !!! Heatmap : number of different HARVESTED species / département ####
data_summary_per_sp_HARV <- read.csv("processed_data/nb_sp_per_dpt_HARV_20.csv")

data_summary_per_sp_HARV_map <- full_join(departements,data_summary_per_sp_HARV, by=c("code", "dpt", "region"))

data_summary_per_sp_HARV_map$nb_sp <- as.numeric(data_summary_per_sp_HARV_map$nb_sp)


plot_sp <- ggplot() +
  geom_sf(data = data_summary_per_sp_HARV_map$geometry, aes(fill=data_summary_per_sp_HARV_map$nb_sp), color="white", lwd=0.7)+
  scale_fill_distiller(type="seq", palette="Reds", direction=1, breaks=c(400,500,600)) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size=21),
        legend.text = element_text(size=22),
        legend.position = c(0.15,0.35),
        legend.key.height = unit(1, "cm")) +
  labs(fill="Number of WHP
species")
plot_sp

png(file = "plots/div_sp.png", width = 1317, height = 1040)
plot_sp
dev.off()



#### !!! Heatmap : number of records / département ####

data_summary_per_obs <- read.csv("processed_data/nb_records_per_dpt_ALL_20.csv")
# data_summary_per_obs <- read.csv("processed_data/nb_records_per_dpt_GBIF.csv")

data_summary_per_obs_map <- full_join(departements,data_summary_per_obs, by=c("code", "dpt", "region"))

data_summary_per_obs_map$nb_obs <- as.numeric(data_summary_per_obs_map$nb_obs)


plot_obs_tot <- ggplot() +
  # geom_sf(data = data_summary_per_obs_map$geometry
  #         [data_summary_per_obs_map$code != "PARIS"], 
  #         aes(fill=data_summary_per_obs_map$nb_obs
  #             [data_summary_per_obs_map$code != "PARIS"]),
  #         
          geom_sf(data = data_summary_per_obs_map$geometry, 
                  aes(fill=data_summary_per_obs_map$nb_obs),
          colour="white")+
  scale_fill_distiller(type="seq", palette="Reds", direction=1) +
  ggtitle(label="Number of records for all plants")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20)) +
  labs(fill="Number of records")
plot_obs_tot

# png(file = "plots/nb_records_tot.png", width = 800, height = 600)
# plot_obs_tot
# dev.off()

pdf(file = "plots/nb_records_tot.pdf", width=10, height=7)
plot_obs_tot
dev.off()

#### Heatmap : number of records of HARVESTED species / département ####

data_summary_per_obs_HARV <- read.csv("processed_data/nb_records_per_dpt_HARV_20.csv")

data_summary_per_obs_map_HARV <- full_join(departements,data_summary_per_obs_HARV, by=c("code", "dpt", "region"))

data_summary_per_obs_map_HARV$nb_obs <- as.numeric(data_summary_per_obs_map_HARV$nb_obs)


plot_obs <- ggplot() +
  # geom_sf(data = data_summary_per_obs_map_HARV$geometry
  #         [data_summary_per_obs_map_HARV$code != "PARIS"], 
  #         aes(fill=data_summary_per_obs_map_HARV$nb_obs
  #             [data_summary_per_obs_map_HARV$code != "PARIS"]),
  #         colour="white")+
  # 
  geom_sf(data = data_summary_per_obs_map_HARV$geometry, 
          aes(fill=data_summary_per_obs_map_HARV$nb_obs),
          colour="white")+
  
  scale_fill_distiller(type="seq", palette="Reds", direction=1) +
  ggtitle(label="Number of records of harvested species")+
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.title = element_text(size=22),
        legend.text = element_text(size=20)) +
  labs(fill="Number of records")
plot_obs

png(file = "plots/nb_records.png", width = 800, height = 600)
plot_obs
dev.off()



#### Map resulting from the cluster analysis on ALL plant species ####
clustering_ALL <- read.csv("processed_data/clustering/7clusters_chisq_no_transfo_ALL_SPECIES_20.csv")
departements_ALL <- full_join(departements, clustering_ALL, by=join_by("code"=="departement")) %>%
  full_join(data_summary_per_sp)

departements_ALL$clus_ward_cut <- factor(departements_ALL$clus_ward_cut, levels=c("1","2","3","4","5","6","7"))

plot_clus_tot <- ggplot() +
  geom_sf(data = departements_ALL, aes(fill=clus_ward_cut), colour="white")+
  ggtitle(label="Clustering of the départements") +
  scale_fill_manual(name="Cluster", values=c('#CC6677', '#44AA99', '#882255', '#117733','#88CCEE','#DDCC77','#332288')) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank())
plot_clus_tot



png(file = "plots/clustering_tot.png",width = 800, height = 652)
plot_clus_tot
dev.off()



#### !!! Map resulting from the cluster analysis on HARVESTED species ####
clustering <- read.csv("processed_data/clustering/7clusters_chisq_no_transfo_20.csv")
departements <- full_join(departements, clustering, by=join_by("code"=="departement")) %>%
  full_join(data_summary_per_sp_HARV)

departements$clus_ward_cut <- factor(departements$clus_ward_cut, levels=c("1","2","3","4","5","6","7"))

plot_clus <- ggplot() +
  geom_sf(data = departements, aes(fill=clus_ward_cut), colour="white", lwd=0.7)+
  scale_fill_manual(name="Cluster", values=c('#882255', '#44AA99', '#CC6677', '#117733','#DDCC77','#88CCEE','#332288')) +
  guides(fill = guide_legend(override.aes = list(size = 8))) +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        legend.title=element_blank(),
        legend.text = element_text(size=25),
        legend.position = c(0.88,0.48))
plot_clus



png(file = "plots/clustering.png",width = 1052, height = 1040)
plot_clus
dev.off()



#### !!! Number of départements / species ####
nb_dpts_per_species_tot <- read.csv("processed_data/nb_dpts_per_species_20.csv")%>%
  arrange(dpts) %>% # Sort the dataframe by number of départements
  mutate(cumulative_frequency_tot = 100*cumsum(dpts)/sum(dpts)) # Create a cumulative frequency column

names(nb_dpts_per_species_tot) <- c("CD_REF", "dpts_tot", "cumulative_frequency_tot")


nb_dpts_per_species_harv <- read.csv("processed_data/nb_dpts_per_species_HARV_20.csv")%>%
  arrange(dpts) %>% # Sort the dataframe by number of départements
  mutate(cumulative_frequency_harv = 100*cumsum(dpts)/sum(dpts)) # Create a cumulative frequency column

names(nb_dpts_per_species_harv) <- c("CD_REF", "dpts_harv", "cumulative_frequency_harv")

# Join datasets to plot cumulative frequencies on the same graph :
nb_dpts_per_species <- full_join(nb_dpts_per_species_tot, nb_dpts_per_species_harv, by="CD_REF")

length(nb_dpts_per_species$dpts_harv[nb_dpts_per_species$dpts_harv==89 & !is.na(nb_dpts_per_species$dpts_harv)])
length(nb_dpts_per_species$dpts_harv[!is.na(nb_dpts_per_species$dpts_harv)])
length(nb_dpts_per_species$dpts_tot[nb_dpts_per_species$dpts_tot==89])

# Plot the cumulative frequency curve
plot_freq_dpt <- ggplot(nb_dpts_per_species) +
  geom_line(aes(x = dpts_tot, y = cumulative_frequency_tot), colour="grey50", size=0.2) +
  # geom_point(aes(x = dpts_tot, y = cumulative_frequency_tot, colour="All plant species")) +
  
  geom_line(aes(x = dpts_harv, y = cumulative_frequency_harv), colour="grey50", size=0.2) +
  # geom_point(aes(x = dpts_harv, y = cumulative_frequency_harv, colour="WHP species")) +
  
  scale_color_manual(values = c("All plant species"="grey20", "WHP species"="seagreen")) +
  
  labs(
    x = "Number of départements",
    y = "Cumulative % of species"
  ) +
  # geom_hline(yintercept=50, size=0.2) +
  
  guides(color = guide_legend(override.aes = list(size = 8))) +

  scale_x_reverse(breaks=c(1,5,10,15,20,25,30,35,40,45,50,55,60,65,70,75,80,85,89))+

  theme_bw() +
  theme_minimal() +
  theme(legend.position = c(0.7,0.7), legend.title = element_blank(),
        legend.background = element_rect(fill = "white"), 
        legend.text=element_text(size=22), 
        axis.title.x=element_text(size=25),
        axis.title.y=element_text(size=25),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        plot.margin = margin(0.4,0,1,1, "cm")) #top, right, bottom, left)
  
plot_freq_dpt

png(file = "plots/nb_dpts_per_species.png", width = 571, height = 492)
plot_freq_dpt
dev.off()

#### !!! V2 Number of départements / species ####
nb_dpts_per_species_tot <- read.csv("processed_data/nb_dpts_per_species_20.csv")
names(nb_dpts_per_species_tot) <- c("CD_REF", "dpts_tot")

nb_dpts_per_species_harv <- read.csv("processed_data/nb_dpts_per_species_HARV_20.csv")
names(nb_dpts_per_species_harv) <- c("CD_REF", "dpts_harv")

# Nombre total de départements, tu peux modifier ce nombre si nécessaire
total_dpts <- 89  

# Calculer le pourcentage de départements pour chaque espèce
nb_dpts_per_species_tot <- nb_dpts_per_species_tot %>%
  mutate(perc_dpts_tot = (dpts_tot / total_dpts) * 100)

nb_dpts_per_species_harv <- nb_dpts_per_species_harv %>%
  mutate(perc_dpts_harv = (dpts_harv / total_dpts) * 100)

# Créer une séquence de pourcentages (de 0% à 100%)
seq_x <- seq(0, 100, by = 1)

# Calculer le pourcentage d'espèces couvrant au moins chaque pourcentage des départements
y_values_tot <- sapply(seq_x, function(x) {
  mean(nb_dpts_per_species_tot$perc_dpts >= x) * 100
})

y_values_harv <- sapply(seq_x, function(x) {
  mean(nb_dpts_per_species_harv$perc_dpts >= x) * 100
})

# Créer un data frame pour le graphique
graph_data <- data.frame(
  perc_departments = seq_x,
  perc_species_harv = y_values_harv,
  perc_species_tot = y_values_tot
  
)

# Tracer le graphique
plot_freq_dpt <- ggplot(graph_data) +
  
  geom_line( aes(x = perc_departments, y = perc_species_harv, colour="WHP species"), size=1) +
  # geom_point(aes(x = perc_departments, y = perc_species_harv, colour="WHP species")) +
  
  geom_line( aes(x = perc_departments, y = perc_species_tot, colour="All plant species"), size=1) +
  # geom_point(aes(x = perc_departments, y = perc_species_tot, colour="All plant species")) +
  
  scale_color_manual(values = c("WHP species"="steelblue2", "All plant species"="grey20")) +
  labs(
    # title = "Reverse cumulative distribution curve",
    # subtitle="y% of species covering at least x% of the départements",
    x = "% of départements",
    y = "% of species"
  ) +
  guides(color = guide_legend(override.aes = list(size = 8))) +
  
  scale_x_continuous(breaks = seq(0,100,10)) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  
  theme_bw() +
  theme_minimal() +
  theme(legend.position = c(0.835,0.9), legend.title = element_blank(),
        legend.background = element_rect(fill = "white"), 
        legend.text=element_text(size=22),
        axis.title.x=element_text(size=25),
        axis.title.y=element_text(size=25),
        axis.text.x=element_text(size=20),
        axis.text.y=element_text(size=20),
        plot.margin = margin(0.4,0,1,1, "cm")) #top, right, bottom, left)


png(file = "plots/nb_dpts_per_species.png", width = 928, height = 571)
plot_freq_dpt
dev.off()



### Number of clusters / species ####

nb_clust_per_species_tot <- read.csv("processed_data/nb_clust_per_species_20.csv")%>%
  arrange(clus_ward_cut) %>% # Sort the dataframe by number of clusters
  mutate(cumulative_frequency_tot = 100*cumsum(clus_ward_cut)/sum(clus_ward_cut)) # Create a cumulative frequency column

names(nb_clust_per_species_tot) <- c("CD_REF", "clust_tot", "cumulative_frequency_tot")


nb_clust_per_species_harv <- read.csv("processed_data/nb_nb_clust_per_species_HARV_20.csv")%>%
  arrange(clus_ward_cut) %>% # Sort the dataframe by number of départements
  mutate(cumulative_frequency_tot = 100*cumsum(clus_ward_cut)/sum(clus_ward_cut)) # Create a cumulative frequency column

names(nb_clust_per_species_harv) <- c("CD_REF", "clust_harv", "cumulative_frequency_harv")

# Join datasets to plot cumulative frequencies on the same graph :
nb_clust_per_species <- full_join(nb_clust_per_species_tot, nb_clust_per_species_harv, by="CD_REF")


# Plot the cumulative frequency curve
plot_freq_clust <- ggplot(nb_clust_per_species) +
  geom_line(aes(x = clust_tot, y = cumulative_frequency_tot), colour="grey50", size=0.2) +
  geom_point(aes(x = clust_tot, y = cumulative_frequency_tot, colour="All plant species")) +
  
  geom_line(aes(x = clust_harv, y = cumulative_frequency_harv), colour="grey50", size=0.2) +
  geom_point(aes(x = clust_harv, y = cumulative_frequency_harv, colour="Wild harvested plants")) +
  
  scale_color_manual(values = c("All plant species"="grey20", "Wild harvested plants"="seagreen")) +
  
  labs(
    title = "Cumulative frequency curve of clusters per species",
    x = "Number of clusters",
    y = "Cumulative frequency"
  ) +
  scale_x_continuous(breaks=seq(1,7,1)) +
  theme_bw() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank())

plot_freq_clust

png(file = "plots/nb_clust_per_species.csv", width = 571, height = 492)
plot_freq_clust
dev.off()





#### Assemble plots ####

png(file = "plots/grouped_nb_rec-nb_sp-clustering_HARV.png", width = 1310, height = 800)
ggarrange(plot_obs, plot_sp, plot_clus, labels=c("a", "b", "c"), ncol=2, nrow=2, align="hv")
dev.off()

png(file = "plots/grouped_nb_rec-nb_sp-clustering_TOT.png", width = 1310, height = 800)
ggarrange(plot_obs_tot, plot_sp_tot, plot_clus_tot, labels=c("a", "b", "c"), ncol=2, nrow=2, align="hv")
dev.off()


png(file = "plots/grouped_freq_clust-freq_dpt.png", width = 1000, height = 450)
ggarrange(plot_freq_dpt, plot_freq_clust, labels=c("a", "b"), ncol=2, align="hv")
dev.off()


###
# 
# png(file = "plots/fig_final.png", width = 1500, height = 1500)
# ggarrange(plot_sp, plot_sp_tot, plot_freq_dpt, plot_clus,  
#           labels=c("a", "b", "c", "d"), ncol=2, nrow=2,
#           font.label = list(size = 30))
# dev.off()


pdf(file = "plots/fig_final.pdf", width = 20, height = 19)
ggarrange(plot_sp, plot_sp_tot, plot_freq_dpt, plot_clus,
          labels=c("a", "b", "c", "d"), ncol=2, nrow=2,
          font.label = list(size = 30))
dev.off()

