# This script is to produce graphs illustrating the distribution of wild-harvested plants.


# Set working directory :
directory <- here::here("2_Distribution")
setwd(directory)

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



#### Figure 3a : number of different species / département ####
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


#### Figure 3b : number of different HARVESTED species / département ####
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



#### Appendix I : number of records / département ####

data_summary_per_obs <- read.csv("processed_data/nb_records_per_dpt_ALL_20.csv")
# data_summary_per_obs <- read.csv("processed_data/nb_records_per_dpt_GBIF.csv")

data_summary_per_obs_map <- full_join(departements,data_summary_per_obs, by=c("code", "dpt", "region"))

data_summary_per_obs_map$nb_obs <- as.numeric(data_summary_per_obs_map$nb_obs)


plot_obs_tot <- ggplot() +
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

# Export :
pdf(file = "plots/nb_records_tot.pdf", width=10, height=7)
plot_obs_tot
dev.off()




#### Figure 3c : Number of départements / species ####

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



#### Figure 3d : Map resulting from the cluster analysis on HARVESTED species ####
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


#### Assemble Figure 3 ####

pdf(file = "plots/Fig_3_fig_final.pdf", width = 20, height = 19)
ggarrange(plot_sp, plot_sp_tot, plot_freq_dpt, plot_clus,
          labels=c("a", "b", "c", "d"), ncol=2, nrow=2,
          font.label = list(size = 30))
dev.off()

