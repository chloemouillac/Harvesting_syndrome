# This script is to calculate the number of species in each département of metropolitan France :
# the total number of species and the number of potentially harvested species


# Set working directory :
directory <- here::here("2_Distribution")
setwd(directory)

# Import packages :
library(terra)
library(dplyr)
library(here)
library(stringr)
library(sf)


#### Import data ####
# Import grid raster for calculations :
grid10 <- rast("raw_data/grid10KM.tif")
grid20 <- aggregate(grid10, fact=2)

# Import presence data :
pres_data10 <- read.csv("processed_data/OpenObs+GBIF_REDUCED_10km.csv")
pres_data20 <- read.csv("processed_data/OpenObs+GBIF_REDUCED_20km.csv")
pres_data10_oobs <- read.csv("processed_data/OpenObs_REDUCED_10km.csv")
pres_data20_oobs <- read.csv("processed_data/OpenObs_REDUCED_20km.csv")


# Read the delimitations of the French départements :
departements <- read_sf("raw_data/departements_FR.shp") %>%
  st_sf() #convert to sf object

# List of harvested species :
list_species <- read.csv("Harvesting_syndrome/WHP_correpondence_table_v17.csv") %>%
  subset(select=CD_REF)

list_species$harvested <- "harvested"




####### Initiate a loop ########
list_pres_data <- list(pres_data10,
                       pres_data20,
                       pres_data10_oobs,
                       pres_data20_oobs)

names(list_pres_data) <- c("10", "20", "10_oobs", "20_oobs")


list_results_nb_dpts_ALL <- list()
list_results_nb_dpts_HARV <- list()
list_results_nb_clust_ALL <- list()
list_results_nb_clust_HARV <- list()
list_results_nb_records_ALL <- list()
list_results_nb_records_HARV <- list()
list_results_nb_sp_ALL <- list()
list_results_nb_sp_HARV <- list()


# Loop :

for (i in 1:length(list_pres_data)) {
  
  data <- list_pres_data[[i]]
  name <- names(list_pres_data)[i]
  
  data <- data %>%
    subset(select=c(decimalLatitude, decimalLongitude, CD_REF)) %>%
    na.omit()
  
  data_sf <- data %>% #remove geom column
    st_as_sf(coords = c("decimalLongitude", "decimalLatitude")) #convert to sf object
  st_crs(data_sf) = st_crs(departements)
  
  
  #### Join the presence data with the départements : ####
  # Associate a département to each record
  data_summary <- st_join(data_sf, departements, join = st_within) %>%
    na.omit() # might take a few minutes to run
  
  
  
  #### Number of different départements each species is in : #### 
  nb_dpts <- data_summary %>%
    st_drop_geometry() %>%
    unique() %>%
    group_by(CD_REF) %>%
    summarise(dpts=n())
  
  nb_dpts_HARV <- nb_dpts %>%
    subset(CD_REF %in% list_species$CD_REF)

  
  # Append to list : 
  list_results_nb_dpts_ALL <- c(list_results_nb_dpts_ALL, nb_dpts)
  list_results_nb_dpts_HARV <- c(list_results_nb_dpts_HARV, nb_dpts_HARV)
  
  # Export :
  write.csv(nb_dpts, paste0("processed_data/nb_dpts_per_species_", name, ".csv"), row.names=F)
  write.csv(nb_dpts_HARV, paste0("processed_data/nb_dpts_per_species_HARV_", name, ".csv"), row.names=F)
  

  #### Number of different clusters each species is in : ####
  clustering <- read.csv("processed_data/clustering/7clusters_chisq_no_transfo_ALL_SPECIES_20.csv")
  clustering_HARV <- read.csv("processed_data/clustering/7clusters_chisq_no_transfo_20.csv")
  
  nb_clust <- full_join(data_summary, clustering, join_by(code==departement)) %>%
    st_drop_geometry() %>%
    subset(select=c(CD_REF, clus_ward_cut)) %>%
    unique() %>%
    group_by(CD_REF) %>%
    summarise(clus_ward_cut=n())
  
  nb_clust_HARV <- data_summary %>%
    subset(CD_REF %in% list_species$CD_REF) %>%
    full_join(clustering_HARV, join_by(code==departement)) %>%
    st_drop_geometry() %>%
    subset(select=c(CD_REF, clus_ward_cut)) %>%
    unique() %>%
    group_by(CD_REF) %>%
    summarise(clus_ward_cut=n())
  
  # Append to list : 
  list_results_nb_clust_ALL <- c(list_results_nb_clust_ALL, nb_clust)
  list_results_nb_cust_HARV <- c(list_results_nb_clust_HARV, nb_clust_HARV)
  
  # Export :
  write.csv(nb_clust, paste0("processed_data/nb_clust_per_species_", name, ".csv"), row.names=F)
  write.csv(nb_clust_HARV, paste0("processed_data/nb_nb_clust_per_species_HARV_", name, ".csv"), row.names=F)
  
# 
#   #### Number of records per département :####
#   data_summary_per_obs <- st_drop_geometry(data_summary) %>% #to write the data, with the "geometry" column it doesn't work...
#     group_by(code, dpt, region) %>%
#     summarise(nb_obs = n())
# 
#   data_summary_per_obs <- data_summary_per_obs[!is.na(data_summary_per_obs$dpt),]
# 
#   # Append to list :
#   list_results_nb_records_ALL <- c(list_results_nb_records_ALL, data_summary_per_obs)
#   # Export :
#   write.csv(data_summary_per_obs, paste0("processed_data/nb_records_per_dpt_ALL_", name, ".csv"), row.names=F)
# 
# 
# 
#   #### Number of records of HARVESTED species per département : ####
#   data_summary_harv <- left_join(data_summary, list_species) %>%
#     subset(harvested=="harvested")
# 
#   data_summary_per_obs_harv <- st_drop_geometry(data_summary_harv) %>% #to write the data, with the "geometry" column it doesn't work...
#     group_by(code, dpt, region) %>%
#     summarise(nb_obs = n())
# 
#   data_summary_per_obs_harv <- data_summary_per_obs_harv[!is.na(data_summary_per_obs_harv$dpt),]
# 
#   # Append to list :
#   list_results_nb_records_HARV <- c(list_results_nb_records_HARV, data_summary_per_obs_harv)
#   # Export :
#   write.csv(data_summary_per_obs_harv, paste0("processed_data/nb_records_per_dpt_HARV_", name, ".csv"), row.names=F)
# 
# 
# 
#   #### Number of species per département : ####
#   data_summary_per_sp <- st_drop_geometry(data_summary) %>% #to write the data, with the "geometry" column it doesn't work...
#     group_by(code, dpt, region) %>%
#     summarise(nb_sp = n_distinct(CD_REF))
# 
#   data_summary_per_sp <- data_summary_per_sp[!is.na(data_summary_per_sp$dpt),]
# 
#   # Append to list :
#   list_results_nb_sp_ALL <- c(list_results_nb_sp_ALL, data_summary_per_sp)
#   # Export :
#   write.csv(data_summary_per_sp, paste0("processed_data/nb_sp_per_dpt_ALL_", name, ".csv"), row.names=F)
# 
# 
# 
#   #### Number of HARVESTED species per département : ####
#   data_summary_per_sp_harv <- st_drop_geometry(data_summary_harv) %>% #to write the data, with the "geometry" column it doesn't work...
#     group_by(code, dpt, region) %>%
#     summarise(nb_sp = n_distinct(CD_REF))
# 
#   data_summary_per_sp_harv <- data_summary_per_sp_harv[!is.na(data_summary_per_sp_harv$dpt),]
# 
#   # Append to list :
#   list_results_nb_sp_HARV <- c(list_results_nb_sp_HARV, data_summary_per_sp_harv)
#   # Export :
#   write.csv(data_summary_per_sp_harv, paste0("processed_data/nb_sp_per_dpt_HARV_", name, ".csv"), row.names=F)

}



################################################...
library(ggplot2)
ggplot(nb_dpts, aes(x=dpts, y=percent)) +
  geom_col() +
  xlab("Number of départements") +
  ylab("Percentage of species")

###### And GBIF ?? #####
