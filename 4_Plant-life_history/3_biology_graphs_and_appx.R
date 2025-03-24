# This script is to produce graphs illustrating the life-history of wild-harvested plants.


# Set working directory :
directory <- here::here("4_Plant-life_history")
setwd(directory)

# Import packages :
library(dplyr)
library(stringr)
library(ggplot2)
library(ggtern)
library(ggpubr)


#### Import data ####
list_species_WHP <- read.csv(here::here("0_List_vascular_flora", "processed_data", "corresp_vascular_2018_&_taxrefv17.csv"))

CSR_data_WHP <- read.csv("processed_data/StrateFy_CSR_clean_output_WHP.csv") 
  
CSR_data_ALL <- read.csv("processed_data/StrateFy_CSR_clean_output_SELECTED_ALL_FRANCE.csv", dec=".")

Raunkiaer_data_WHP <- read.csv("processed_data/Raunkieaer_data_REVIEWED_WHP.csv") %>%
  select(c(CD_REF, NOM_VALIDE, choix_type_bio)) %>%
  unique()
  
Raunkiaer_data_ALL <- read.csv("processed_data/Raunkieaer_data_REVIEWED_ALL_FRANCE_SELECTED.csv") %>%
  select(c(CD_REF, LB_NOM, choix_type_bio)) %>%
  unique()



#### DF grouping biological data - WHP ####
bio_data_WHP <- full_join(CSR_data_WHP, Raunkiaer_data_WHP) %>%
  subset(!is.na(choix_type_bio))

bio_data_WHP$type_bio_simple[grepl("hém", bio_data_WHP$choix_type_bio)] <- "hemicr"
bio_data_WHP$type_bio_simple[grepl("phan", bio_data_WHP$choix_type_bio)] <- "phan"
bio_data_WHP$type_bio_simple[grepl("théro", bio_data_WHP$choix_type_bio)] <- "thero"
bio_data_WHP$type_bio_simple[grepl("cham", bio_data_WHP$choix_type_bio)] <- "cham"
bio_data_WHP$type_bio_simple[grepl("géo", bio_data_WHP$choix_type_bio)] <- "geo"
bio_data_WHP$type_bio_simple[grepl("hydro", bio_data_WHP$choix_type_bio)] <- "hydro"
bio_data_WHP$type_bio_simple[grepl("hélo", bio_data_WHP$choix_type_bio)] <- "helo"


bio_data_WHP$csr_simple <- str_split_i(bio_data_WHP$strategy_class, pattern="/", i=1)
bio_data_WHP$type_bio_simple <- factor(bio_data_WHP$type_bio_simple, levels=c("hemicr", "phan", "thero" , "cham", "geo", "hydro", "helo"))



#### DF grouping biological data - all french flora ####
bio_data_ALL <- left_join(CSR_data_WHP, Raunkiaer_data_WHP) %>% #left join to keep only the species that were selected for the CSR
  subset(!is.na(choix_type_bio))

bio_data_ALL$type_bio_simple[grepl("hém", bio_data_ALL$choix_type_bio)] <- "hemicr"
bio_data_ALL$type_bio_simple[grepl("phan", bio_data_ALL$choix_type_bio)] <- "phan"
bio_data_ALL$type_bio_simple[grepl("théro", bio_data_ALL$choix_type_bio)] <- "thero"
bio_data_ALL$type_bio_simple[grepl("cham", bio_data_ALL$choix_type_bio)] <- "cham"
bio_data_ALL$type_bio_simple[grepl("géo", bio_data_ALL$choix_type_bio)] <- "geo"
bio_data_ALL$type_bio_simple[grepl("hydro", bio_data_ALL$choix_type_bio)] <- "hydro"
bio_data_ALL$type_bio_simple[grepl("hélo", bio_data_ALL$choix_type_bio)] <- "helo"


bio_data_ALL$csr_simple <- str_split_i(bio_data_ALL$strategy_class, pattern="/", i=1)
bio_data_ALL$type_bio_simple <- factor(bio_data_ALL$type_bio_simple, levels=c("hemicr", "phan", "thero" , "cham", "geo", "hydro", "helo"))

CSR_data_ALL$csr_simple <- str_split_i(CSR_data_ALL$strategy_class, pattern="/", i=1)



#### Descr : Raunkiaear - WHP ####
bio_freq_WHP <- as.data.frame(table(bio_data_WHP$type_bio_simple))

ggplot(bio_freq_WHP, aes(x=reorder(Var1, -Freq), y=100*Freq/sum(Freq))) +
  geom_col(fill="skyblue") +
  labs(y= "Proportion of species", x = "Raunkiaear life form") +
  theme_minimal(base_size = 20)

# Frequencies :
bio_freq_WHP$p <- bio_freq_WHP$Freq / sum(bio_freq_WHP$Freq )


#### Descr : Raunkiaear - All French flora #### 
bio_freq_ALL <- as.data.frame(table(bio_data_ALL$type_bio_simple))

ggplot(bio_freq_ALL, aes(x=reorder(Var1, -Freq), y=100*Freq/sum(Freq))) +
  geom_col(fill="skyblue") +
  labs(y= "Proportion of species", x = "Raunkiaear life form") +
  theme_minimal(base_size = 20)

# Frequencies :
bio_freq_ALL$p <- bio_freq_ALL$Freq / sum(bio_freq_ALL$Freq )


#### Descr : CSR - WHP ####
csr_freq_WHP <- as.data.frame(table(bio_data_WHP$csr_simple))

ggplot(csr_freq_WHP, aes(x=reorder(Var1, -Freq), y=100*Freq/sum(Freq))) +
  geom_col(fill="skyblue") +
  labs(y= "Proportion of species", x = "CSR category") +
  theme_minimal(base_size = 20)

# Frequencies :
csr_freq_WHP$p <- csr_freq_WHP$Freq / sum(csr_freq_WHP$Freq )


#### Descr : CSR - all french flora ####
csr_freq_ALL <- as.data.frame(table(CSR_data_ALL$csr_simple))

ggplot(csr_freq_ALL, aes(x=reorder(Var1, -Freq), y=100*Freq/sum(Freq))) +
  geom_col(fill="skyblue") +
  labs(y= "Proportion of species", x = "CSR category") +
  theme_minimal(base_size = 20)


# Frequencies :
csr_freq_ALL$p <- csr_freq_ALL$Freq / sum(csr_freq_ALL$Freq )



#### Ternary plots with ggtern ####
# Order data :
bio_data_WHP$type_bio_simple <- factor(bio_data_WHP$type_bio_simple, levels=c("hemicr",  "geo", "thero" , "cham", "phan", "hydro", "helo"))
bio_data_WHP <- with(bio_data_WHP, bio_data_WHP[order(type_bio_simple),]) %>%
  subset(!is.na(strategy_class)) #remove nas

# Increase slightly the lower values of CSR (otherwise they are cropped out of the plot) :
csr_WHP_born <- bio_data_WHP[,3:5]
csr_WHP_born[csr_WHP_born<2] <- 2
bio_data_WHP_born <- bio_data_WHP
bio_data_WHP_born[,3:5] <- csr_WHP_born



#### Density - WHP ####
plot_dens_WHP <- ggtern(data=bio_data_WHP_born, 
                    aes(C, S, R)) + 
  xlab("C") + ylab("S") + zlab("R") +
  stat_density_tern(geom = "polygon", n=200, contour = TRUE, 
                    aes(alpha=..level..), fill="grey4", bdl=0, show.legend=T, bins=50, expand=c(0.5,0.5)) +
  geom_point(size=1, alpha=0.5) +
  ggtitle("a") +
  theme_bw(base_size=25) + # For clarity
  theme_hidegrid() + # Turn off both major and minor
  theme_showarrows() +
  guides(colour = guide_legend(override.aes = list(size = 7), nrow = 1)) +
  theme(legend.position = "bottom")

plot_dens_WHP

## Export
# png(file = "plots/BW_grime_triangle_HARV.png", width = 890, height = 710)
# plot_dens_WHP
# dev.off()

pdf(file = "plots/BW_grime_triangle_HARV.pdf")
plot_dens_WHP
dev.off()



#### Density - all french flora ####

CSR_data_ALL_born <- CSR_data_ALL
CSR_data_ALL_born[CSR_data_ALL_born<2] <- 2

plot_dens_tot <- ggtern(data=CSR_data_ALL_born, 
                    aes(C, S, R)) + 
  xlab("C") + ylab("S") + zlab("R") +
  stat_density_tern(geom = "polygon", n=200, contour = TRUE, 
                    aes(alpha=..level..), fill="grey4", bdl=0, show.legend=T, bins=50, expand=c(0.5,0.5)) +
  geom_point(size=1, alpha=0.5) +
  ggtitle("b") +
  theme_bw(base_size=25) + # For clarity
  theme_hidegrid() + # Turn off both major and minor
  theme_showarrows() +
  guides(colour = guide_legend(override.aes = list(size = 7), nrow = 1)) +
  theme(legend.position = "bottom")

plot_dens_tot



## Export
# png(file = "plots/BW_grime_triangle_TOT.png", width = 890, height = 710)
# plot_dens_tot
# dev.off()

pdf(file = "plots/BW_grime_triangle_TOT.pdf")
plot_dens_tot
dev.off()

legend <- get_legend(plot_dens_WHP, position='bottom')

pdf(file = "plots/BW_grime_triangle_dual.pdf", width=12, height=7)
grid.arrange(plot_dens_WHP+theme(legend.position="hidden"), 
             plot_dens_tot+theme(legend.position="hidden"),
             legend,
             layout_matrix= rbind(c(1,2), 3),
             ncol=2, nrow=2,
             heights=c(6,1))
dev.off()
