# This script is to produce graphs illustrating the conservation status of wild-harvested plants.

# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/5_Conservation_status/")

# Import packages :
library(ggplot2)
library(dplyr)
library(here)
library(ggbreak)
library(ggrepel)
library(gridExtra)
library(ggpubr)
library(cowplot)
# # devtools::install_github("timcashion/IUCNpalette")
# library("IUCNpalette")


#### Import data ####
status <- read.csv("processed_data/prop_status_prot-reg.csv")



#### Format data ####
# Transform data to get data for all species + harvested, instead of not harvested + harvested
# Create a df with the status for ALL species :
status_all_sp <- status
status_all_sp$harvested = "All species"
status_all_sp <- status_all_sp %>%
  group_by(NIVEAU_ADMIN, CODE_STATUT, harvested) %>%
  summarise(n=sum(n))

# Calculate percentage of each IUCN status for each protection level (NIVEAU_ADMIN)
status_all_sp$percentage <- NA
for (i in unique(status_all_sp$NIVEAU_ADMIN)) {
  for (j in unique(status_all_sp$CODE_STATUT)) {
    
    status_all_sp$percentage[status_all_sp$NIVEAU_ADMIN==i &
                               status_all_sp$CODE_STATUT==j &
                               status_all_sp$harvested=="All species"] = 100*
      status_all_sp$n[status_all_sp$NIVEAU_ADMIN==i &
                                 status_all_sp$CODE_STATUT==j &
                                 status_all_sp$harvested=="All species"] /
      sum(status_all_sp$n[status_all_sp$NIVEAU_ADMIN==i &
                                     status_all_sp$harvested=="All species"])
  }
}

# Bind this data with the similar data for harvested species (percentages are already calculated)
status_all_sp <- rbind(filter(status, harvested!="not harvested"), status_all_sp)
status_all_sp$harvested[status_all_sp$harvested=="harvested"] <- "Harvested" # add cap for aesthetics


# A few mor aesthetic changes for plotting :
status_all_sp$CODE_STATUT <- as.factor(status_all_sp$CODE_STATUT) %>%
  ordered(levels = c("NE","DD","LC","NT","VU","EN","CR","RE", "EX")) #to have the right order on the graph

status_all_sp$NIVEAU_ADMIN[status_all_sp$NIVEAU_ADMIN=="État"] <- "National" #translate
status_all_sp$NIVEAU_ADMIN[status_all_sp$NIVEAU_ADMIN=="Région"] <- "Regional"
status_all_sp$NIVEAU_ADMIN[status_all_sp$NIVEAU_ADMIN=="Département"] <- "Local"
status_all_sp$NIVEAU_ADMIN[status_all_sp$NIVEAU_ADMIN=="no protection or regulation"] <- "None"

status_all_sp$NIVEAU_ADMIN <- as.factor(status_all_sp$NIVEAU_ADMIN) %>%
  ordered(levels = c("National", "Regional", "Local", "None")) #to have the right order on the graph


# Calculate the percentage of species that are not evaluated, data deficient or scored by the IUCN
status_all_sp$IUCN[status_all_sp$CODE_STATUT %in% c("LC","NT","VU","EN","CR","RE", "EX")] = "Scored"
status_all_sp$IUCN[status_all_sp$CODE_STATUT == "DD"] = "Data deficient"
status_all_sp$IUCN[status_all_sp$CODE_STATUT == "NE"] = "Not assessed"
status_all_sp$IUCN <- as.factor(status_all_sp$IUCN) %>%
  ordered(levels = c("Not assessed", "Data deficient", "Scored")) 

status_all_sp$percentage_IUCN <- NA
for (i in unique(status_all_sp$NIVEAU_ADMIN)) {
  for (j in unique(status_all_sp$IUCN)) {
    
    status_all_sp$percentage_IUCN[status_all_sp$NIVEAU_ADMIN==i &
                               status_all_sp$IUCN==j &
                               status_all_sp$harvested=="All species"] = 100*
      status_all_sp$n[status_all_sp$NIVEAU_ADMIN==i &
                        status_all_sp$IUCN==j &
                        status_all_sp$harvested=="All species"] /
      sum(status_all_sp$n[status_all_sp$NIVEAU_ADMIN==i &
                            status_all_sp$harvested=="All species"])
    
    status_all_sp$percentage_IUCN[status_all_sp$NIVEAU_ADMIN==i &
                                    status_all_sp$IUCN==j &
                                    status_all_sp$harvested=="Harvested"] = 100*
      status_all_sp$n[status_all_sp$NIVEAU_ADMIN==i &
                        status_all_sp$IUCN==j &
                        status_all_sp$harvested=="Harvested"] /
      sum(status_all_sp$n[status_all_sp$NIVEAU_ADMIN==i &
                            status_all_sp$harvested=="Harvested"])
  }
}





#### Plot ####
status_all_sp <- status_all_sp[status_all_sp$CODE_STATUT!="EX",] # remove the extinct species

final1 <- ggplot(status_all_sp, aes(x = NIVEAU_ADMIN, y = n, 
                                    fill= CODE_STATUT, color=CODE_STATUT)) +
  scale_fill_manual(name="IUCN Red List Status",
                    values=c("white","#808080","#008000","#ADFF2F","#FFFF00","#FFA500","#FF0000","#000000")) +
  scale_color_manual(name="IUCN Red List Status",
                     values=c("#808080","#808080","#008000","#ADFF2F","#FFFF00","#FFA500","#FF0000","#000000")) +
  geom_bar(stat = "identity") +
  labs(
    # title = "IUCN Status for Each Protection Level in France\n(All Species vs Harvested Species)",
       y = "Number of species") +
  
  # add label with total n of species for each bar :
  geom_text(data = status_all_sp %>% 
              group_by(NIVEAU_ADMIN, harvested) %>% 
              summarise(tot=sum(n)), 
            aes(x = NIVEAU_ADMIN, y = max(tot)+150, 
                label = paste0("n=",tot)),  inherit.aes=F,
            show.legend = F, size = 3.5)+
  
  scale_y_continuous(breaks=c(0,500,1000,1500,2000,2500,3000,3500,4000),
                     limits=c(0,4300))+
  theme_minimal() +
  facet_grid(.~harvested) +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_text(size=12),
        # panel.grid.major.x=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size=13),
        axis.line = element_line(color="grey20", size = 0.5),
        strip.text.x = element_text(size = 12, face="bold"),
        strip.background = element_rect(fill="grey80", colour="white"),
        legend.position = "none")


final1


final2 <- ggplot(status_all_sp, 
                 aes(x = NIVEAU_ADMIN, y = percentage, fill = CODE_STATUT, color=CODE_STATUT)) +
  scale_fill_manual(name="IUCN Red List Status",
                    values=c("white","#808080","#008000","#ADFF2F","#FFFF00","#FFA500","#FF0000","#000000")) +
  scale_color_manual(name="IUCN Red List Status",
                     values=c("#808080","#808080","#008000","#ADFF2F","#FFFF00","#FFA500","#FF0000","#000000")) +
  geom_bar(stat = "identity") +
  labs( x = "
        Protection Level",
    y = "Percentage of species  ") +
  
  theme_minimal() +
  facet_grid(.~harvested) +
  scale_y_continuous(breaks=seq(0,100,10), 
                     labels=c(seq(0,90,10), "     100"),
                     sec.axis=sec_axis( ~ ., 
                                        breaks=seq(0,100,10), 
                                        labels=c(seq(0,90,10), "100    "))) + # adjust breaks
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12),
        axis.text.y = element_text(size=12),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=13),
        # panel.grid.major.x=element_blank(),
        axis.line = element_line(color="grey20", size = 0.5),
        strip.text.x = element_blank())

final2


# png(file = "plots/conservation.png", 
#     width = 2600, 
#     height = 2360,
#     res = 400)
# ggpubr::ggarrange(final1, final2, align="v",
#                   nrow=2, common.legend = T, legend="right", labels=c("a", "b"))
# dev.off()

pdf(file = "plots/conservation.pdf", width=8, height=7)
ggpubr::ggarrange(final1, final2, align="v",
                  nrow=2, common.legend = T, legend="right", labels=c("a", "b"),
                  font.label = list(size = 16))
dev.off()
