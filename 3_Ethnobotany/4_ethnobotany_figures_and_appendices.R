# This script is to produce graphs illustrating the uses and harvested parts of wild-harvested plants.

# Set working directory :
directory <- here::here("3_Ethnobotany")
setwd(directory)

# Import packages :
library(ggplot2)
# devtools::install_github("davidsjoberg/ggsankey")
library(ggnewscale)
library(ggsankey)
library(reshape2)
library(dplyr)
library(ggrepel)
library(stringr)
library(tidyverse)
library(RColorBrewer)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(scales)  # For percent formatting
library(patchwork)  # For combining plots with a shared legend


#### Import data ####
# Import uses :
uses <- read.csv("processed_data/uses_detail_count.csv") %>%
  melt(id.vars=c("NOM_VALIDE", "CD_REF"))

uses <- uses %>%
  filter(value>0)
names(uses) <- c("NOM_VALIDE", "CD_REF", "USE", "value")

uses$USE <- as.character(uses$USE)
uses$USE[uses$USE=="Medical.Therapeutic"] <- "Medical Therapeutic"
uses$USE[uses$USE=="Food.and.Beverages"] <- "Food and Beverages"
uses$USE[uses$USE=="Ornamental.Plants"] <- "Ornamental Plants"


# Import harvested parts :
parts <- read.csv("processed_data/harvested_detail.csv") %>%
  melt(c("CD_REF","NOM_VALIDE")) %>%
  subset(value>0 & variable != "champignons.et.lichens.ou.autres", select=-value) %>%
  unique() 
names(parts) <- c("CD_REF","NOM_VALIDE", "PART")


# Import list of french vascular flora :
vascular_list <- read.csv(here::here("list_vascular_v17.csv") )%>%
  subset(select=c("CD_REF", "FAMILLE", "LB_NOM"))


#### Join uses and list of French flora ####
# to get the proportion of species that aren't on my list for each family
join_uses_vascular <- full_join(uses, vascular_list) %>%
  unique()

join_uses_vascular$USE <- as.factor(join_uses_vascular$USE)
levels(join_uses_vascular$USE) <- c(levels(join_uses_vascular$USE), "No recorded use")

join_uses_vascular$USE[is.na(join_uses_vascular$USE)] <- "No recorded use"


join_uses_vascular_summar <- join_uses_vascular %>% # Calculate the number of species per use in each family
  group_by(FAMILLE, USE) %>%
  summarise(weight=sum(value), nb_species=n()) %>%
  filter(!is.na(FAMILLE))


#### Get the percentage of each use, and percentage of species not used ####
for (fam in unique(join_uses_vascular_summar$FAMILLE)) {
  for (use in unique(join_uses_vascular_summar$USE)) {
    if (use == "No recorded use") {
      join_uses_vascular_summar$percentage[join_uses_vascular_summar$FAMILLE==fam & join_uses_vascular_summar$USE==use] <- 
        join_uses_vascular_summar$nb_species[join_uses_vascular_summar$FAMILLE==fam & join_uses_vascular_summar$USE==use] /
        sum(join_uses_vascular_summar$nb_species[join_uses_vascular_summar$FAMILLE==fam], na.rm = T)
    } else {
      join_uses_vascular_summar$percentage[join_uses_vascular_summar$FAMILLE==fam & join_uses_vascular_summar$USE==use] <- 
        join_uses_vascular_summar$nb_species[join_uses_vascular_summar$FAMILLE==fam & join_uses_vascular_summar$USE==use] /
        sum(join_uses_vascular_summar$nb_species[join_uses_vascular_summar$FAMILLE==fam & join_uses_vascular_summar$USE
                                               != "No recorded use"], na.rm = T)
    }
  }
}
join_uses_vascular_summar$percentage <- 100*round(join_uses_vascular_summar$percentage, 2)

# Export :
write.csv(join_uses_vascular_summar, "processed_data/uses_percentages.csv", row.names=F)



#### 10 families - End-uses of the most harvested plant families plot #####
# 10 most harvested plant families :
most_harv <-  c("Lamiaceae", "Rosaceae", "Ranunculaceae", "Boraginaceae", "Caprifoliaceae", "Ericaceae", "Violaceae", "Crassulaceae","Pinaceae", "Polygonaceae")
most_harv <- factor(most_harv, levels= c("Lamiaceae", "Rosaceae", "Ranunculaceae", "Boraginaceae", "Caprifoliaceae", "Ericaceae", "Violaceae", "Crassulaceae","Pinaceae", "Polygonaceae"))
# Ordered by highest number of WHP species to lowest, and when equal number, the highest percentage of WHP goes first


#### Figure 2a : % of harvesetd species in the 10 selected families ####
# Get the percentage of harvested species : 
pct_on_harv_list <- read.csv(here::here("1_Phylogeny", "processed_data", "tip_data_for_tree.csv"))
pct_on_harv_list_10fam <- pct_on_harv_list %>%
  subset(FAMILLE %in% most_harv)

# Get the total percentage of harvested species
ALL_list <- data.frame(FAMILLE="ALL",
                         number_harvested=sum(pct_on_harv_list$number_harvested),
                         total_sp= sum(pct_on_harv_list$total_sp),
                         percentage_harvested= round(100*
                           sum(pct_on_harv_list$number_harvested)/
                           sum(pct_on_harv_list$total_sp),1))


pct_on_harv_list_10fam <- rbind(pct_on_harv_list_10fam, ALL_list)
pct_on_harv_list_10fam$percentage_harvested <- round(pct_on_harv_list_10fam$percentage_harvested, 1)

pct_on_harv_list_10fam$FAMILLE <- factor(pct_on_harv_list_10fam$FAMILLE, c("ALL", "Lamiaceae", "Rosaceae", "Ranunculaceae", "Boraginaceae", "Caprifoliaceae", "Ericaceae", "Violaceae", "Crassulaceae","Pinaceae", "Polygonaceae"))



# Plot :
plot1 <- ggplot(pct_on_harv_list_10fam[pct_on_harv_list_10fam$FAMILLE!="ALL",], aes(x=FAMILLE,
                                                                                      y=percentage_harvested, width=0.7)) +
  geom_col(fill="grey60") +
  
  scale_y_continuous(breaks = seq(0,40,10), limits = c(0, 48),
                     labels= seq(0,40,10)) +
  
  geom_hline(yintercept=pct_on_harv_list_10fam$percentage_harvested[pct_on_harv_list_10fam$FAMILLE=="ALL"],
             colour="black") +
  
  annotate("text", x = 0.5,
                y = 2 + pct_on_harv_list_10fam$percentage_harvested[pct_on_harv_list_10fam$FAMILLE=="ALL"],
           label =  "Average %",  size = 6, hjust=0) +
  
  theme_bw() +
  labs(y="% of commercial WHP species") +
  theme(legend.title=element_blank(),
        axis.text.x = element_blank(), axis.title.x = element_blank(),
        axis.text.y = element_text(size=20), axis.title.y = element_text(size=18),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(1,1,0,2, "cm")) #top, right, bottom, left)
plot1



#### Figure 2b : % of each use in the 10 families ####
uses_per_fam <- join_uses_vascular_summar[join_uses_vascular_summar$FAMILLE %in% most_harv,]

# Calculate the mean % of each use over all families and add the data to uses_per_fam
ALL <- join_uses_vascular_summar %>%
  group_by(USE) %>%
  summarise(weight=sum(weight),
            nb_species=sum(nb_species))
ALL$percentage[ALL$USE!="No recorded use"] <- ALL$nb_species[ALL$USE!="No recorded use"]/
  sum(ALL$nb_species[ALL$USE!="No recorded use"])*100
ALL$percentage[ALL$USE=="No recorded use"] <- ALL$nb_species[ALL$USE=="No recorded use"]/sum(ALL$nb_species)
ALL$FAMILLE<- "ALL"
uses_per_fam <- rbind(uses_per_fam, ALL)
#

uses_per_fam$percentage <- round(uses_per_fam$percentage,1)
uses_per_fam$FAMILLE <- factor(uses_per_fam$FAMILLE, levels= c("ALL", "Lamiaceae", "Rosaceae", "Ranunculaceae", "Boraginaceae", "Caprifoliaceae", "Ericaceae", "Violaceae", "Crassulaceae","Pinaceae", "Polygonaceae"))

uses_per_fam$USE <- as.character(uses_per_fam$USE)
uses_per_fam$USE <- gsub("\\.", " ", uses_per_fam$USE)

uses_per_fam$USE <- factor(uses_per_fam$USE, levels=c("No recorded use", "Other", "Ornamental Plants","Cosmetics", "Food and Beverages", "Crafts", "Medical Therapeutic"))




# calculate a column with the difference of each use-weight compared to the mean use-weight :
for (fam in unique(uses_per_fam$FAMILLE)) {
  for(use in unique(uses_per_fam$USE)) {
    uses_per_fam$diff[uses_per_fam$FAMILLE==fam & uses_per_fam$USE==use] <-
      uses_per_fam$percentage[uses_per_fam$FAMILLE==fam & uses_per_fam$USE==use] -
      uses_per_fam$percentage[uses_per_fam$FAMILLE=="ALL" & uses_per_fam$USE==use]
    # and add column with the mean values :
    uses_per_fam$mean_percentage[uses_per_fam$FAMILLE==fam & uses_per_fam$USE==use] <- uses_per_fam$percentage[uses_per_fam$FAMILLE=="ALL" & uses_per_fam$USE==use]
  }
}


uses_per_fam$positive <- ifelse(uses_per_fam$diff>=0,"yes","no")
uses_per_fam_simpl <- uses_per_fam[uses_per_fam$USE %in% c("Crafts", "Food and Beverages", "Medical Therapeutic"),]


# Add a column that will be used for column numbers
k=0
for (fam in levels(uses_per_fam_simpl$FAMILLE)) {
  uses_per_fam_simpl$fam_n[uses_per_fam_simpl$FAMILLE==fam] <- k
  k=k+1
}
  

# Add a column that will be used to jitter the vertical lines
uses_per_fam_simpl$jitter[uses_per_fam_simpl$USE=="Medical Therapeutic"] <- +0.12
uses_per_fam_simpl$jitter[uses_per_fam_simpl$USE=="Food and Beverages"] <- +0.12
uses_per_fam_simpl$jitter[uses_per_fam_simpl$USE=="Crafts"] <- -0.12


# Add a column with total number of harvested species / family
for (fam in levels(uses_per_fam_simpl$FAMILLE)) {
  uses_per_fam_simpl$total_sp[uses_per_fam_simpl$FAMILLE==fam] <- 
    pct_on_harv_list_10fam$number_harvested[pct_on_harv_list_10fam$FAMILLE==fam]
}

# Reorder for plotting :
uses_per_fam_simpl <- arrange(uses_per_fam_simpl, desc(total_sp))


# Add a stripe variable for alternate background grid filling :
uses_per_fam_simpl <- mutate(uses_per_fam_simpl,
                             stripe = factor(ifelse(fam_n %% 2 == 0, 1, 0)))

# Plot :
plot2 <- ggplot(uses_per_fam_simpl[uses_per_fam_simpl$FAMILLE != "ALL",], aes(x=fam_n,
                                                                              y=percentage)) +
  geom_rect(aes(xmax = fam_n + 0.5, 
                xmin = fam_n-0.5, 
                ymin = -Inf, 
                ymax = Inf, 
                fill = stripe), alpha = 0.2, show.legend = F) + 
  
  scale_fill_manual(values = c("transparent", "grey90")) + 
  
  geom_hline(data=uses_per_fam_simpl[uses_per_fam_simpl$FAMILLE == "ALL",],
             aes(yintercept=percentage, color=USE), show.legend=F) +
  
  geom_segment(data=uses_per_fam_simpl[uses_per_fam_simpl$FAMILLE != "ALL",],
               aes(x=fam_n+jitter, y=mean_percentage, yend=mean_percentage+diff, color=USE),
               linewidth=7, show.legend=F) +
  
  scale_color_manual(name = "Use categories",
                    values=c('orange','#66B2B2','mediumpurple3')) +
  
  new_scale_color() +
  
  geom_point(aes(x=fam_n+jitter,
                 y=percentage,
                 group=USE, color=USE), size=6) +
  
  # geom_vline(xintercept=seq(1.5,9.5,1), alpha=0.5, colour="grey60", size=0.2) +
  
  scale_color_manual(name = "Use categories",
                     values=c('darkorange', '#008080', 'mediumpurple4')) +
  
  scale_y_continuous(breaks = seq(0,50,10), limits=c(0,56),
                     labels= seq(0,50,10)) +
  
  scale_x_continuous(breaks=seq(1,10,1),
                     labels=paste0(most_harv,
                                   " (n=",
                                   uses_per_fam_simpl$total_sp[uses_per_fam_simpl$FAMILLE != "ALL" &
                                                                 uses_per_fam_simpl$USE=="Medical Therapeutic"],
                                   ")")) +
  
  labs(y="% of WHP species by use category") +
  theme_bw() +
  theme(axis.text.x = element_text(size=18, angle=45, hjust=1), axis.title.x = element_blank(),
        axis.text.y = element_text(size=20), axis.title.y = element_text(size=18),
        legend.position="bottom", legend.title=element_blank(), legend.text=element_text(size=15),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        plot.margin = margin(0,1,0,2, "cm")) + #top, right, bottom, left
  coord_cartesian( xlim = c(0.9, 10.1))
plot2



#### Assemble figure 2 ####
# Export :
pdf(file = "plots/Fig_2_uses_10fam.pdf",
    width = 10,
    height = 15)

ggpubr::ggarrange(print(plot1), print(plot2), align="v",
                  nrow=2, labels=c("a", "b"), heights=c(0.5,1),
                  font.label = list(size = 25))
dev.off()


#### Appendix G : Contribution of each dataset for each use ####
# Read the data
uses_refs <- read.csv("processed_data/uses_detail_with_sources.csv")

# Define the order of categories (x-axis)
uses_refs$SELECTED_TYPOLOGY_ENG <- factor(uses_refs$SELECTED_TYPOLOGY_ENG, 
                                          levels = c("Medical/Therapeutic", "Food and Beverages", 
                                                     "Crafts", "Ornamental Plants", 
                                                     "Cosmetics", "Other"))

# Define the order of databases (legend & stack order)
uses_refs$SOURCE <- factor(uses_refs$SOURCE, 
                           levels = c("CBN", "Chabert", "PFAF", "KEW"))

# Custom color palette
custom_colors <- c(
  "CBN" = "#D82632", 
  "Chabert" = "#85B22C", 
  "PFAF" = "#51A3CC", 
  "KEW" = "#6551CC"
)

# Base plot (to ensure consistent themes)
base_plot <- ggplot(uses_refs, aes(x = SELECTED_TYPOLOGY_ENG, fill = SOURCE)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),  # Remove x-axis title
        legend.position = "right",  # Places legend on the side
        legend.direction = "vertical") +  # Make legend vertical
  scale_fill_manual(values = custom_colors)  # Apply custom colors

# Create the first plot (species count)
p1 <- base_plot +
  geom_bar(position = "stack") +
  labs(y = "Count of species")

# Create the second plot (percentage)
p2 <- base_plot +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +  # Convert y-axis to percentages
  labs(y = "Proportion")

# Combine plots with shared legend and title
combined_plot <- (p1 + p2) +
  plot_layout(guides = "collect") +  # Collects the legend into one
  plot_annotation(title = "Contribution of each dataset for each use")  # Center main title

# Display the final combined plot
print(combined_plot)

# Export :  
pdf(file = "plots/Appx_G_data_contribution.pdf", 
    width = 10, 
    height = 5)
print(combined_plot)
dev.off()





#### Appendix O : Graph with the % of each part used ####
df <- read.csv("processed_data/harvested_detail.csv", header = TRUE, stringsAsFactors = FALSE) %>%
  # Identify destructively harvested species
  mutate(Destructive_harvest = ifelse(underground_part == 1 | whole_plant == 1, 1, 0))

# Convert data to long format
df_long <- df %>%
  pivot_longer(cols = 3:14, names_to = "Harvested_part", values_to = "Present") %>%
  filter(Present == 1)  # Keep only harvested parts

# Format harvested part names (capitalize and replace underscores with spaces)
df_long$Harvested_part <- df_long$Harvested_part %>%
  str_replace_all("_", " ") %>%   # Replace underscores with spaces
  str_to_title()  # Capitalize each word

# Calculate percentage of species per harvested part
part_counts <- df_long %>%
  group_by(Harvested_part) %>%
  summarise(Count = n()) %>%
  mutate(Percentage = (Count / n_distinct(df$CD_REF)) * 100)  # % of species

# Identify parts associated with destructive harvest
destructive_parts <- c("Destructive Harvest", "Underground Part", "Whole Plant")

# Plot percentage of species for each harvested part
parts <- ggplot(part_counts, aes(x = reorder(Harvested_part, Percentage), y = Percentage, fill = Harvested_part %in% destructive_parts)) +
  geom_bar(stat = "identity") +  
  geom_text(aes(label = sprintf("%.1f%%", Percentage)), hjust = -0.2, size = 3) +  # Add percentage labels
  scale_fill_manual(values = c("FALSE" = "#1c9099", "TRUE" = "#d73027")) +  # Non-destructive in blue, destructive in red
  coord_flip() +  # Flip to horizontal
  scale_y_continuous(breaks = seq(0, 100, by = 10), limits=c(0,70)) +  # Steps for y-axis
  labs(x = NULL,  # Remove x-axis title
       y = "
       Percentage of species") +
  theme_minimal() +
  theme(legend.position = "none")  # Remove legend


# Export :  
pdf(file = "plots/Appx_O_percentage_harvested_parts.pdf", 
    width = 5, 
    height = 5)
print(parts)
dev.off()



#### Appendix P : tripartite network ####
# Import Raunkiaer data :
raunkiear <- read.csv(here::here("4_Plant-life_history", "processed_data", "Raunkieaer_data_REVIEWED_WHP.csv")) %>%
  subset(select=c(CD_REF, NOM_VALIDE, choix_type_bio))%>%
  unique()

# A few formatting details :
for (i in 1:length(raunkiear$choix_type_bio)) {
  raunkiear$choix_type_bio[i] <- sub("é", "e", raunkiear$choix_type_bio[i])
}

# Join data with Raunkiear plant types :
join_raunk <- inner_join(raunkiear, parts) %>%
  inner_join(uses)

join_raunk$USE <- tolower(join_raunk$USE)

names(join_raunk) <- c("CD_REF", "NOM_VALIDE", "Raunkiear_type", "Part", "Use", "Value")

# Format data for Sankey plot : 
df_raunk <- join_raunk %>%
  make_long(Use, Part, Raunkiear_type, value=Value) #here the order is important, it dictates the plotting

# Plot :
width <- 0.4

p <- ggplot(df_raunk, aes(x=x,
                          next_x=next_x,
                          node=node,
                          next_node=next_node, 
                          fill=node,
                          colour=node,
                          label=node,
                          show.legend = FALSE,
                          width=width,
                          value=value)) +
  geom_sankey(flow.alpha = .6, node.colour="gray40") +
  scale_fill_viridis_d() +
  scale_colour_viridis_d() +
  theme_sankey(base_size = 18) +
  labs(x = NULL) +
  geom_sankey_label(size = 5, color = "grey20", fill = "white") +
  guides(fill = "none") +
  theme(legend.position = "none",
        plot.title = element_text(hjust = .5))

p


# Export :  
pdf(file = "plots/Appx_P_tripartite.pdf", 
    width = 10, 
    height = 7)
print(p)
dev.off()



