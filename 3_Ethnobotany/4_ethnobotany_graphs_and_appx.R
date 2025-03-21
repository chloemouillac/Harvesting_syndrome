# This script is to produce graphs illustrating the uses and harvested parts of wild-harvested plants.

# Set working directory :
setwd("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/")

# Import packages :
library(ggplot2)
# devtools::install_github("davidsjoberg/ggsankey")
library(ggnewscale)
library(ggsankey)
library(reshape2)
library(dplyr)
library(here)
library(ggrepel)
library(stringr)

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
vascular_list <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/list_vascular_v17.csv" )%>%
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

# Export :
write.csv(join_uses_vascular_summar, "processed_data/uses_percentages.csv", row.names=F)



#### 10 families - End-uses of the most harvested plant families plot #####
# 10 most harvested plant families :
most_harv <-  c("Lamiaceae", "Ranunculaceae", "Caprifoliaceae", "Boraginaceae", "Ericaceae", "Pinaceae", "Violaceae", "Polygonaceae", "Asparagaceae", "Crassulaceae")
most_harv <- factor(most_harv, levels= c("Lamiaceae", "Ranunculaceae", "Caprifoliaceae", "Boraginaceae", "Ericaceae", "Pinaceae", "Violaceae", "Polygonaceae", "Asparagaceae", "Crassulaceae"))
# Ordered by highest number of WHP species to lowest, and when equal number, the highest percentage of WHP goes first



#### First plot : nb of sp on VS not on list ####
# Calculate percentage of species on and not on list : 
pct_on_harv_list <- join_uses_vascular
pct_on_harv_list$list[pct_on_harv_list$USE=="No recorded use"] <- "No"
pct_on_harv_list$list[pct_on_harv_list$USE!="No recorded use"] <- "Yes" #On commercial harvesting lists
pct_on_harv_list <- pct_on_harv_list %>%
  select(LB_NOM, list, FAMILLE) %>%
  unique() %>%
  group_by(FAMILLE, list) %>%
  summarise(n=n()) %>%
  na.omit()


for (i in unique(pct_on_harv_list$FAMILLE)) {
  pct_on_harv_list$percentage[pct_on_harv_list$FAMILLE==i & 
                                pct_on_harv_list$list=="Yes"] <- 100*
    pct_on_harv_list$n[pct_on_harv_list$FAMILLE==i & pct_on_harv_list$list=="Yes"]/
    sum(pct_on_harv_list$n[pct_on_harv_list$FAMILLE==i])
  
  pct_on_harv_list$percentage[pct_on_harv_list$FAMILLE==i & 
                                pct_on_harv_list$list=="No"] <- 100*
    pct_on_harv_list$n[pct_on_harv_list$FAMILLE==i & pct_on_harv_list$list=="No"]/
    sum(pct_on_harv_list$n[pct_on_harv_list$FAMILLE==i])
}


pct_on_harv_list_10fam <- pct_on_harv_list[pct_on_harv_list$FAMILLE %in% most_harv,]

# Calculate the mean % of each family which is on the commercial list
ALL_list <- pct_on_harv_list %>%
  group_by(list) %>%
  summarise(n=sum(n))

ALL_list$percentage <- 100*ALL_list$n/sum(ALL_list$n)

ALL_list$FAMILLE <- "ALL"

pct_on_harv_list_10fam <- rbind(pct_on_harv_list_10fam, ALL_list)
pct_on_harv_list_10fam$percentage <- round(pct_on_harv_list_10fam$percentage, 1)

pct_on_harv_list_10fam$FAMILLE <- factor(pct_on_harv_list_10fam$FAMILLE, c("ALL", "Lamiaceae", "Ranunculaceae", "Caprifoliaceae", "Boraginaceae", "Ericaceae", "Pinaceae", "Violaceae", "Polygonaceae", "Asparagaceae", "Crassulaceae"))



# Plot :
plot1 <- ggplot(pct_on_harv_list_10fam[pct_on_harv_list_10fam$list=="Yes"&
                                         pct_on_harv_list_10fam$FAMILLE!="ALL",], aes(x=FAMILLE,
                                                                                      y=percentage, width=0.7)) +
  geom_col(fill="grey60") +
  
  scale_y_continuous(breaks = seq(0,40,10), limits = c(0, 48),
                     labels= seq(0,40,10)) +
  
  geom_hline(yintercept=pct_on_harv_list_10fam$percentage[pct_on_harv_list_10fam$list=="Yes" &
                                                            pct_on_harv_list_10fam$FAMILLE=="ALL"],
             colour="black") +
  
  annotate("text", x = 0.5,
                y = 2 + pct_on_harv_list_10fam$percentage[pct_on_harv_list_10fam$list=="Yes" &
                                                            pct_on_harv_list_10fam$FAMILLE=="ALL"],
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



#### Second plot ####
uses_per_fam <- join_uses_vascular_summar[join_uses_vascular_summar$FAMILLE %in% most_harv,]

# Calculate the mean % of each use over all families and add the data to uses_per_fam
ALL <- join_uses_vascular_summar %>%
  group_by(USE) %>%
  summarise(weight=sum(weight),
            nb_species=sum(nb_species))
ALL$percentage[ALL$USE!="No recorded use"] <- ALL$nb_species[ALL$USE!="No recorded use"]/
  sum(ALL$nb_species[ALL$USE!="No recorded use"])
ALL$percentage[ALL$USE=="No recorded use"] <- ALL$nb_species[ALL$USE=="No recorded use"]/sum(ALL$nb_species)
ALL$FAMILLE<- "ALL"
uses_per_fam <- rbind(uses_per_fam, ALL)
#

uses_per_fam$percentage <- round(uses_per_fam$percentage,3)*100
uses_per_fam$FAMILLE <- factor(uses_per_fam$FAMILLE, levels= c("ALL", "Lamiaceae", "Ranunculaceae", "Caprifoliaceae", "Boraginaceae", "Ericaceae", "Pinaceae", "Violaceae", "Polygonaceae", "Asparagaceae", "Crassulaceae"))

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
    pct_on_harv_list_10fam$n[pct_on_harv_list_10fam$FAMILLE==fam &
                                   pct_on_harv_list_10fam$list=="Yes"]
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



# Export :  
# png(file = "plots/uses_10fam.png", 
#     width = 2800, 
#     height = 4000,
#     res = 300)
# 
# ggpubr::ggarrange(print(plot1), print(plot2), align="v",
#                   nrow=2, labels=c("a", "b"), heights=c(0.5,1),
#                   font.label = list(size = 25))
# dev.off()

pdf(file = "plots/uses_10fam.pdf",
    width = 10,
    height = 15)

ggpubr::ggarrange(print(plot1), print(plot2), align="v",
                  nrow=2, labels=c("a", "b"), heights=c(0.5,1),
                  font.label = list(size = 25))
dev.off()


#### Density of nb of species / family ####
nb_harv_per_fam <- pct_on_harv_list %>%
  subset(list=="Yes", select=c("FAMILLE", "n"))


ggplot(nb_harv_per_fam, aes(x=reorder(FAMILLE, n, decreasing=T), y=n)) +
  geom_col() +
  ylab("Number of harvested species in the family")+
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1),
        axis.title.x=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())



#### Percentage of each part used ####
summar_parts <- parts %>%
  group_by(PART) %>%
  summarise(percentage=100* n() / length(parts$LB_NOM))

summar_parts <- parts %>%
  group_by(PART) %>%
  summarise(percentage=n())


#### Create a tripartite network ####
# Import Raunkiaer data :
raunkiear <- read.csv("Harvesting_syndrome/1-Bilan_cueillette/R/Paper_WHP/4_Plant-life_history/processed_data/Raunkieaer_data_REVIEWED_WHP.csv") %>%
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
# png(file = "plots/tripartite.png", 
#     width = 3000, 
#     height = 2000,
#     res = 300)
# print(p)
# dev.off()


pdf(file = "plots/tripartite.pdf", 
    width = 10, 
    height = 7)
print(p)
dev.off()





