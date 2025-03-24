# This script is to produce graphs illustrating the life-history of wild-harvested plants.


# Set working directory :
directory <- here::here("4_Plant-life_history")
setwd(directory)

# Import packages :
library(dplyr)
library(ggplot2)


# Import data :
list_species <- read.csv(here::here("WHP_correpondence_table_v17.csv"))

CSR_data_WHP <- read.csv("processed_data/StrateFy_CSR_clean_output_WHP.csv") %>%
  na.omit() %>%
  unique()

CSR_data_ALL <- read.csv("processed_data/StrateFy_CSR_clean_output_SELECTED_ALL_FRANCE.csv")


CSR_data_WHP$csr_simple <- str_split_i(CSR_data_WHP$strategy_class, pattern="/", i=1)
CSR_data_ALL$csr_simple <- str_split_i(CSR_data_ALL$strategy_class, pattern="/", i=1)



###### Descr : CSR ######
csr_freq_WHP <- as.data.frame(table(CSR_data_WHP$csr_simple))

ggplot(csr_freq_WHP, aes(x=reorder(Var1, -Freq), y=100*Freq/sum(Freq))) +
  geom_col(fill="skyblue") +
  labs(y= "Proportion of species", x = "CSR category") +
  theme_minimal(base_size = 20)


csr_freq_ALL <- as.data.frame(table(CSR_data_ALL$csr_simple))

ggplot(csr_freq_ALL, aes(x=reorder(Var1, -Freq), y=100*Freq/sum(Freq))) +
  geom_col(fill="skyblue") +
  labs(y= "Proportion of species", x = "CSR category") +
  theme_minimal(base_size = 20)



###### Stats ######
# summary(CSR_data_WHP) : wild harvested plants
# species_binomial    AccSpeciesID          C               S                R              CSR            strategy_class      csr_simple       
# Length:585         Min.   :    29   Min.   : 0.00   Min.   :  0.00   Min.   :  0.00   Length:585         Length:585         Length:585        
# Class :character   1st Qu.: 17034   1st Qu.:15.67   1st Qu.:  3.19   1st Qu.: 16.66   Class :character   Class :character   Class :character  
# Mode  :character   Median : 33806   Median :30.32   Median : 26.10   Median : 32.80   Mode  :character   Mode  :character   Mode  :character  
# Mean   : 41101   Mean   :32.61   Mean   : 31.77   Mean   : 35.62                                                           
# 3rd Qu.: 47558   3rd Qu.:47.08   3rd Qu.: 51.92   3rd Qu.: 52.70                                                           
# Max.   :459576   Max.   :89.78   Max.   :100.00   Max.   :100.00          

# summary(CSR_data_ALL) : all of the French vascular flora
# species_binomial    AccSpeciesID          C               S                 R              CSR            strategy_class      csr_simple       
# Length:1104        Min.   :    50   Min.   : 0.00   Min.   :  0.000   Min.   :  0.00   Length:1104        Length:1104        Length:1104       
# Class :character   1st Qu.: 15176   1st Qu.:11.78   1st Qu.:  2.281   1st Qu.: 15.85   Class :character   Class :character   Class :character  
# Mode  :character   Median : 34636   Median :26.52   Median : 31.835   Median : 33.02   Mode  :character   Mode  :character   Mode  :character  
# Mean   : 41644   Mean   :28.86   Mean   : 35.053   Mean   : 36.08                                                           
# 3rd Qu.: 47985   3rd Qu.:43.52   3rd Qu.: 57.897   3rd Qu.: 53.03                                                           
# Max.   :455707   Max.   :92.08   Max.   :100.000   Max.   :100.00   


# 1. Vérification de la normalité
shapiro.test(CSR_data_WHP$C) #p-value = 4.742e-10
shapiro.test(CSR_data_ALL$C) #p-value < 2.2e-16
shapiro.test(CSR_data_WHP$S) #p-value < 2.2e-16
shapiro.test(CSR_data_ALL$S) #p-value < 2.2e-16
shapiro.test(CSR_data_WHP$R) #p-value = 1.821e-10
shapiro.test(CSR_data_ALL$R) #p-value < 2.2e-16

# Pour les tests de Shapiro-Wilk, un p-value > 0.05 indique que les données suivent une distribution normale.
# Pas de distribution normale ici !

# Si les données ne sont pas normales, utiliser le test de Mann-Whitney
wilcox.test(CSR_data_WHP$C, CSR_data_ALL$C) #p-value = 0.0007242
wilcox.test(CSR_data_WHP$S, CSR_data_ALL$S) #p-value = 0.06943
wilcox.test(CSR_data_WHP$R, CSR_data_ALL$R) #p-value = 0.9102

#Pour les tests t et de Mann-Whitney, un p-value < 0.05 indique une différence significative entre les moyennes des deux groupes.
# On observe une différence significative pour la stratégie C (les espèces cueillies sont globalament moins compétitives)

summary(CSR_data_WHP)



# Plot les distributions :
# ggplot(CSR_data_ALL, aes(x=C)) +
#   geom_density()




# Comparer les CSR plus détaillés :
# Supposons que vos deux jeux de données soient sous forme de data frames : 
# data1 et data2, avec une colonne "CSR" contenant les valeurs "C", "S", ou "R"

# Ajoutez une colonne pour identifier le jeu de données
CSR_data_WHP$group <- "harv"
CSR_data_ALL$group <- "FR"

# Combinez les deux jeux de données
combined_data <- rbind(CSR_data_WHP, CSR_data_ALL)

# Créez une table de contingence
table_contingency <- table(combined_data$group, combined_data$CSR)
print(table_contingency)


# Test du chi-carré
#Chi-squared test: Can be used for large sample sizes and when the expected frequency is at least 5 for each cell in the contingency table.

chi2_test <- chisq.test(table_contingency)
print(chi2_test) #p-value = 0.9983

#p-value > 0.05 : Indique qu'il n'y a pas de différence significative entre les distributions des catégories CSR des deux jeux de données.
