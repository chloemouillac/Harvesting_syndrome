# This script is to identify the most 'the 10 most ‘overutilised’ families 
# (those containing more harvested species than expected)
# using Chi-squared analysis and focusing on families with more than 10 harvested species

# Set working directory :
setwd("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/3_Ethnobotany/")


# Import packages :
library(dplyr)


#### Import data ####
data <- read.csv("/home/mouillac/Documents/1-Bilan_cueillette/R/Paper_WHP/1_Phylogeny/processed_data/tip_data_for_tree.csv") %>%
  select(FAMILLE,number_harvested,total_sp)


####  Filter families with more than 10 harvested species #### 
filtered_data <- data %>%
  filter(number_harvested > 10)

####  Create a contingency table #### 
contingency_table <- data.frame(
  harvested = filtered_data$number_harvested,
  not_harvested = filtered_data$total_sp - filtered_data$number_harvested,
  row.names = filtered_data$FAMILLE
)

####  Perform the Chi-squared test #### 
chi2_results <- chisq.test(contingency_table)

# Extract expected values
expected_harvested <- chi2_results$expected[,1]

# Calculate standardized residuals
std_residuals <- (contingency_table$harvested - expected_harvested) / sqrt(expected_harvested)

# Add expected values and residuals to the data
filtered_data <- filtered_data %>%
  mutate(expected_harvested = expected_harvested, std_residuals = std_residuals)

# Identify the 10 most overutlised families
over_harvested <- filtered_data %>%
  arrange(desc(std_residuals)) %>%  # Sort by highest residuals
  slice_head(n = 10)  # Select top 10 families


print(over_harvested)

#Lamiaceae, Pinaceae, Ericaceae, Violaceae, Ranunculaceae, Caprifoliaceae, Crassulaceae, Polygonaceae, Rosaceae, Boraginaceae



### Export data ###
write.csv(filtered_data, "processed_data/overutilised_families.csv")
