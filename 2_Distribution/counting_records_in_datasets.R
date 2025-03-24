directory <- here::here("2_Distribution")
setwd(directory)

d1 <- read.csv("processed_data/OpenObs_10km.csv")
library(dplyr)

toMatch <- c("CEN ", 
             "CBN",
             "FCBN",
             "Relevés floristiques du protocole de l'Inventaire National Forestier de l'IGN",
             "ONF - Observations opportunistes et inventaires")

for (i in toMatch) {
  d1$match[grepl(i,d1$libelleJeuDonnees)] <- i
}

d1_summar <- d1 %>%
  group_by(match)%>%
  summarise(count=n())


d2 <- read.csv("processed_data/GBIF_clean_10km.csv")

28724130 -22027635

d3 <- read.csv("processed_data/OpenObs+GBIF_10km_NOT_REDUCED.csv")

22027635+20984826


d4 <- read.csv("processed_data/OpenObs+GBIF_REDUCED_20km.csv")

