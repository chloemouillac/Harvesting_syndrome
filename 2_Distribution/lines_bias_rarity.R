
### with cells containing too little obs removed
cov_df10_bias <- data.frame(matrix(nrow = 0, ncol=6))
names(cov_df10_bias) <- c("dpt_code", "dpt_name", "dpt_area", "sp_area", "sp_relative_area", "CD_REF")

# make a mask of pixels to remove for the calculations (the ones with less than 100 total obs) :
bias_rem10 <- rast("processed_data/obs_10km.tif")
plot(bias_rem10)
values(bias_rem10)
global(bias_rem10, quantile, probs=seq(0,1,0.1), na.rm=TRUE)
# 0    0    0    0    0  226  337  442  535  684  1557

for (l in 1:length(names(sp_raster10))) {
  raster <- sp_raster10[[l]]
  raster[bias_rem10<=226]<-NA ##########################... ?
  rel_cov <- calc.sp.cover(raster, departements)
  rel_cov$CD_REF <- names(raster)
  cov_df10_bias <- rbind(cov_df10_bias, rel_cov)
}

# Export :
fwrite(cov_df10_bias, "processed_data/OpenObs+GBIF_RARITY_10km_bias.csv")


### with cells containing too little obs removed
cov_df20_bias <- data.frame(matrix(nrow = 0, ncol=6))
names(cov_df20_bias) <- c("dpt_code", "dpt_name", "dpt_area", "sp_area", "sp_relative_area", "CD_REF")

# make a mask of pixels to remove for the calculations (the ones with less than 100 total obs) :
bias_rem20 <- rast("processed_data/obs_20km.tif")
global(bias_rem20, quantile, probs=seq(0,1,0.1), na.rm=TRUE)
# 0    0    0    0    0  446  601  715  842 1115.4  2112

for (l in 1:length(names(sp_raster20))) {
  raster <- sp_raster20[[l]]
  raster[bias_rem20<=446]<-NA ##########################... ?
  rel_cov <- calc.sp.cover(raster, departements)
  rel_cov$CD_REF <- names(raster)
  cov_df20_bias <- rbind(cov_df20_bias, rel_cov)
}

#Export :
fwrite(cov_df20_bias, "processed_data/OpenObs+GBIF_RARITY_20km_bias.csv")



### with cells containing too little obs removed
cov_df10_oobs_bias <- data.frame(matrix(nrow = 0, ncol=6))
names(cov_df10_oobs_bias) <- c("dpt_code", "dpt_name", "dpt_area", "sp_area", "sp_relative_area", "CD_REF")

# make a mask of pixels to remove for the calculations (the ones with less than 100 total obs) :
bias_rem10_oobs <- rast("processed_data/obs_10km_oobs.tif")
global(bias_rem10_oobs, quantile, probs=seq(0,1,0.1), na.rm=TRUE)
#  0    0    0    0    0   92 249.8  385  484  634  1532

for (l in 1:length(names(sp_raster10))) {
  raster <- sp_raster10_oobs[[l]]
  raster[bias_rem10_oobs<=92]<-NA ##########################... ?
  rel_cov <- calc.sp.cover(raster, departements)
  rel_cov$CD_REF <- names(raster)
  cov_df10_oobs_bias <- rbind(cov_df10_oobs_bias, rel_cov)
}

# Export :
fwrite(cov_df10_oobs_bias, "processed_data/OpenObs+GBIF_RARITY_10km_oobs_bias.csv")


### with cells containing too little obs removed
cov_df20_oobs_bias <- data.frame(matrix(nrow = 0, ncol=6))
names(cov_df20_oobs_bias) <- c("dpt_code", "dpt_name", "dpt_area", "sp_area", "sp_relative_area", "CD_REF")

# make a mask of pixels to remove for the calculations (the ones with less than 200 total obs) :
bias_rem20_oobs <- rast("processed_data/obs_20km_oobs.tif")
global(bias_rem20_oobs, quantile, probs=seq(0,1,0.1), na.rm=TRUE)
# 0    0    0    0    0  269 531.4  656  791 1063.7  2067

for (l in 1:length(names(sp_raster20))) {
  raster <- sp_raster20_oobs[[l]]
  raster[bias_rem20_oobs<=269]<-NA ##########################... ?
  rel_cov <- calc.sp.cover(raster, departements)
  rel_cov$CD_REF <- names(raster)
  cov_df20_oobs_bias <- rbind(cov_df20_oobs_bias, rel_cov)
}

# Export :
fwrite(cov_df20_oobs_bias, "processed_data/OpenObs+GBIF_RARITY_20km_oobs_bias.csv")


