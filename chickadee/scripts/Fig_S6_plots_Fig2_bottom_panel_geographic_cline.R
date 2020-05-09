# This code corresponds to Fig. S6 in Alexander et al.
# It creates the geographic cline component of Fig. 2 
# in the main manuscript of Alexander et al.

# 1. Loading required libraries and scripts
library(tidyverse)
library(geosphere)
library(hzar)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping row
# corresponding to sample 99788 with low coverage
temp <- read_tsv("../data/Table_S1.txt")
temp <- temp %>% filter(Catalog_number!="99788")

# 4. Creating variables with our data of interest
mod_names <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(Catalog_number))

hist_names <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(Catalog_number))

mod_coords <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(DecimalLongitude,DecimalLatitude))

hist_coords <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(DecimalLongitude,DecimalLatitude))

mod_q.matrix <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

hist_q.matrix <-  as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

# 5. Calculating distance across the transect
# Obtaining the coordinates to create the approximate SW-NE beginning of transect line
min_long <- min(mod_coords[,1],hist_coords[,1])
max_long <- max(mod_coords[,1], hist_coords[,1])
min_lat <- min(mod_coords[,2], hist_coords[,2])
max_lat <- max(mod_coords[,2], hist_coords[,2])

# Centering the line on the SE corner of the plotting coordinates
long_diff <- max_long-min_long
lat_diff <- max_lat-min_lat

# Longitude/latitude of line as a matrix of 2 columns (first one is longitude, second is latitude) 
transect_center <- matrix(c(min_long,(min_lat-lat_diff),(max_long+long_diff),max_lat),ncol=2,byrow=T)

# Calculating distances 
mod_dist <- (dist2Line(mod_coords, transect_center, distfun=distGeo))[,1]
hist_dist <- (dist2Line(hist_coords, transect_center, distfun=distGeo))[,1]

# Setting minimum distance to zero
min_dist <- min(mod_dist,hist_dist)
mod_dist <- mod_dist-min_dist
hist_dist <- hist_dist-min_dist

# 6. Creating the modern dataframe and modeling
mod_df <- as.data.frame(cbind(mod_names,mod_dist,mod_q.matrix[,1],1))
names(mod_df) <- c("names","distance","frequency","sample_size")
mod_hzar <- hzar.doMolecularData1DPops(mod_df$distance,mod_df$frequency,mod_df$sample_size)

# Building the model, including upper constraints
mod_hzar_cline <- hzar.makeCline1DFreq(data = mod_hzar,scaling = "fixed", tails = "none", direction = NULL)
mod_hzar_cline <- hzar.model.addMaxCenter(mod_hzar_cline, max(mod_dist,hist_dist))
mod_hzar_cline <- hzar.model.addMaxWidth(mod_hzar_cline, max(mod_dist,hist_dist))

# Creating fit request model
mod_fitrequest <- hzar.first.fitRequest.old.ML(mod_hzar_cline,mod_hzar,verbose=TRUE)

# Fitting model
mod_fit <- hzar.doFit(mod_fitrequest)

# Checking fit
plot(mod_fit$mcmcRaw)
hzar.get.ML.cline(mod_fit)$param.free
hzar.getLLCutParam(mod_fit,params = c("center","width"))

# Repeating as often as necessary for stationarity
new_mod_fit <- hzar.next.fitRequest(oldFitRequest = mod_fit)
mod_fit <- hzar.doFit(new_mod_fit)
plot(mod_fit$mcmcRaw)
hzar.get.ML.cline(mod_fit)$param.free
hzar.getLLCutParam(mod_fit,params = c("center","width"))

# After being satisfied stationarity has been reached
# running second chain to verify convergence

# Creating fit request model
mod_fitrequest2 <- hzar.first.fitRequest.old.ML(mod_hzar_cline,mod_hzar,verbose=TRUE)

# Fitting model
mod_fit2 <- hzar.doFit(mod_fitrequest2)

# Checking fit
plot(mod_fit2$mcmcRaw)
hzar.get.ML.cline(mod_fit)$param.free
hzar.get.ML.cline(mod_fit2)$param.free
hzar.getLLCutParam(mod_fit,params = c("center","width"))
hzar.getLLCutParam(mod_fit2,params = c("center","width"))

# Summarizing results (from initial run) to plot
# obtaining the 95% credible interval
mod_conf <- hzar.getCredParamRed(mod_fit)
# recording the parameters of interest
mod_conf_95 <- matrix(NA,ncol=4,nrow=length(mod_conf$clines))
for (i in 1:length(mod_conf$clines)) {
  mod_conf_95[i,1] <- mod_conf$clines[[i]]$param.all$center
  mod_conf_95[i,2] <- mod_conf$clines[[i]]$param.all$width
  mod_conf_95[i,3] <- mod_conf$clines[[i]]$param.all$pMin
  mod_conf_95[i,4] <- mod_conf$clines[[i]]$param.all$pMax
}

# Summarizing the 95% CI for our params of interest
mod_center_min <- min(mod_conf_95[,1])
mod_center_max <- max(mod_conf_95[,1])
mod_width_min <- min(mod_conf_95[,2])
mod_width_max <- max(mod_conf_95[,2])

# Summarizing the best estimate of our parameters
mod_center <- hzar.get.ML.cline(mod_fit)$param.all$center
mod_width <- hzar.get.ML.cline(mod_fit)$param.all$width
mod_pMin <- hzar.get.ML.cline(mod_fit)$param.all$pMin
mod_pMax <- hzar.get.ML.cline(mod_fit)$param.all$pMax

# Generating the 95% CIs for the cline
est_freq <- function (x) {
  pMin + (pMax - pMin) * (1/(1 + exp(-((x - center) * 4/width))))
}

cline_conf <- matrix(NA,ncol=length(mod_conf$clines),nrow=ceiling(max(mod_dist,hist_dist)))

center <- mod_center
width <- mod_width
pMin <- mod_pMin
pMax <- mod_pMax

for (i in 1:length(mod_conf$clines)) {
  center <- mod_conf_95[i,1]
  width <- mod_conf_95[i,2]
  for (j in 1:ceiling(max(mod_dist,hist_dist))) {
    cline_conf[j,i] <- est_freq(j)
  }
}

# Taking the min, max, and mean at each distance
mod_conf_min_max_mean <- matrix(NA,ncol=4,nrow=ceiling(max(mod_dist,hist_dist)))
mod_conf_min_max_mean[,1] <- seq(1,ceiling(max(mod_dist,hist_dist)))

center <- mod_center
width <- mod_width
pMin <- mod_pMin
pMax <- mod_pMax

for (j in 1:ceiling(max(mod_dist,hist_dist))) {
  mod_conf_min_max_mean[j,2] <- min(cline_conf[j,])
  mod_conf_min_max_mean[j,3] <- max(cline_conf[j,])
  mod_conf_min_max_mean[j,4] <- est_freq(j)
}  

# 7. Creating the historical dataframe and modeling
hist_df <- as.data.frame(cbind(hist_names,hist_dist,hist_q.matrix[,1],1))
names(hist_df) <- c("names","distance","frequency","sample_size")
hist_hzar <- hzar.doMolecularData1DPops(hist_df$distance,hist_df$frequency,hist_df$sample_size)

# Building the model, including upper constraints
hist_hzar_cline <- hzar.makeCline1DFreq(data = hist_hzar,scaling = "fixed", tails = "none", direction = NULL)
hist_hzar_cline <- hzar.model.addMaxCenter(hist_hzar_cline, max(mod_dist,hist_dist))
hist_hzar_cline <- hzar.model.addMaxWidth(hist_hzar_cline, max(mod_dist,hist_dist))

# Creating fit request model
hist_fitrequest <- hzar.first.fitRequest.old.ML(hist_hzar_cline,hist_hzar,verbose=TRUE)

# Fitting model
hist_fit <- hzar.doFit(hist_fitrequest)

# Checking fit
plot(hist_fit$mcmcRaw)
hzar.get.ML.cline(hist_fit)$param.free
hzar.getLLCutParam(hist_fit,params = c("center","width"))

# Repeating as often as necessary for stationarity
new_hist_fit <- hzar.next.fitRequest(oldFitRequest = hist_fit)
hist_fit <- hzar.doFit(new_hist_fit)
plot(hist_fit$mcmcRaw)
hzar.get.ML.cline(hist_fit)$param.free
hzar.getLLCutParam(hist_fit,params = c("center","width"))

# After being satisfied stationarity has been reached
# running second chain to verify convergence

# Creating fit request model
hist_fitrequest2 <- hzar.first.fitRequest.old.ML(hist_hzar_cline,hist_hzar,verbose=TRUE)

# Fitting model
hist_fit2 <- hzar.doFit(hist_fitrequest2)

# Checking fit
plot(hist_fit2$mcmcRaw)
hzar.get.ML.cline(hist_fit)$param.free
hzar.get.ML.cline(hist_fit2)$param.free
hzar.getLLCutParam(hist_fit,params = c("center","width"))
hzar.getLLCutParam(hist_fit2,params = c("center","width"))

# Summarizing results (from initial run) to plot
# obtaining the 95% credible interval
hist_conf <- hzar.getCredParamRed(hist_fit)
# recording the parameters of interest
hist_conf_95 <- matrix(NA,ncol=4,nrow=length(hist_conf$clines))
for (i in 1:length(hist_conf$clines)) {
  hist_conf_95[i,1] <- hist_conf$clines[[i]]$param.all$center
  hist_conf_95[i,2] <- hist_conf$clines[[i]]$param.all$width
  hist_conf_95[i,3] <- hist_conf$clines[[i]]$param.all$pMin
  hist_conf_95[i,4] <- hist_conf$clines[[i]]$param.all$pMax
}

# Summarizing the 95% CI for our params of interest
hist_center_min <- min(hist_conf_95[,1])
hist_center_max <- max(hist_conf_95[,1])
hist_width_min <- min(hist_conf_95[,2])
hist_width_max <- max(hist_conf_95[,2])

# Summarizing the best estimate of our parameters
hist_center <- hzar.get.ML.cline(hist_fit)$param.all$center
hist_width <- hzar.get.ML.cline(hist_fit)$param.all$width
hist_pMin <- hzar.get.ML.cline(hist_fit)$param.all$pMin
hist_pMax <- hzar.get.ML.cline(hist_fit)$param.all$pMax

cline_conf <- matrix(NA,ncol=length(hist_conf$clines),nrow=ceiling(max(mod_dist,hist_dist)))

center <- hist_center
width <- hist_width
pMin <- hist_pMin
pMax <- hist_pMax

for (i in 1:length(hist_conf$clines)) {
  center <- hist_conf_95[i,1]
  width <- hist_conf_95[i,2]
  for (j in 1:ceiling(max(mod_dist,hist_dist))) {
    cline_conf[j,i] <- est_freq(j)
  }
}

# Taking the min, max, and mean at each distance
hist_conf_min_max_mean <- matrix(NA,ncol=4,nrow=ceiling(max(mod_dist,hist_dist)))
hist_conf_min_max_mean[,1] <- seq(1,ceiling(max(mod_dist,hist_dist)))

center <- hist_center
width <- hist_width
pMin <- hist_pMin
pMax <- hist_pMax

for (j in 1:ceiling(max(mod_dist,hist_dist))) {
  hist_conf_min_max_mean[j,2] <- min(cline_conf[j,])
  hist_conf_min_max_mean[j,3] <- max(cline_conf[j,])
  hist_conf_min_max_mean[j,4] <- est_freq(j)
}  

# 8. Plotting
# Getting the data frames together
mod_qmat_dist <- as.data.frame(cbind(mod_dist, mod_q.matrix[,1]))
hist_qmat_dist <- as.data.frame(cbind(hist_dist, hist_q.matrix[,1]))
names(mod_qmat_dist) <- c("dist","BC_cluster")
names(hist_qmat_dist) <- c("dist","BC_cluster")
hist_conf_min_max_mean <- as.data.frame(hist_conf_min_max_mean)
mod_conf_min_max_mean <- as.data.frame(mod_conf_min_max_mean)
names(hist_conf_min_max_mean) <- c("dist","min","max","mean")
names(mod_conf_min_max_mean) <- c("dist","min","max","mean")

ggplot() +
  geom_ribbon(data=hist_conf_min_max_mean,aes(x=dist/1000,ymax = max, ymin = min),fill="grey90",color="grey65") +
  geom_line(data=hist_conf_min_max_mean,aes(x=dist/1000,y=mean),color="grey75", size=7, alpha=0.5) +
  geom_vline(xintercept = hist_center/1000, linetype="longdash", color = "grey75", size=7, alpha=0.5) +
  geom_vline(xintercept = hist_center_min/1000, color = "grey75", size=5, alpha=0.5) +
  geom_vline(xintercept = hist_center_max/1000, color = "grey75", size=5, alpha=0.5) +
  geom_ribbon(data=mod_conf_min_max_mean,aes(x=dist/1000,ymax = max, ymin = min),fill="grey40",color="grey15") +
  geom_line(data=mod_conf_min_max_mean,aes(x=dist/1000,y=mean),color="grey25", size=7, alpha=0.5) +
    geom_vline(xintercept = mod_center/1000, linetype="longdash", color = "grey25", size=7, alpha=0.5) +
  geom_vline(xintercept = mod_center_min/1000, color = "grey25", size=5, alpha=0.5) +
  geom_vline(xintercept = mod_center_max/1000, color = "grey25", size=5, alpha=0.5) +
    geom_point(data=mod_qmat_dist,aes(x = dist/1000, y = BC_cluster),shape=21,color="black", fill="black", size=30) +
  geom_point(data=hist_qmat_dist,aes(x = dist/1000, y = BC_cluster),shape=21,color="black", fill="white", size=30) + theme_bw(base_size=66) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title=element_text(size=80,face="bold")) +
  scale_y_continuous(name="Assignment to BC genetic cluster") +
  scale_x_continuous(name="Distance along transect (km)") 

# Saved manually as a plot 4000 pixels wide * 2000 pixels wall
# Fig_2_transect.png

# 9. Printing to screen some parameters of interest to report in manuscript
results <- as_tibble(rbind(c("Center",hist_center/1000,mod_center/1000),
c("min 95% CI Center",hist_center_min/1000,mod_center_min/1000),
c("max 95% CI Center",hist_center_max/1000,mod_center_max/1000),
c("Width",hist_width/1000,mod_width/1000),
c("min 95% CI Width",hist_width_min/1000,mod_width_min/1000),
c("max 95% CI Width",hist_width_max/1000,mod_width_max/1000),
c("pMin",hist_pMin,mod_pMin),
c("pMax",hist_pMax,mod_pMax)))

names(results) <- c("Parameters","Historical","Modern")

results
print("All distances in km")

sessionInfo()
#R version 3.6.2 (2019-12-12)
#Platform: x86_64-apple-darwin15.6.0 (64-bit)
#Running under: macOS Sierra 10.12.6

#Matrix products: default
#BLAS:   /System/Library/Frameworks/Accelerate.framework/Versions/A/Frameworks/vecLib.framework/Versions/A/libBLAS.dylib
#LAPACK: /Library/Frameworks/R.framework/Versions/3.6/Resources/lib/libRlapack.dylib

#locale:
#  [1] en_US.UTF-8/en_US.UTF-8/en_US.UTF-8/C/en_US.UTF-8/en_US.UTF-8

#attached base packages:
#  [1] grid      stats     graphics  grDevices utils     datasets 
#[7] methods   base     

#other attached packages:
#  [1] hzar_0.2-5         foreach_1.4.8      MCMCpack_1.4-6    
#[4] MASS_7.3-51.5      coda_0.19-3        geosphere_1.5-10  
#[7] fields_10.3        maps_3.3.0         spam_2.5-1        
#[10] dotCall64_1.0-0    RColorBrewer_1.1-2 tess3r_1.1.0      
#[13] forcats_0.4.0      stringr_1.4.0      purrr_0.3.3       
#[16] tidyr_1.0.2        tibble_2.1.3       tidyverse_1.3.0   
#[19] ggrepel_0.8.1      ggmap_3.0.0        scatterpie_0.1.4  
#[22] ggplot2_3.2.1      dplyr_0.8.4        readr_1.3.1       

#loaded via a namespace (and not attached):
#  [1] nlme_3.1-144        mcmc_0.9-6.1        bitops_1.0-6       
#[4] fs_1.3.1            lubridate_1.7.4     httr_1.4.1         
#[7] tools_3.6.2         backports_1.1.5     utf8_1.1.4         
#[10] R6_2.4.1            DBI_1.1.0           lazyeval_0.2.2     
#[13] colorspace_1.4-1    withr_2.1.2         sp_1.3-2           
#[16] tidyselect_1.0.0    curl_4.3            compiler_3.6.2     
#[19] cli_2.0.1           rvest_0.3.5         quantreg_5.54      
#[22] SparseM_1.78        xml2_1.2.2          labeling_0.3       
#[25] scales_1.1.0        digest_0.6.24       jpeg_0.1-8.1       
#[28] pkgconfig_2.0.3     dbplyr_1.4.2        rlang_0.4.4        
#[31] readxl_1.3.1        rstudioapi_0.11     farver_2.0.3       
#[34] generics_0.0.2      jsonlite_1.6.1      magrittr_1.5       
#[37] Matrix_1.2-18       Rcpp_1.0.3          munsell_0.5.0      
#[40] fansi_0.4.1         lifecycle_0.1.0     stringi_1.4.5      
#[43] plyr_1.8.5          crayon_1.3.4        lattice_0.20-38    
#[46] haven_2.2.0         hms_0.5.3           pillar_1.4.3       
#[49] rjson_0.2.20        codetools_0.2-16    reprex_0.3.0       
#[52] glue_1.3.1          BiocManager_1.30.10 modelr_0.1.5       
#[55] png_0.1-7           vctrs_0.2.2         tweenr_1.0.1       
#[58] RgoogleMaps_1.4.5.3 MatrixModels_0.4-1  cellranger_1.1.0   
#[61] gtable_0.3.0        polyclip_1.10-0     assertthat_0.2.1   
#[64] ggforce_0.3.1       broom_0.5.4         RcppEigen_0.3.3.7.0
#[67] iterators_1.0.12    rvcheck_0.1.7       ellipsis_0.3.0 
