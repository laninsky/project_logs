# 1. Loading required libraries and scripts
library(tidyverse)
library(geosphere)
library(hzar)

# 2. Setwd
setwd("chickadee/output/")

# 3. Reading in data (tab delimited), dropping last blank row
temp <- read_tsv("../data/S2.txt")
temp <- temp[1:165,]

# 4. Creating variables with our data of interest
mod_names <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(Catalog_number))

hist_names <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(Catalog_number))

mod_coords <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(DecimalLongitude,DecimalLatitude))

hist_coords <- as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(DecimalLongitude,DecimalLatitude))

mod_q.matrix <- as.matrix(temp %>% filter(Sampling_period=="MODERN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

hist_q.matrix <-  as.matrix(temp %>% filter(Sampling_period=="SMITHSONIAN") %>% filter(Included_in_tess3r=="YES") %>% dplyr::select(BC_genetic_cluster_assignment,CC_genetic_cluster_assignment))

# 5. Calculating distance across the transect
# Obtaining the coordinates to create the approximate SW-NE center of transect line
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
  mod_pMin + (mod_pMax - mod_pMin) * (1/(1 + exp(-((x - center) * 4/width))))
}

cline_conf <- matrix(NA,ncol=length(mod_conf$clines),nrow=ceiling(max(mod_dist,hist_dist)))

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
  geom_line(data=hist_conf_min_max_mean,aes(x=dist/1000,y=mean),color="grey75", size=1.5, alpha=0.5) +
  geom_vline(xintercept = hist_center/1000, linetype="longdash", color = "grey75", size=1.5, alpha=0.5) +
  geom_vline(xintercept = hist_center_min/1000, color = "grey75", size=1.0, alpha=0.5) +
  geom_vline(xintercept = hist_center_max/1000, color = "grey75", size=1.0, alpha=0.5) +
  geom_ribbon(data=mod_conf_min_max_mean,aes(x=dist/1000,ymax = max, ymin = min),fill="grey40",color="grey15") +
  geom_line(data=mod_conf_min_max_mean,aes(x=dist/1000,y=mean),color="grey25", size=1.5, alpha=0.5) +
    geom_vline(xintercept = mod_center/1000, linetype="longdash", color = "grey25", size=1.5, alpha=0.5) +
  geom_vline(xintercept = mod_center_min/1000, color = "grey25", size=1.0, alpha=0.5) +
  geom_vline(xintercept = mod_center_max/1000, color = "grey25", size=1.0, alpha=0.5) +
    geom_point(data=mod_qmat_dist,aes(x = dist/1000, y = BC_cluster),shape=21,color="black", fill="black", size=6) +
  geom_point(data=hist_qmat_dist,aes(x = dist/1000, y = BC_cluster),shape=21,color="black", fill="white", size=6) + theme_bw(base_size=20) +
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank()) +
  theme(axis.title=element_text(size=24,face="bold")) +
  scale_y_continuous(name="Assignment to BC genetic cluster") +
  scale_x_continuous(name="Distance along transect (km)") 

# Saved manually as a plot 1000 pixels wide, Fig_SX_transect.png