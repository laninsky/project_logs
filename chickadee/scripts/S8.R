# TO DO...
# 1. Loading necessary libraries
library(ggmap)
library(ggrepel)

# 2. Reading in modern and historical data
mymarkers <- read.table("modern_avhybrid_latlong.txt",header=TRUE)
mymarkers <- rbind(mymarkers,read.table("historical_avhybrid_latlong.txt",header=TRUE))

# 3. Making a box containing all sites in both modern and historical files
sbbox <- make_bbox(lon=mymarkers$lon, lat=mymarkers$lat,f=1)

# 4. Using the sbbox object to retrieve a map covering the sample sites
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")

# 5. Re-reading in modern data only
mymarkers <- read.table("modern_avhybrid_latlong.txt",header=TRUE)

# 6. Plotting modern locations based on their hybrid_status
modern <- ggmap(sq_map) + geom_point(data = mymarkers, mapping = aes(x = lon, y = lat,fill = hybrid_status), shape=21,color = "black",size=24)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF"))
# 7. Exported plot at 2000 vertical pixels in sizing

# 8. Repeating steps 5-7 for historical data
mymarkers <- read.table("historical_avhybrid_latlong.txt",header=TRUE)
historical <- ggmap(sq_map) + geom_point(data = mymarkers, mapping = aes(x = lon, y = lat,fill = hybrid_status), shape=21,color = "black",size=24)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF"))

# 9. Replotting with Location as labels for points for transferring to
# hand-annotation
mymarkers <- read.table("modern_avhybrid_latlong.txt",header=TRUE)
modern <- ggmap(sq_map) + geom_point(data = mymarkers, mapping = aes(x = lon, y = lat,fill = hybrid_status), shape=21,color = "black",size=4)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF"))+geom_text_repel(data=mymarkers,aes(label = Location, color= hybrid_status))+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))

mymarkers <- read.table("historical_avhybrid_latlong.txt",header=TRUE)
historical <- ggmap(sq_map) + geom_point(data = mymarkers, mapping = aes(x = lon, y = lat,fill = hybrid_status), shape=21,color = "black",size=4)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF"))+geom_text_repel(data=mymarkers,aes(label = Location, color= hybrid_status))+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))

# 10. For densely sampled areas, creating maps of a subset of sites allowed
# them to be more clearly displayed. The following example is for a subset 
# of the following sites: 21, 24, 29, 30, 32, 33, 36
subset <- c(21,24,29,30,32,33,36)

# 11. The following creates a box slightly larger than the spatial extent
# of the subsetted locations, and retrieves the map for that box
mymarkers <- read.table("modern_avhybrid_latlong.txt",header=TRUE)

lonextbox <- c(mymarkers[subset,]$lon, mymarkers[subset,]$lon+0.01, mymarkers[subset,]$lon-0.01)
latextbox <- c(mymarkers[subset,]$lat, mymarkers[subset,]$lat+0.01, mymarkers[subset,]$lat-0.01) 
sbbox <- make_bbox(lon=lonextbox, lat=latextbox,f=1)
sq_map <- get_map(location = sbbox, maptype = "terrain", source = "google")

# 12. Creating a map displaying the subsetted locations.
modern <- ggmap(sq_map) + geom_point(data = mymarkers[subset,], mapping = aes(x = lon, y = lat,fill = hybrid_status), shape=21,color = "black",size=24)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF"))
# Note: if sampling site class not present (e.g. black-capped, Carolina,
# hybrid/mixed), then remove color from scale_fill_manual values
# These plots were exported at 2000 vertical pixels in sizing

# 13. To_find_the_label for the subsetted locations.
modern <- ggmap(sq_map) + geom_point(data = mymarkers[subset,], mapping = aes(x = lon, y = lat,fill = hybrid_status), shape=21,color = "black",size=4)+scale_fill_manual(values=c("#CE1B26", "#15326C","#9437FF"))+geom_text_repel(data=mymarkers[subset,],aes(label = Location, color= hybrid_status))+scale_color_manual(values=c("#CE1B26", "#15326C","#9437FF"))
