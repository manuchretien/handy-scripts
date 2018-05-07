# Conversion of spatial data: decimal to meters
# by Emmanuelle Chretien
# Sept. 27 2017

rm(list=ls())

# Set working directory
# Enter path into parentheses
setwd("")

# Load library
require(rgdal)

# Load data
data.xy<-read.csv(file.choose())

# Set existing coordinate system
# (Modify according to column names, as long as it respects order x, y)
coord.dec<-SpatialPoints(cbind(data.xy$longitude,data.xy$latitude),proj4string=CRS("+proj=longlat"))

# Transform to UTM
# In this specific case, to convert to NAD83 / UTM zone 18N
# Function uses EPSG Projection. (here: 26918) **can be found on Google easily
coord.UTM<-spTransform(coord.dec,CRS("+init=epsg:26918"))

# plot points
plot(coord.UTM, axes=TRUE, main = "positions - UTM coordinates")

# Convert SpatialPoints data to numeric 
data.xy.utm<-as.data.frame(coord.UTM)

# Add to initial dataframe
data.xy<-cbind(data.xy,data.xy.utm)

plot(coords.x2~coords.x1,data=data.xy)

# Export file
write.csv(fish.xy,"achigan.xy.new.csv")
