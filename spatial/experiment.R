library(geosphere)
library(sp)
library(rgdal)

Coordinates<-SpatialPoints(data[, c("lon","lat")])#extracting coordinates
spatialData<-SpatialPointsDataFrame(Coordinates, crimeData)
proj4string(spatialData)=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")

#identifying clusters
## use the distm function to generate a geodesic distance matrix in meters
mdist <- distm(spatialData)

# cluster all points using a hierarchical clustering approach
hc <- hclust(as.dist(mdist), method="complete")


# define the distance threshold, in this case 40 m
d=2000

# define clusters based on a tree "height" cutoff "d" and add them to the SpDataFrame
spatialData$clust <- cutree(hc, h=d)

library(dismo)
library(rgeos)

# expand the extent of plotting frame
spatialData@bbox[] <- as.matrix(extend(extent(spatialData),0.001))

# get the centroid coords for each cluster
cent <- matrix(ncol=2, nrow=max(spatialData$clust))
for (i in 1:max(spatialData$clust))
  # gCentroid from the rgeos package
  cent[i,] <- gCentroid(subset(spatialData, clust == i))@coords

# compute circles around the centroid coords using a 40m radius
# from the dismo package
ci <- circles(cent, d=d, lonlat=T)

# plot
plot(ci@polygons, axes=T)
plot(spatialData, col=rainbow(4)[factor(spatialData$clust)], add=T)
