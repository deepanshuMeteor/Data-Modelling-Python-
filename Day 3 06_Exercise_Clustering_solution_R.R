# k-means clustering

library(cluster)
library(scatterplot3d)
library(rgl)

# set the work directory
setwd('C:\\EK\\work_data')

#import the data (student grades)
grade_data <- read.csv('grades_km_input.csv')

# the first column is not needed
kmdata <- grade_data[,2:4]
head(kmdata)

# 3D plot of the data
scatterplot3d(kmdata)

# perform k-means clustering for k = 1:15 and store WSS for each run
WSS <- rep(0,15) 
for (k in 1:15) WSS[k] <- sum(kmeans(kmdata, centers=k, nstart=25)$withinss)

# plot WSS for each k and select the best (optimum) k
plot(1:15, WSS, type="b", xlab="Number of Clusters", ylab="WSS") 
# the elbow point is for k = 3

km <- kmeans(kmdata,3, nstart=25)
km

# 3D plot of the clusters
scatterplot3d(kmdata, color = km$cluster)

# interactive 3D plot of the clusters (can expand and rotate)
plot3d( 
  x=kmdata$English, y=kmdata$Math, z=kmdata$Science, 
  col = km$cluster, 
  type = 's', 
  radius = 0.5,
  xlab="English", ylab="Math", zlab="Science")
