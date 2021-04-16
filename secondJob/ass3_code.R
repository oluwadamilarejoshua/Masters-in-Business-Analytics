library(cluster)
library(fpc)
library(tidyverse)
library(factoextra)
library(tseries)

# theData <- write.csv(wholesale_customers.19052471, file = "assData.csv")

wholesale_customers.19052471 <- read.csv("assData.csv")
# K-MEAN CLUSTERING ANALYSIS ----------------------------------------------

# Preparation of data to be used for first task

firstObj_data <- wholesale_customers.19052471[, -c(1:3)]
summary(firstObj_data)
dim(firstObj_data)
plot(firstObj_data)   # Checking relationships between pairs of variables

table(is.na(firstObj_data)) # Checking for missing values
head(firstObj_data)

firstObj_data <- firstObj_data %>% 
  transmute(
    Fresh <- as.numeric(Fresh),
    Milk <- as.numeric(Milk),
    Grocery <- as.numeric(Grocery),
    Frozen <- as.numeric(Frozen),
    DetergentsAndPaper <- as.numeric(DetergentsAndPaper),
    Delicatessen <- as.numeric(Delicatessen)
  )

kmeanClust2_result <- kmeans(scale(firstObj_data), centers = 2) # 2 centroids
kmeanClust2_result$tot.withinss
kmeanClust2_result$size
kmeanClust2_result$centers

kmeanClust3_result <- kmeans(scale(firstObj_data), centers = 3) # 3 centroids
kmeanClust3_result$tot.withinss
kmeanClust3_result$size
kmeanClust3_result$centers


# Loop for iterating over a set of centroids

sse_vector <- vector()
for (i in 2:15) {
  kmeanClust <- kmeans(scale(firstObj_data), centers = i)
  sse_vector[i] <- kmeanClust$tot.withinss
}

length(sse_vector)
sse_vector

index_ <- c(1:15)

theSSW <- as.data.frame(cbind(index_, sse_vector))
t(theSSW) %>%
  write.csv(., file = "SSW_vector.csv")

theSSE <- as.data.frame(cbind(index_[-15], diff(sse_vector)))
colnames(theSSE) <- c("index", "SSE")
t(theSSE) %>%
  write.csv(., file = "SSE_vector.csv")

 
# The optimal model
kmeanClust6_result <- kmeans(scale(firstObj_data), centers = 6)
kmean6_centers <- kmeanClust6_result$centers
write.csv(kmean6_centers, file = "optimalClustCenters.csv")

aggrG_ <- aggregate(firstObj_data, by = list(cluster = kmeanClust6_result$cluster), mean)
write.csv(aggrG_, file = "aggregated.csv")

plotcluster(firstObj_data, kmeanClust6_result$cluster)


# K-MEDIOD CLUSTERING ANALYSIS --------------------------------------------

# Preparation of data for the second task

Sector <- wholesale_customers.19052471$Sector
secondObj_data <- data.frame(cbind(Sector, firstObj_data))
str(secondObj_data)

optSelDat_ <- secondObj_data %>% 
  transmute(
    Fresh <- as.numeric(Fresh),
    Milk <- as.numeric(Milk),
    Grocery <- as.numeric(Grocery),
    Frozen <- as.numeric(Frozen),
    DetergentsAndPaper <- as.numeric(DetergentsAndPaper),
    Delicatessen <- as.numeric(Delicatessen)
  )
scaleddata2 <- scale(optSelDat_)
fviz_nbclust(x = scaleddata2, pam, method = "wss")

kmed5 <- pam(secondObj_data, k = 5)
clusterInfo <- kmed5$clusinfo
write.csv(clusterInfo, file = "kmedoidsClusterInfo.csv")

# aggrG_medoids <- aggregate(scaleddata2, by = list(cluster = kmed5$medoids), mean)

fviz_cluster(kmed5, data = secondObj_data)


