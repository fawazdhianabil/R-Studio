library(cluster)
library(factoextra)
library(tidyverse)
library(clusterSim)
library(fpc)
library(NbClust)

data<-read_csv(file.choose())
view(data)
summary(data)


datanumerikk<-data[2:6]
view(datanumerikk)


fviz_nbclust(datanumerikk,kmeans,method = "wss", linecolor = "steelblue")

finalz=kmeans(datanumerikk,3)
print(finalz)

#mencari SSE
finalz$withinss
#mencari total SSE
finalz$tot.withinss

finalzakhir=data.frame(datanumerikk,finalz$cluster)
view(finalzakhir)

fviz_cluster(finalz, data = datanumerikk)

kmean2 <- eclust(datanumerikk, "kmeans", k = 2, nstart = 25, graph = FALSE)
kmean2_stats <- cluster.stats(dist(datanumerikk),  kmean2$cluster)
kmean3 <- eclust(datanumerikk, "kmeans", k = 3, nstart = 25, graph = FALSE)
kmean3_stats <- cluster.stats(dist(datanumerikk),  kmean3$cluster)
kmean4 <- eclust(datanumerikk, "kmeans", k = 4, nstart = 25, graph = FALSE)
kmean4_stats <- cluster.stats(dist(datanumerikk),  kmean4$cluster)
kmean5 <- eclust(datanumerikk, "kmeans", k = 5, nstart = 25, graph = FALSE)
kmean5_stats <- cluster.stats(dist(datanumerikk),  kmean5$cluster)

kmean2_stats$dunn
kmean3_stats$dunn
kmean4_stats$dunn
kmean5_stats$dunn

d <-dist(datanumerikk)
dbi_kmean2 <-print(index.DB(datanumerikk, kmean2$cluster, d, centrotypes = 'centroids'))
dbi_kmean3 <-print(index.DB(datanumerikk, kmean3$cluster, d, centrotypes = 'centroids'))
dbi_kmean4 <-print(index.DB(datanumerikk, kmean4$cluster, d, centrotypes = 'centroids'))
dbi_kmean5 <-print(index.DB(datanumerikk, kmean5$cluster, d, centrotypes = 'centroids'))

dbi_kmean2$DB
dbi_kmean3$DB
dbi_kmean4$DB
dbi_kmean5$DB

#persentaset banyak klaster
x = list()
y = list()
z = list()
for (arr in finalzakhir$finalz.cluster)
  {
  if (arr == 1){
  x <- append(x,arr)}
  else if(arr == 2){
  y <-append(y,arr)}
  else {
  z <- append(z,arr)}
}
x
y
z

#panjang tiap list
lx = length(x)
ly = length(y)
lz = length(z)

lx
ly
lz

#total klaster
total_klaster = lx+ly+lz
total_klaster

#persentase tiap klaster
byk_klaster1 = lx/total_klaster
byk_klaster2 = ly/total_klaster
byk_klaster3 = lz/total_klaster


byk_klaster1*100
byk_klaster2*100
byk_klaster3*100
