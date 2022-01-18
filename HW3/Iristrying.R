library(ggplot2)
library(dplyr)
library(factoextra)
library(cluster)
library(datasets)

data(iris)
iris
set.seed(102)
       
ggplot(iris, mapping = aes(x = iris$Sepal.Width, y = iris$Sepal.Length, color = iris$Species))+
         geom_point() +
         labs(title = "Sepal Width Vs Sepal Length",
              x = "Sepal Width",
              y = "Sepal Length")

pca_iris <-  prcomp(iris[1:4],scale =T,center=T)
summary(pca_iris)

fviz_pca_ind(pca_iris,geom.ind = "point", pointshape = 21,
             pointsize = 2, 
             fill.ind = iris$Species,
             col.ind = "black", 
             palette = "jco",
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title ="Species")+
  ggtitle("2D PCA-plot from 4 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))


is.factor(iris$Species)
kmeans_clustering <- kmeans(iris[1:4],centers=10,algorithm = "Lloyd")
kmeans_clustering$cluster
cluster <- data.frame(iris,cluster=as.factor(kmeans_clustering$cluster))
cluster[5:6]

BSS3 <- kmeans_clustering$betweenss
TSS3 <- kmeans_clustering$totss
BSS3 / TSS3 * 100

library(factoextra)
library(NbClust)


fviz_nbclust(iris.X, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 3,color="red") + 
  labs(subtitle = "Elbow method")



fviz_cluster(kmeans_clustering, data = iris[,-5],
             palette = c("red", "green", "blue"),
             geom = "point",
             ellipse.type = "convex")

distances <-  dist(iris[,-5],method = "euclidian")
hier_clust <- hclust(distances,method = "average")
hier_clust
plot(hier_clust)
clust <- cutree(hier_clust,k=3)
clust


kmeans_clustering <- kmeans(iris_pca[,2:3],centers=3,algorithm = "Lloyd")
kmeans_clustering$cluster
cluster <- data.frame(iris,clust)
cluster

BSS3 <- kmeans_clustering$betweenss
TSS3 <- kmeans_clustering$totss
BSS3 / TSS3 * 100

species <- as.vector(cluster$Species)
predict_species <- as.vector(cluster$cluster)
species

counter <- 0
for (i in 150) {
  if(species[i] != predict_species[i]){
    counter <- counter+1
  }
}
counter
length(species)


