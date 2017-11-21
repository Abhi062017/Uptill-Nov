# Performing 'kmeans cluster' on Iris dataset
getwd()

# Reading dataset
iris.dataset <- read.csv('Iris.csv')
View(iris.dataset)
names(iris.dataset)

# Pre-proecessing
  # 1)remove unwanted column(s)
  iris.dataset <- iris.dataset[-c(1)]
  View(iris.dataset)
  names(iris.dataset)
  
  # 2)lower case the column names
  names(iris.dataset) <- c('sepal.length','sepal.width', 'petal.length', 'petal.width', 'species')
  names(iris.dataset)

# Invoking kmeans
  #remove the species column (as it will be predicted by kmean)
  iris_kmeans <- iris.dataset[-5]
  names(iris_kmeans)
  
  kmeans_cluster <- kmeans(iris_kmeans, 3) #as 3 is the no. of species
  kmeans_cluster
    #can also access features using:
    kmeans_cluster['size'] #can also be written as kmeans_cluster[7] or kmeans_cluster$size
    kmeans_cluster['cluster'] #can also be written as kmeans_cluster[1] or kmeans_Cluster$cluster

  # comparing the values of kmeans with the original dataset
  table(iris.dataset$species, kmeans_cluster$cluster) 
  #notice kmeans cluster is close to species of original dataset in cluster#1 and cluster#2,
  #but the cluster#3 has a lot of overlappings.

# comparing the overlapping thru plots
plot(iris_kmeans[c('petal.length','petal.width')], col = kmeans_cluster$cluster)
plot(iris_kmeans[c('petal.length','petal.width')], col = iris.dataset$species)

plot(iris_kmeans[c('sepal.length','sepal.width')], col = kmeans_cluster$cluster)
plot(iris_kmeans[c('sepal.length','sepal.width')], col = iris.dataset$species)

boxplot(iris_kmeans[c('petal.length','petal.width')], col = kmeans_cluster$cluster)
boxplot(iris_kmeans[c('petal.length','petal.width')], col = iris.dataset$species)

boxplot(iris_kmeans[c('sepal.length','sepal.width')], col = kmeans_cluster$cluster)
boxplot(iris_kmeans[c('sepal.length','sepal.width')], col = iris.dataset$species)