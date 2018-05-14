# cluster analysis

library("cluster") 
library("factoextra") 
library("magrittr")

# load dataset "USArrests"
data("USArrests")
USArrests

# data preparation 
# - remove na
# - scale value
my_data <- USArrests %>%
    na.omit() %>%
    scale()
str(my_data)

# show data
head(my_data, n = 8)
head(my_data, n = 50)

# calculation of the distances with different methods
res.dist <- get_dist(USArrests, stand = TRUE, method = "euclidean")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(title = "euclidean")

res.dist <- get_dist(USArrests, stand = TRUE, method = "pearson")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + 
    labs(title = "pearson")

res.dist <- get_dist(USArrests, stand = TRUE, method = "manhattan")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(title = "manhattan")

res.dist <- get_dist(USArrests, stand = TRUE, method = "minkowski")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(title = "minkowski")

# to be fixed...
par(mfrow=c(2,2))
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(title = "euclidean")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) + 
    labs(title = "pearson")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(title = "manhattan")
fviz_dist(res.dist, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07")) +
    labs(title = "minkowski")
par(mfrow=c(1,1))

# Determining the optimal number of clusters
fviz_nbclust(my_data, FUNcluster = fanny, method = "gap_stat")
fviz_nbclust(my_data, FUNcluster = kmeans, method = "gap_stat")
fviz_nbclust(my_data, FUNcluster = hcut, method = "gap_stat")
fviz_nbclust(my_data, kmeans, method = "silhouette")
fviz_nbclust(my_data, kmeans, method = "wss")

# perform kmeans
set.seed(123)
km.res <- kmeans(my_data, centers = 2, nstart = 25)

plot(my_data~rep.int(1,50),col = km.res$cluster)
plot(my_data,col = km.res$cluster)
fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())
km.res$cluster

km.res <- kmeans(my_data, 5, nstart = 25)
fviz_cluster(km.res, data = my_data,
             ellipse.type = "convex",
             palette = "jco",
             ggtheme = theme_minimal())

pam.res <- pam(my_data, 3)
fviz_cluster(pam.res)
pam.res$clustering

km.res$cluster 
canc <- data.frame(pam.res$clustering,km.res$cluster)


# hierarchical clustering
res.hc <- USArrests %>%
    scale() %>%
    dist(method = "euclidean") %>%
    hclust(method = "ward.D2")
attributes(res.hc)
fviz_dend(res.hc, k = 10,
          cex = 0.5,
          rect = TRUE
)
