library(dplyr)
library(ggplot2)
library(GGally)
library(Rtsne)
library(cluster)

df_full <- read.table('https://tofu.byu.edu/docs/files/stat666/datasets/collins.txt',
                      skip=10, header=TRUE)

df <- df_full %>% 
  select(-Text, -Text_Coverage, -Genre, -Counter, -Corpus, -Corp.Genre) %>%
  scale()

# T-SNE
set.seed(77)
tsne_result <- Rtsne(df, perplexity=30)
tsne_df_base <- data.frame(X = tsne_result$Y[,1], Y = tsne_result$Y[,2])

# t-SNE on scaled data with DBSCAN clusters
tsne_df_dbscan <- data.frame(X=tsne_result$Y[,1], Y=tsne_result$Y[,2], Cluster=dbscan_cluster)
ggplot(tsne_df_dbscan, aes(x=X, y=Y, color=Cluster)) +
  geom_point(alpha=0.7, size=2) +
  theme_minimal() +
  labs(title="t-SNE Visualization (DBSCAN Clusters)", 
       x="t-SNE Dimension 1", y="t-SNE Dimension 2")

# k-means clustering with varying k
par(mfrow=c(2,3))

for (k in 3:7) {
  set.seed(77)
  kmeans_result <- kmeans(df, centers=k, nstart=20)
  kmeans_cluster <- as.factor(kmeans_result$cluster)
  
  tsne_df <- tsne_df_base
  tsne_df$Cluster <- kmeans_cluster
  
  plot_title <- paste("t-SNE (k-means, k =", k, ")")
  
  p <- ggplot(tsne_df, aes(x=X, y=Y, color=Cluster)) +
    geom_point(alpha=0.7, size=2) +
    theme_minimal() +
    labs(title=plot_title, x="t-SNE Dimension 1", y="t-SNE Dimension 2")
  
  print(p)
}

# get elbow scores and sillouette scores for k-means
elbow_scores <- numeric(10)
silhouette_scores <- numeric(10)
for (k in 1:10) {
  set.seed(77)
  kmeans_result <- kmeans(df, centers=k, nstart=20)
  elbow_scores[k] <- kmeans_result$tot.withinss
  
  if (k > 1) {
    ss <- silhouette(kmeans_result$cluster, dist(df))
    silhouette_scores[k] <- mean(ss[, 3])
  } else {
    silhouette_scores[k] <- NA
  }
}
par(mfrow=c(1,2))
plot(1:10, elbow_scores, type='b', xlab='Number of Clusters (k)', ylab='Total Within-Cluster SS',
     main='Elbow Method for k-means')
plot(1:10, silhouette_scores, type='b', xlab='Number of Clusters (k)', ylab='Average Silhouette Width',
     main='Silhouette Scores for k-means')





