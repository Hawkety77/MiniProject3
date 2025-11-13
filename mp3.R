library(tidyverse)
library(reshape2)
library(cluster)

set.seed(42)

# Read in the data --------------------------------------------------------
df <- read.table('https://tofu.byu.edu/docs/files/stat666/datasets/collins.txt',skip=10,header=TRUE)
head(df)
df$Genre <- as.factor(df$Genre)
df$Corpus <- as.factor(df$Corpus)
df$Corp.Genre <- as.factor(df$Corp.Genre)
df$Text <- as.factor(df$Text)
df$Counter <- as.factor(df$Counter)
summary(df)


# Do the clustering -------------------------------------------------------

cluster_df <- df %>% select(-c(Genre,Counter,Corpus,Corp.Genre,Text,Text_Coverage))
cluster_mat <- data.matrix(cluster_df)

vars <- apply(cluster_mat, 2, var)
means <- apply(cluster_mat, 2, mean)
cluster_mat_std <- cluster_mat[]
for (i in 1:nrow(cluster_mat)) {
  cluster_mat_std[i, ] <- (cluster_mat[i, ] - means) / sqrt(vars)
}



## agglomerative clustering
dc <- dist(cluster_mat_std)
wardlink <- hclust(dc,method='ward.D2')
plot(wardlink,labels=FALSE)

cluster_assigns <- matrix(nrow=nrow(cluster_mat_std),ncol=10)
colnames(cluster_assigns) <- c('agglo_3','agglo_4','agglo_5','agglo_6','agglo_7',
                               'means_3','means_4','means_5','means_6','means_7')
for(k in 3:7){
  cluster_assigns[,k-2] <- cutree(wardlink,k=k)
}

## K-means
for(k in 3:7){
  cluster_assigns[,k+3] <- kmeans(cluster_mat_std,k)$cluster
}

cluster_df_with_clusters <- as.data.frame(cbind(cluster_df,cluster_assigns))

# Follow up analysis ------------------------------------------------------

## Need to do manova in SAS?
for(col in colnames(cluster_assigns)) {
  
  # Build formula dynamically
  fmla <- reformulate(termlabels = paste0("as.factor(", col, ")"),
                      response = paste0("cbind(",
                                        paste(c("FirstPerson", "InnerThinking", "ThinkPositive", "ThinkNegative",
                                                "ThinkAhead", "ThinkBack", "Reasoning", "Share_SocTies", 
                                                "Direct_Activity", "Interacting", "Notifying", "LinearGuidance", 
                                                "WordPicture", "SpaceInterval", "Motion", "PastEvents", 
                                                "TimeInterval", "ShiftingEvents"),
                                              collapse = ", "),
                                        ")"))
  
  cat("\n\n==============================\n")
  cat("Results for factor:", col, "\n")
  cat("==============================\n")
  
  model <- manova(fmla, data = cluster_df_with_clusters)

  # Extract each test
  pillai    <- summary(model, test = "Pillai")$stats
  wilks     <- summary(model, test = "Wilks")$stats
  hotelling <- summary(model, test = "Hotelling-Lawley")$stats
  roy       <- summary(model, test = "Roy")$stats
  
  # Print results in SAS-like table
  cat("\nMultivariate Tests of Significance\n")
  cat("---------------------------------------------\n")
  cat("Test\t\t|\tStatistic\tF Value\t\tPr(>F)\n")
  cat("---------------------------------------------\n")
  cat(sprintf("Pillai\t\t|\t%.2f\t\t%.2f\t\t%.2g\n", pillai[1,2], pillai[1,3], pillai[1,6]))
  cat(sprintf("Wilks\t\t|\t%.2f\t\t%.2f\t\t%.2g\n",  wilks[1,2],  wilks[1,3],  wilks[1,6]))
  cat(sprintf("Hotelling\t|\t%.2f\t\t%.2f\t\t%.2g\n", hotelling[1,2], hotelling[1,3], hotelling[1,6]))
  cat(sprintf("Roy\t\t|\t%.2f\t\t%.2f\t\t%.2g\n",  roy[1,2],  roy[1,3],  roy[1,6]))
  cat("---------------------------------------------\n")
}

## misclassification rates
# lda
for(col in colnames(cluster_assigns)) {
  predictors <- c("FirstPerson", "InnerThinking", "ThinkPositive", "ThinkNegative",
                  "ThinkAhead", "ThinkBack", "Reasoning", "Share_SocTies", 
                  "Direct_Activity", "Interacting", "Notifying", "LinearGuidance", 
                  "WordPicture", "SpaceInterval", "Motion", "PastEvents", 
                  "TimeInterval", "ShiftingEvents")
  y <- as.factor(cluster_df_with_clusters[[col]])
  n <- length(y)
  k <- 5
  # stratified fold assignment
  set.seed(42)
  folds <- integer(n)
  for(level in levels(y)) {
    idx <- which(y == level)
    idx <- sample(idx)
    folds[idx] <- rep(1:k, length.out = length(idx))
  }
  
  fold_mis <- numeric(k)
  for(f in 1:k) {
    train_idx <- which(folds != f)
    test_idx  <- which(folds == f)
    train_df <- cluster_df_with_clusters[train_idx, , drop = FALSE]
    test_df  <- cluster_df_with_clusters[test_idx, , drop = FALSE]
    
    # if some classes are missing in training, skip this fold
    if(length(unique(train_df[[col]])) < length(levels(y))) {
      fold_mis[f] <- NA
      next
    }
    
    fmla <- reformulate(
      response = paste0("as.factor(", col, ")"),
      termlabels = predictors
    )
    
    model <- tryCatch(MASS::lda(fmla, data = train_df), error = function(e) NULL)
    if(is.null(model)) {
      fold_mis[f] <- NA
      next
    }
    
    preds <- predict(model, newdata = test_df)$class
    fold_mis[f] <- 1 - mean(preds == as.factor(test_df[[col]]))
  }
  
  mean_mis <- mean(fold_mis, na.rm = TRUE)
  cat("Factor:", col, "\t5-fold CV Misclassification rate:", round(mean_mis, 4), "\n")
}
# qda
for(col in colnames(cluster_assigns)) {
  predictors <- c("FirstPerson", "InnerThinking", "ThinkPositive", "ThinkNegative",
                  "ThinkAhead", "ThinkBack", "Reasoning", "Share_SocTies", 
                  "Direct_Activity", "Interacting", "Notifying", "LinearGuidance", 
                  "WordPicture", "SpaceInterval", "Motion", "PastEvents", 
                  "TimeInterval", "ShiftingEvents")
  y <- as.factor(cluster_df_with_clusters[[col]])
  n <- length(y)
  k <- 5
  # stratified fold assignment
  set.seed(42)
  folds <- integer(n)
  for(level in levels(y)) {
    idx <- which(y == level)
    idx <- sample(idx)
    folds[idx] <- rep(1:k, length.out = length(idx))
  }
  
  fold_mis <- numeric(k)
  for(f in 1:k) {
    train_idx <- which(folds != f)
    test_idx  <- which(folds == f)
    train_df <- cluster_df_with_clusters[train_idx, , drop = FALSE]
    test_df  <- cluster_df_with_clusters[test_idx, , drop = FALSE]
    
    # if some classes are missing in training, skip this fold
    if(length(unique(train_df[[col]])) < length(levels(y))) {
      fold_mis[f] <- NA
      next
    }
    
    fmla <- reformulate(
      response = paste0("as.factor(", col, ")"),
      termlabels = predictors
    )
    
    model <- tryCatch(MASS::qda(fmla, data = train_df), error = function(e) NULL)
    if(is.null(model)) {
      fold_mis[f] <- NA
      next
    }
    
    preds <- predict(model, newdata = test_df)$class
    fold_mis[f] <- 1 - mean(preds == as.factor(test_df[[col]]))
  }
  
  mean_mis <- mean(fold_mis, na.rm = TRUE)
  cat("Factor:", col, "\t5-fold CV Misclassification rate:", round(mean_mis, 4), "\n")
}

## Silhouette scores
for(col in colnames(cluster_assigns)) {
  clusters <- cluster_assigns[,col]
  # Compute silhouette scores using Euclidean distance in predictor space
  dist_matrix <- dist(cluster_mat)  # distance matrix
  sil <- silhouette(as.numeric(clusters), dist_matrix)
  avg_sil <- mean(sil[, "sil_width"])  # average silhouette width
  
  cat("Factor: ", col,
      "\tAverage Silhouette:", round(avg_sil, 4), "\n")
}




# Step 3 ------------------------------------------------------------------
final_df <- df[]
## compare to genres
genre_labels <- c(
  "Press: Reporting",
  "Press: Editorial",
  "Press: Reviews",
  "Religion",
  "Skills and Hobbies",
  "Popular Lore",
  "Biography, Memoirs,etc.",
  "Official Communications",
  "Learned (scholarly journals)",
  "General Fiction",
  "Mystery and Detective Fiction",
  "Science Fiction",
  "Adventure and Western Fiction",
  "Romance and Love Story",
  "Humor"
)
final_df$GenreLabel <- genre_labels[final_df$Genre]
final_df$Cluster <- cluster_assigns[,'means_3']
genre_cluster_table <- table(final_df$Cluster, final_df$GenreLabel)
print(genre_cluster_table)
# Convert to long format for ggplot
genre_cluster_long <- melt(genre_cluster_table)
colnames(genre_cluster_long) <- c("Cluster", "Genre", "Count")
ggplot(genre_cluster_long, aes(x = Genre, y = factor(Cluster), fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 3) +  # add numbers
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Genre", y = "Cluster", fill = "Count",
       title = "Genre Distribution Across Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))

## Compare to super genres
super_genre_labels <- c(
'Press','Press','Press',
'Non-press Nonfiction','Non-press Nonfiction','Non-press Nonfiction',
'Biography',
'Scholarship and Official Docs','Scholarship and Official Docs',
'Fiction','Fiction','Fiction','Fiction','Fiction','Fiction'
)
final_df$SuperGenre <- super_genre_labels[final_df$Genre]
final_df$FiveCluster <- cluster_assigns[,'agglo_5']
super_genre_cluster_table <- table(final_df$FiveCluster, final_df$SuperGenre)
print(super_genre_cluster_table)
# Convert to long format for ggplot
super_genre_cluster_long <- melt(super_genre_cluster_table)
colnames(super_genre_cluster_long) <- c("Cluster", "Genre", "Count")
ggplot(super_genre_cluster_long, aes(x = Genre, y = factor(Cluster), fill = Count)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Count), color = "black", size = 3) +  # add numbers
  scale_fill_gradient(low = "white", high = "steelblue") +
  labs(x = "Genre", y = "Cluster", fill = "Count",
       title = "Genre Distribution Across Clusters") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))



output_df  <- df[]
colnames(cluster_mat_std) <- paste0(colnames(cluster_mat_std), '_std')
output_df <- cbind(output_df, cluster_mat_std, cluster_assigns)

write.csv(output_df,'./final_df.csv',row.names=FALSE)
