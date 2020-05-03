# Session -> Set Working Directiory -> To Source File Location

# Clustering
# Algorithm - K-means 
# Objective: separate player playstyles.

# Rules to make K-means work correctly:
# -------------------------------------
# 1) Rows must be observations and columns variables;
# 2) NAs must be removed;
# 3) Data must be scaled to make variables comparable.

library(cluster)
library(factoextra)

set.seed(123)

df3 <- na.omit(read.csv("../datasets/mar_20/7wonders_3.csv"))

df3_winners <- df3[df3$Place == 1, ]
df3_vps <- df3_winners[, c("VP.from.Civilian.Structures", "VP.from.Commercial.Structures", 
           "VP.from.Guilds", "VP.from.Military.Conflicts..Victory.",
           "VP.from.Military.Conflicts..Defeat.", "VP.from.Scientific.Structures", 
           "VP.from.Treasury.Contents", "VP.from.Wonder")]

df3_vps_scaled <- scale(df3_vps)

# Finding the optimal optimal number of clusters.
# Elbow method found k = 3 or 4
fviz_nbclust(df3_vps_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Silhouette found k = 2
fviz_nbclust(df3_vps_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistics found k = 10 and it's too slow; not suitable for this dataset
#fviz_nbclust(df3_vps_scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
#  labs(subtitle = "Gap statistic method")

df3_kmeans <- kmeans(df3_vps_scaled, 3)
clusters <- df3_kmeans$cluster
cluster_list <- cbind(df3_vps, clusters)
fviz_cluster(df3_kmeans, data=df3_vps)
