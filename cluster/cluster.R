# Session -> Set Working Directory -> To Source File Location

# Clustering
# Algorithm - K-means 
# Objective: separate player playstyles.

# Great reference: https://uc-r.github.io/kmeans_clustering

# Rules to make K-means work correctly:
# -------------------------------------
# 1) Rows must be observations and columns variables;
# 2) NAs must be removed;
# 3) Data must be scaled to make variables comparable.

library(cluster)
library(factoextra)
library(dplyr)

set.seed(123)

df3 <- na.omit(read.csv("../datasets/mar_20/7wonders_3.csv"))

df3_winners <- df3[df3$Place == 1, ]
#df3_winners <- df3
df3_vps <- df3_winners[, c("VP.from.Civilian.Structures", "VP.from.Commercial.Structures", 
           "VP.from.Guilds", "VP.from.Military.Conflicts..Victory.",
           "VP.from.Scientific.Structures", 
           "VP.from.Treasury.Contents", "VP.from.Wonder")]

df3_vps_scaled <- scale(df3_vps)

# Finding the optimal optimal number of clusters.
# Elbow method found k = 2 or 3
fviz_nbclust(df3_vps_scaled, kmeans, method = "wss") +
  labs(subtitle = "Elbow method")

# Silhouette found k = 2
fviz_nbclust(df3_vps_scaled, kmeans, method = "silhouette")+
  labs(subtitle = "Silhouette method")

# Gap statistics found k = 10 and it's too slow; not suitable for this dataset
#fviz_nbclust(df3_vps_scaled, kmeans, nstart = 25,  method = "gap_stat", nboot = 50)+
#  labs(subtitle = "Gap statistic method")

df3_kmeans <- kmeans(df3_vps_scaled, 2)
#df3_pam <- pam(df3_vps_scaled, 3) # PAM wields similar but worse results. 
clusters <- df3_kmeans$cluster
cluster_list <- cbind(df3_vps, clusters)
fviz_cluster(df3_kmeans, data=df3_vps, geom=c("point"))

df3_kmeans$size

# Counts contribution in % of each VP to each cluster.
cluster_table <- df3_vps %>%
  mutate(Cluster = clusters) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
cluster_table <- select(cluster_table, -1)


# Plotting stuff.
vps <- c("VP Civil", "VP Commerce", "VP Guilds", "VP Military", "VP Science", "VP Treasury", "VP Wonder")
col <- c("orange",   "yellow",      "green",     "red",         "blue",       "purple",      "pink")
barplot(t(cluster_table), beside = TRUE, ylim = c(0, 50), names.arg=c("Cluster 1", "Cluster 2"), xlab = "Cluster", ylab = "% Contribution", col=col, axis.lty="solid")+
  legend("topright", vps, cex = 1.0, fill = col, title="Victory Point Type")


# In general, each Cluster can be defined as it follows (n_clusters = 3):
# > Military with some balanced play.
# > Purely scientific.
# > Military focus.
#
# Arguably, "Military" focus and "Military with some balanced play". This is the case for n_clusters = 2.
#
# It's clear to see that while the Military stategy is the go-to for most 1st place players,
# it still needs to be complemented by some other type of VP: mostly provided by Civilian
# structures, Guilds and sometimes Treasury.
# The mad scientist, on the other hand, relies only on Science VPs, as they provide more
# points alone than any other type of VP.