# Cluster
# Algorithm - Kmeans 
setwd('../Documents/Projetos/Mining/')

# read data from matches with 3 players
data_3players <- na.omit(read.csv('./datasets/7wonders_3.csv'))

# read data from matches with 4 players
data_4players <- na.omit(read.csv('./datasets/7wonders_4.csv'))

# read data from matches with 5 players 
data_5players <- na.omit(read.csv('./datasets/7wonders_5.csv'))

# read data from matches with 6 players
data_6players <- na.omit(read.csv('./datasets/7wonders_6.csv'))

# read data from matches with 7 players
data_7players <- na.omit(read.csv('./datasets/7wonders_7.csv'))

# place x vp total 
cluster <- kmeans(data_3players[, c(1, 2)], 3)
plot(data_3players[, c(1, 2)], col = cluster$cluster, pch = 20, cex = 1)
points(cluster$centers, pch = 4, cex = 4, lwd = 4)


cluster <- kmeans(data_3players[, c(17, 15)], 8)
plot(data_3players[, c(17, 15)], col = cluster$cluster, pch = 20, cex = 1)
points(cluster$centers, pch = 4, cex = 4, lwd = 4)

# tentativa usando clusters 
# por enquanto nenhum resultado interessante