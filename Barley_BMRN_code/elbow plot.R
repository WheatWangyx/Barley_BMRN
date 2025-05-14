library(cluster)

test_cluster <- 20
wss <- numeric(test_cluster)
wss[1] <- (nrow(data_clean_log2_zs)-1) * sum(apply(data_clean_log2_zs, 2, var))

for (i in 2:test_cluster) {
  wss[i] <- kmeans(data_clean_log2_zs, centers=i, iter.max=100, nstart=25)$tot.withinss
}

pdf("elbow_plot.pdf", width=6, height=5)
plot(1:test_cluster, wss, type="b", 
     xlab="Number of Clusters", 
     ylab="Within groups sum of squares")
dev.off()