center <- 5
fit <- kmeans(data_clean_log2_zs, centers = center, iter.max = 100, nstart = 25)
withinss <- fit$tot.withinss
print(paste("Get withinss for the first run", withinss))

try_count <- 10
for (i in 1:try_count) {
  tmpfit <- kmeans(data_clean_log2_zs, centers = center, iter.max = 100, nstart = 25)
  tmpwithinss <- tmpfit$tot.withinss
  print(paste("The additional", i, "run, withinss", tmpwithinss))
  
  if (tmpwithinss < withinss) {
    withinss <- tmpwithinss
    fit <- tmpfit
  }
}

fit_cluster <- fit$cluster
data_clean_log2_zs_kmeans <- data.frame(cbind(data_clean_log2_zs, cluster = fit_cluster))
write.table(data_clean_log2_zs_kmeans, file = "data_clean_log2_zs_kmeans.txt", 
            sep = "\t", quote = FALSE, row.names = TRUE)