library(ggpubr)

plotlist <- list()
rep <- 3
gname <- c("P1", "P2", "P3", "P4", "P5", "P6", "L1", "L2", "L3", "L4", "L5", "L6")
center <- 5

for (k in 1:center) {
  module <- data_clean_log2_zs_kmeans[data_clean_log2_zs_kmeans$cluster == k, ]
  df1 <- data.frame(ID = rownames(module), module[, -ncol(module)])
  
  df2 <- data.frame()
  df3 <- data.frame()
  
  for (i in seq(2, (ncol(df1) - rep + 1), by = rep)) {
    df_tmp <- data.frame(
      ID = df1$ID,
      Value = rowMeans(df1[, i:(i + rep - 1)]),
      Group = gname[(i - 2 + rep)/rep]
    )
    df2 <- rbind(df2, df_tmp)
    
    df3 <- rbind(df3, data.frame(
      ID = "mean",
      Value = mean(df_tmp$Value),
      Group = gname[(i - 2 + rep)/rep]
    ))
  }
  
  df2$Group <- factor(df2$Group, levels = gname)
  df3$Group <- factor(df3$Group, levels = gname)
  
  plot <- ggplot(df2, aes(x = Group, y = Value)) +
    geom_line(aes(group = ID), colour = '#FFB6C1', alpha = 0.5, linewidth = 0.5) +
    geom_line(data = df3, aes(group = ID), color = '#B22222', linewidth = 0.7) +
    theme_bw() +
    labs(x = paste('Cluster', k, '(n =', nrow(module), ')'), y = 'Scaled Value') +
    theme(
      panel.grid = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black")
    )
  
  assign(paste0("p", k), plot)
}

kmeans_plot <- ggarrange(p1, p2, p3, p4, p5, ncol = 3, nrow = 2)
ggsave("kmeans_plot.pdf", kmeans_plot, width = 9, height = 8)