group <- read.table("group.txt", header = T, sep = "\t", check.names = F)
pca <- prcomp(t(data_clean_log2_zs), center = T, scale = F)
pca_val <- pca$x[,1:2]
pca_val_1 <- round((pca$sdev^2/sum(pca$sdev^2))[1]*100, 1)
pca_val_2 <- round((pca$sdev^2/sum(pca$sdev^2))[2]*100, 1)
pca_data <- merge(pca_val, group, by = "row.names", all.x = T)

library(ggplot2)
pca_plot <- ggplot(pca_data) +
  geom_point(aes(x = PC1, y = PC2, color = Group), alpha = 0.7, size = 3) +
  theme_bw() +
  theme(legend.background = element_blank(),
        legend.key = element_blank(),
        panel.background = element_rect(fill = 'transparent')) +
  labs(x = paste0("PC1(", pca_val_1, "%)"), 
       y = paste0("PC2(", pca_val_2, "%)"))
ggsave("pca_plot.pdf", pca_plot, width = 6, height = 5)

hcl <- hclust(dist(t(data_clean_log2_zs), method = "euclidean"), method = "ward.D2")
pdf("hcl_plot.pdf", width = 6, height = 5)
plot(hcl, hang = -1, cex = 0.8)
dev.off()