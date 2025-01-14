rm(list=ls()) #清除工作区
library(ggpubr)
library(magrittr)
library(ggthemes)
library(gmodels)
data<-read.csv(file.choose(),header=T,row.names=1)
dim(data)
head(data)
pca.info<-fast.prcomp(data)
head(pca.info$rotation)
pca.data<-data.frame(sample=rownames(pca.info$rotation),Type=c(rep("Case",5),rep("Control",5)),pca.info$rotation)
head(pca.data)
ggscatter(pca.data,x="PC1",y="PC2",color=c("Type"),ellipse=TRUE)
ggscatter(pca.data,x="PC1",y="PC2",color=c("Type"),ellipse=TRUE,ellipse.type="convex")

png("pca1118.pdf")
dev.off()

ggscatter(pca.data,x="PC1",y="PC2",color=c("Type"),ellipse=TRUE,ellipse.alpha=0.1,ellipse.border.remove=TRUE,palette=c("#1d419b","#CC0000"),label="sample",repel=TRUE,label.select=())