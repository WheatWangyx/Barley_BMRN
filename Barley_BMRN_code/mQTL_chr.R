#############QTL染色体分布图本地做图，代码如下：

library(ggplot2)#加载程序包
ColD <- c("#EE0000","#4682B4","#A020F0","#ff7f00","#008856","#fb9a99","#d2691e","#7FFF00",
             "#9ACD32","#F3C300","#D02090","#836FFF","#A65628","#928262")

plotData <- read.table("window_trait_dis2.txt", header = T, stringsAsFactors = F, sep = "\t", quote = "")#导入QTL数据
chrData <- read.table("chr.txt", header = F, stringsAsFactors = F, sep = "\t", quote = "")#导入染色体数据
head(plotData)
str(plotData)
for(i in 1:3) plotData[,i] = as.integer(plotData[,i])
for(i in 4:6) plotData[,i] = as.character(plotData[,i])
for(i in 7:7) plotData[,i] = as.integer(plotData[,i])
str(plotData)
head(chrData)
str(chrData)
for(i in 1:2) chrData[,i] = as.integer(chrData[,i])
str(chrData)
chrLen<-c(0)
chrId<-factor(chrData[, 1], ordered = T, levels = chrData[, 1])
tmp <- 0
for (i in chrId){
  if (i != chrId[1]){
    plotData[plotData[, 1] == i, 2] = plotData[plotData[, 1] == i, 2] + tmp
    plotData[plotData[, 1] == i, 3] = plotData[plotData[, 1] == i, 3] + tmp
    tmp <- tmp + chrData[i, 2]
    chrLen <- c(chrLen, tmp)
  }else{
    plotData[plotData[, 1] == i, 2] = plotData[plotData[, 1] == i, 2]
    plotData[plotData[, 1] == i, 3] = plotData[plotData[, 1] == i, 3]
    tmp <- tmp + chrData[i, 2]
    chrLen <- c(chrLen, tmp)
  }
}

tick<-c()
for (i in 1:length(chrId)){
  tick <- c(tick, c(chrLen[i + 1] - chrLen[i]) / 2 + chrLen[i])
}
plotData <- plotData[order(plotData[, 5]), ]
plotData$trait <- factor(plotData$trait, ordered = T, levels = rev(unique(plotData$trait)))
classColor <- ColD[1:length(unique(plotData$class))]
p <- ggplot(plotData, aes(window_start, trait, color = year))
p <- p + geom_point(shape = 15, size = 0)
p <-p + geom_rect(aes(xmin = window_start, xmax = window_end, ymin = trait, ymax = trait), size = 2)
p <- p + theme_bw() + theme(panel.grid = element_blank())
p <- p + scale_x_continuous(breaks = tick, labels = chrId)
p <- p + xlab(label = "Chromosome") + ylab(label = "trait")
p <- p + geom_vline(xintercept = chrLen, linetype="dashed", col="grey")
#p <- p + guides(col=FALSE)
p <- p + scale_color_manual(values=classColor, guide = guide_legend(override.aes = list(size = 1)))
#p <- p + theme(axis.text=element_text(size=16),axis.title=element_text(size=16))
#p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
######plot backgroud annotate
ggplot_build(p)$data[[1]]
pGroup <- unique(ggplot_build(p)$data[[1]][c(1,3)])
count <- 0
for (i in unique(pGroup[,1])){
  count <- count + 1
  if (count %% 2 != 0){
    p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = min(pGroup[pGroup[, 1] == i, 2]) - 0.5, ymax = max(pGroup[pGroup[, 1] == i, 2]) + 0.5, alpha = 0.05)
  } else {
    p <- p + annotate("rect", xmin = -Inf, xmax = Inf, ymin = min(pGroup[pGroup[, 1] == i, 2]) - 0.5, ymax = max(pGroup[pGroup[, 1] == i, 2]) + 0.5, fill="#dbf0ff", alpha = 0.3)
  }
}

pdf("leadsnp_distribution.pdf", height = 10, width = 10)
print(p)
dev.off()

plotData <- read.table("density_plot.txt", header = F, stringsAsFactors = F, sep = "\t", quote = "")
chrData <- read.table("chr.txt", header = F, stringsAsFactors = F, sep = "\t", quote = "")

chrLen<-c(0)
chrId<-factor(chrData[, 1], ordered = T, levels = chrData[, 1])
tmp <- 0
for (i in chrId){
  if (i != chrId[1]){
    plotData[plotData[, 1] == i, 2] = plotData[plotData[, 1] == i, 2] + tmp
    plotData[plotData[, 1] == i, 3] = plotData[plotData[, 1] == i, 3] + tmp
    tmp <- tmp + chrData[i, 2]
    chrLen <- c(chrLen, tmp)
  }else{
    plotData[plotData[, 1] == i, 2] = plotData[plotData[, 1] == i, 2]
    plotData[plotData[, 1] == i, 3] = plotData[plotData[, 1] == i, 3]
    tmp <- tmp + chrData[i, 2]
    chrLen <- c(chrLen, tmp)
  }
}

tick<-c()
for (i in 1:length(chrId)){
  tick <- c(tick, c(chrLen[i + 1] - chrLen[i]) / 2 + chrLen[i])
}
p <- ggplot(plotData, aes(V3, 1, col = V4))
p <- p + geom_bar(stat = "identity")
p <- p + theme_bw() + theme(panel.grid = element_blank(), panel.border = element_blank())
p <- p + scale_color_gradient(low = "white", high = "red")
p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
p <- p + scale_x_continuous(breaks = tick, labels = chrId)
p <- p + xlab(label = "Chromosome") + ylab(label = "")
p <- p + labs(colour = "Density")

pdf("leadsnp_density.pdf", height = 2, width = 10)
print(p)
dev.off()


