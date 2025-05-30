rm(list=ls()) #清除工作区
library(ggplot2)
Data<-read.csv(file.choose())
head（Data)
str(Data)
for(i in 1:2) Data[,i] = as.factor(Data[,i])    #将前n个因素转化为因子（Facter），n在具体操作室应换成具体数值
str(Data)

#对数据进行统计的函数
#指定分组变量和求值变量后，可计算出不同分组变量(或分组变量间的组合)对应的求值变量的均值，标准差，标准误，置信区间ci

#汇总数据
#计算出计数，平均值，标准差，均值的标准误差和置信区间（默认为95％）
#data：一个数据框
#measurevar：包含要汇总的变量的列的名称
#groupvars：包含分组变量的列名称的向量
#na.rm：一个布尔值，表示是否忽略NA
## conf.interval：置信区间的百分比范围（默认为95％）

## Summarizes data.
## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}


#依据分组对vale进行统计
Data_summary <- summarySE(Data, measurevar="Value", groupvars=c("Trait","Attribute"))
head(Data_summary)
P1 <- ggplot(Data_summary,aes(x=Trait, y=Value, fill=Attribute)) + #“fill=”设置填充颜色依据Attribute指定
  geom_point(aes(x=Trait, y=Value),pch=19,position=position_dodge(0.9),size=2.5)+ #绘制均值为点图
  geom_bar(stat = "identity",position = "dodge",alpha = 0.7) + #绘制条形图
  
  #如果误差条想表示标准差：请设置 ymin = value-sd, ymax=value+sd
  #如果误差条想表示标准误：请设置 ymin = value-se, ymax=value+se
  geom_errorbar(aes(ymin = Value-ci, ymax=Value+ci), #误差条表示95%的置信区间
                width=0.1, #误差条末端短横线的宽度
                position=position_dodge(0.9), 
                color="black",
                alpha = 0.7,
                size=0.5) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "#FF4040", "#7FFF00", "#BF3EFF", "#00EEEE", "FF8C00", "#FFE4C4"))+ #设置填充颜色
  theme_bw()+ #背景变为白色
  theme(axis.text.x=element_text(angle=0,hjust = .5,colour="black",family="Times",size=20), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
        axis.text.y=element_text(family="Times",size=16,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
        axis.title.y=element_text(family="Times",size = 20,face="plain"), #设置y轴标题的字体属性
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
        legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                 size=16),
        legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                  size=18),
        panel.grid.major = element_blank(),   #不显示网格线
        panel.grid.minor = element_blank())+  #不显示网格线
  ylab("Value")+xlab("") #设置x轴和y轴的标题

  P1
  jpeg(file = "results_Value_1.jpg",width =1600,height = 2000,units = "px",res =300) #结果保存
  print(P1)
  dev.off()

P2<- ggplot(Data, aes(x=Trait, y=Value,fill=Attribute)) + 
  geom_violin(trim=FALSE,color="white") + #绘制小提琴图, “color=”设置小提琴图的轮廓线的颜色(以下设为背景为白色，其实表示不要轮廓线)
  #"trim"如果为TRUE(默认值),则将小提琴的尾部修剪到数据范围。如果为FALSE,不修剪尾部。
  geom_boxplot(width=0.2,position=position_dodge(0.9))+ #绘制箱线图
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "#FF4040", "#7FFF00", "#BF3EFF"))+ #设置填充的颜色
  theme_bw()+ #背景变为白色
  theme(axis.text.x=element_text(angle=0,hjust = .5,colour="black",family="Times",size=20), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
        axis.text.y=element_text(family="Times",size=16,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
        axis.title.y=element_text(family="Times",size = 20,face="plain"), #设置y轴标题的字体属性
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
        legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                 size=16),
        legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                  size=18),
        panel.grid.major = element_blank(),   #不显示网格线
        panel.grid.minor = element_blank())+  #不显示网格线
  ylab("Value")+xlab("") #设置x轴和y轴的标题
  
P2

jpeg(file = "results_Value_2.jpg",width =1600,height = 2000,units = "px",res =300) #结果保存
print(P2)
dev.off()

P3 <- ggplot(Data, aes(x=Trait, y=Value,fill=Attribute)) + 
  geom_violin(trim=FALSE,color="white") + #绘制小提琴图
  geom_point(data = Data_summary,aes(x=Trait, y=Value),pch=19,position=position_dodge(0.9),size=1.5)+ #绘制均值为点图
  geom_errorbar(data = Data_summary,aes(ymin = Value-ci, ymax=Value+ci), #误差条表示95%的置信区间
                width=0.1, #误差条末端短横线的宽度
                position=position_dodge(0.9), 
                color="black",
                alpha = 0.7,
                size=0.5) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00", "#FF4040", "#7FFF00", "#BF3EFF"))+ #设置填充的颜色
  theme_bw()+ #背景变为白色
  theme(axis.text.x=element_text(angle=0,hjust = .5,colour="black",family="Times",size=20), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
        axis.text.y=element_text(family="Times",size=16,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
        axis.title.y=element_text(family="Times",size = 20,face="plain"), #设置y轴标题的字体属性
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
        legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                 size=16),
        legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                  size=18),
        panel.grid.major = element_blank(),   #不显示网格线
        panel.grid.minor = element_blank())+  #不显示网格线
  ylab("Value")+xlab("") #设置x轴和y轴的标题

P3 

jpeg(file = "results_Value_3.jpg",width =1600,height = 2000,units = "px",res =300) #结果保存
print(P3)
dev.off()

GeomSplitViolin <- ggproto(
  "GeomSplitViolin", 
  GeomViolin, 
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <- transform(data, 
                      xminv = x - violinwidth * (x - xmin), 
                      xmaxv = x + violinwidth * (xmax - x))
    grp <- data[1,'group']
    newdata <- plyr::arrange(
      transform(data, x = if(grp%%2==1) xminv else xmaxv), 
      if(grp%%2==1) y else -y
    )
    newdata <- rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1,nrow(newdata)-1,nrow(newdata)), 'x'] <- round(newdata[1, 'x']) 
    if (length(draw_quantiles) > 0 & !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <= 1))
      quantiles <- ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <- data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <- rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <- GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin", 
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    } else {
      ggplot2:::ggname("geom_split_violin", GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <- function (mapping = NULL, 
                               data = NULL, 
                               stat = "ydensity", 
                               position = "identity", ..., 
                               draw_quantiles = NULL, 
                               trim = TRUE, 
                               scale = "area", 
                               na.rm = FALSE, 
                               show.legend = NA, 
                               inherit.aes = TRUE) {
  layer(data = data, 
        mapping = mapping, 
        stat = stat, 
        geom = GeomSplitViolin, 
        position = position, 
        show.legend = show.legend, 
        inherit.aes = inherit.aes, 
        params = list(trim = trim, 
                      scale = scale, 
                      draw_quantiles = draw_quantiles, 
                      na.rm = na.rm, ...)
  )
}

P4 <- ggplot(data=Data, aes(x=Trait, y=Value,fill=Attribute)) + 
  geom_split_violin(trim=FALSE,color="white") + #绘制分半的小提琴图
  geom_point(data = Data_summary,aes(x=Trait, y=Value),pch=19,position=position_dodge(0.9),size=1.5)+ #绘制均值为点图
  geom_errorbar(data = Data_summary,aes(ymin = Value-ci, ymax=Value+ci), #误差条表示95%的置信区间
                width=0.1, #误差条末端短横线的宽度
                position=position_dodge(0.9), 
                color="black",
                alpha = 0.7,
                size=0.5) +
  scale_fill_manual(values = c("#56B4E9", "#E69F00"))+ #设置填充的颜色
  theme_bw()+ #背景变为白色
  theme(axis.text.x=element_text(angle=0,hjust = .5,colour="black",family="Times",size=20), #设置x轴刻度标签的字体显示倾斜角度为15度，并向下调整1(hjust = 1)，字体簇为Times大小为20
        axis.text.y=element_text(family="Times",size=16,face="plain"), #设置y轴刻度标签的字体簇，字体大小，字体样式为plain
        axis.title.y=element_text(family="Times",size = 20,face="plain"), #设置y轴标题的字体属性
        panel.border = element_blank(),axis.line = element_line(colour = "black",size=1), #去除默认填充的灰色，并将x=0轴和y=0轴加粗显示(size=1)
        legend.text=element_text(face="italic", family="Times", colour="black",  #设置图例的子标题的字体属性
                                 size=16),
        legend.title=element_text(face="italic", family="Times", colour="black", #设置图例的总标题的字体属性
                                  size=18),
        panel.grid.major = element_blank(),   #不显示网格线
        panel.grid.minor = element_blank())+  #不显示网格线
  ylab("Value")+xlab("") #设置x轴和y轴的标题

P4

jpeg(file = "results_Value_4.jpg",width =1600,height = 2000,units = "px",res =300) #结果保存
print(P4)
dev.off()