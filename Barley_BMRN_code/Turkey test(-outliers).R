data<-read.csv(file.choose(),header=T,row.names=1)
dim(data)
head(data)
for (i in 1:ncol(data)) {

  data[, i] <- as.numeric(as.character(data[, i]))#将目标列修改为数值型变量
  
  outlier_limup <-
    3 * IQR(data[, i], na.rm = TRUE) + quantile(data[, i], 3 / 4, na.rm = TRUE, names = FALSE)#极端值上界 ：Q3+k(Q3-Q1)
  
  outlier_limdown <-
    quantile(data[, i], 1 / 4, na.rm = TRUE, names = FALSE) - 3 * IQR(data [, i], na.rm = TRUE) #3*极端值下界 ：Q1-k(Q3-Q1)
  
  data[data[, i] >= outlier_limup |
        data[, i] <= outlier_limdown, i] = ""#将极值点变为空值；若希望直接去除极值点所在行，可以使用把此条代码替换为：
#data <- data[!data[, i] >= outlier_limup & ! data[, i] <= outlier_limdown, ]
}
write.csv(data,file="W:/医学项目/肠癌/Dealed_neg_WTF_Tukey.csv")