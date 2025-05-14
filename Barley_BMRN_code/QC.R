data <- read.table("Data.txt", header=T, row.names=1, sep="\t", check.names=F)
data[is.na(data)] <- 100

clean <- c()
for (i in 1:nrow(data)){
    for (j in seq(1, ncol(data[i,]), by=3)){
        if(data[i,j]>0.5 & data[i,j+1]>0.5 & data[i,j+2]>0.5){
            clean <- append(clean,i)
            break
        }
    }
}
data_clean <- data[clean,]