datain <- c(1, 4, 9, 16, 25, 36, 49, 64, 81)
datain
source('D:/R tasks/clusters/functions.R')
do(datain)
hc <- hclust(dist(datain))  

plot(hc) 

x <- a(1, 4, 9, 16, 25, 36, 49, 64, 81)


hc <- hclust(dist(datain))  
plot(hc)


x <- c (1, 4, 9, 16, 25, 36, 49, 64, 81)
x
hc <- hclust(dist(x))  
plot(hc)


hc <- hclust(dist(x), method="ward.D")
hc <- hclust(dist(x))  
plot(hc, labels=abbreviate(x[1:9]))
hc

hc 
