# 6.2.1
rm (list=ls())
print("Hello, R World!")
5 + 3 * 2 - 10 / 2
(5 + 3) * (2 - 10) / 2
sqrt(841)
?sqrt
2^3
98^7
sqrt(-4)
x<-42
x
hoge<- 1:10

#6.2.2
hoge
hoge2 <- hoge*2
hoge2
str(hoge2)
hoge2[3]
hoge2[2:5]
hoge2[c(2,4,6,8,10)]

#6.2.3
matrix(hoge2,ncol=2)
hoge3<-matrix(hoge2,ncol=2,byrow=TRUE)
hoge3
dim(hoge3)
hoge3[1,]
hoge3[,2]
hoge3[2,2]
hoge3<-as.data.frame(hoge3)
str(hoge3)
colnames(hoge3)<-c("A","B")

#6.2.4
install.packages("tidyverse")
library(tidyverse)
hoge3<-as_tibble(hoge3)
hoge3
getwd()
install.packages("styler")
