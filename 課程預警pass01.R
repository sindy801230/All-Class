setwd('/Users/sharon/Documents/銘傳大學/專案研究/課程預警/MOODLE預測學生課程通過/Moodle學習平台/MOODLE預測分析1070425')
library(readxl)
dat104 <- read_excel('1070430Newdata.xlsx',sheet = '104')
dat105 <- read_excel('1070430Newdata.xlsx',sheet = '105')
dat105 <- read_excel('mculog.xlsx',sheet = '105')

unique(dat105$cour) #cour namber
#d4 <- dat104[grep('05212-1',dat104$cour),]
d5 <- dat105[grep('16345',dat105$cour),c(1,2,10:16,31)]
d5 <- dat105[grep('39212',dat105$cour),c(2:11)]

#步驟1 cor(d$國文, d$系PR值)
v <- cor(d5$`6view`, d5$sco) #3
p <- cor(d5$`6post`, d5$sco) #4
rep <- cor(d5$`6reply`, d5$sco) #5
vf <- cor(d5$`6view_forum`, d5$sco) #6
vp <- cor(d5$`6view_post`, d5$sco) #7
res <- cor(d5$`6resource`, d5$sco) #8
u <- cor(d5$`6upload`, d5$sco) #9

t <- cbind(v,p,rep,vf,vp,res,u)
min(t)
#is.na(t)
t[,is.na(t)==T]
#t[,t == min(t)]
nd5<- d5[-c(4,9)] #刪除相關性最低的欄位因子

#步驟2 所有欄位與成績Z分佈
#a <- nd5[,c(3:9)] 
length(nd5)
a <- nd5[,-c(1,2)]
a <- scale(a)
a<- as.data.frame(a,na.rm=TRUE)

#步驟3 所有欄位加總與成績相乘
a1<- rowSums(a[,-length(a)])
a2 <- a[,length(a)]
a3 <- a1*a2

#步驟4 刪除負值

order(a3) #由小到大
a4 <- a[which(a3 >= 0),]

#利用for迴圈除一找出預測較準的recall rate
#先分訓練與測試
#n <- nrow(a4)/2
#train <- a4[1:n,]
#test <- a4[(n+1):nrow(a4),]

id <- 1:trunc(nrow(a4) * 0.8)
train <- a4[id, ]
test <- a4[-id,]

a5 <- train[order(train[,length(train)]),] #重新排序
m <- median(a5[,4])
min(which(a5$sco >= m))

n <- a5[c(min(which(a5$sco >= m)):nrow(a5)),]
#n <- a5[c(18:30),]
n1 <- n$sco
n1

length(n1)
length(unique(n1))

pe <- function(x){
library(dplyr)
a61 <- mutate(a5, newpass = ifelse(a5$sco >= x, "Y", "N"))
a62 <- mutate(test, newpass = ifelse(test$sco >= x, "Y", "N"))
a11 <- a61
a12 <- a62


rownames(a11)
rownames(a12)

a11$newpass <- as.factor(a11$newpass)
a12$newpass <- as.factor(a12$newpass)

library(rpart)
wdbc.tree=rpart(newpass~.,data=a11)


#wdbc.tree=tree(newpass~.,data=a2)
test.pred=predict(wdbc.tree,newdata=a12, type='class')
(table.test=table(a12$newpass,test.pred))
cat("Total records(test)=",(table.test[1,1]+table.test[1,2]),"\n")

tp <- table.test[2, 2]
tn <- table.test[1, 1]
fp <- table.test[2, 1]
fn <- table.test[1, 2]
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標
print(c(accuracy,recall,precision,f1))
}


lapply(n1,pe)
length(grep('N',train$newpass)) #訓練集
length(grep('N',test$newpass))#測試集
