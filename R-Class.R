library(C50)
library(readxl)
a1026 <- read.csv("/Users/sharon/Documents/銘傳大學/專案研究/學生報到預測/data/app1026完整.csv")

name_1 <- unique(a1026$學院)
#法律 社科 健康 教語 設計 傳播 資訊 管理 觀光

#z<-subset(a1026,a1026$年度==105&a1026$學院=='法律', select =c(9:14,19))
#x<-subset(a1026,a1026$年度==106&a1026$學院=='法律', select =c(9:14,19))

#全校為單位
z<-subset(a1026,a1026$年度==104, select =c(9:14,19))
x<-subset(a1026,a1026$年度==105, select =c(9:14,19))

#install.packages("tree")
#length(grep('no',a$Results))
a <- z[,-7]
b <- x[,-7]

#z分佈(常態分佈) ####
mina1<-apply(a,2,min)
mina1 #print(minb1)
maxa1<-apply(a,2,max)
maxa1 #print(maxb1)  
a1 <- maxa1- mina1

minb1<-apply(b,2,min)
minb1 #print(minb1)
maxb1<-apply(b,2,max)
maxb1 #print(maxb1)  
b2 <- maxb1-minb1

a <- cbind(a,z$註冊)
b <- cbind(b,x$註冊)

## 1. 決策樹tree####
#install.packages("tree")
a1 <- a
b1 <- b
library(tree)
a1$`z$註冊` <- as.factor(a1$`z$註冊`)
b1$`x$註冊` <- as.factor(b1$`x$註冊`)
a1 <- a1[1:7315,]

wdbc.tree=tree(a1$`z$註冊`~.,data=a1
               #,tree.control(nobs=6,mincut = 1,minsize = 2,mindev = 0.01)
)
wdbc.tree
summary(wdbc.tree)
plot(wdbc.tree)
par(family = "STKaiti")
text(wdbc.tree)

#train confusion matrix
train.pred=predict(wdbc.tree,newdata=b1, type='class')
(table.train=table(b1$`x$註冊`,train.pred))
cat("Total records(train)=",(table.train[1,2]+table.train[1,3]),"\n")
cat("Correct Classification Ratio(train)=", table.train[3,2]/(table.train[2,1]+table.train[3,2])*100,"%\n")
all01 =data.frame(a1,Spec.Pred=train.pred) #可查看結果
#all02 =data.frame(b1,Spec.Pred=test.pred) #可查看結果

#準確度####
tp <- length(which(all01$z.註冊=='未到'&all01$Spec.Pred=='未到'))#未＝未
tn <- length(which(all01$z.註冊=='報到'&all01$Spec.Pred=='報到'))#來＝來
fp <- length(which(all01$z.註冊=='未到'&all01$Spec.Pred=='報到'))#未＝來
fn <- length(which(all01$z.註冊=='報到'&all01$Spec.Pred=='未到'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

# 2.C50分類樹計算####
a1 <- a
b1 <- b
a1$`z$註冊` <- gsub('報到','Y',a1$`z$註冊`)
a1$`z$註冊` <- gsub('未到','N',a1$`z$註冊`)
b1$`x$註冊` <- gsub('報到','Y',b1$`x$註冊`)
b1$`x$註冊` <- gsub('未到','N',b1$`x$註冊`)

a1$`z$註冊` <- as.factor(a1$`z$註冊`)

b1$`x$註冊` <- as.factor(b1$`x$註冊`)
colnames(a1)= c("C","E","M","S","N","PR","come")
colnames(b1)= c("C","E","M","S","N","PR","come")
a1 <- a1[1:7315,]
#winnow選特徵值
#a.C5=C5.0(a1$come~., data=a1, rules= F,control = C5.0Control(winnow = F))
a.C5 <- C5.0(a1$come~ ., data = a1, rules = TRUE)

summary(a.C5)
plot(a.C5)


p <- predict( a.C5, b1, type="class" )
(confus.matrix <- table(real=a1$come, predict=p))
(confus.matrix[2,2]/(confus.matrix[2,1]+confus.matrix[2,2])*100) 
#all03 =data.frame(a1,Spec.Pred=p) #可查看結果
#準確度####
tp <- confus.matrix[2, 2]
tn <- confus.matrix[1, 1]
fp <- confus.matrix[2, 1]
fn <- confus.matrix[1, 2]
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

# 3.CART分類迴歸樹 ####
#提醒：不要把數值換成因子、數量太少會無法訓練及預測。
library(rpart)
a1 <- a
b1 <- b
a1 <- a1[1:7315,]
# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(a1$`z$註冊`~. , data=a1,model = F)

require(rpart.plot)
par(family = "STKaiti") 
prp(cart.model,         # 模型
    faclen=0,           # 呈現的變數不要縮寫
    fallen.leaves=TRUE, # 讓樹枝以垂直方式呈現
    shadow.col="gray",  # 最下面的節點塗上陰影
    # number of correct classifications / number of observations in that node
    extra=2)  

pred <- predict(cart.model, newdata=b1, type="class")
# 用table看預測的情況
(confus.matrix <- table(real=a1$`z$註冊`, predict=pred))
(confus.matrix[3,3]/(confus.matrix[2,2]+confus.matrix[3,3])*100)

all04 =data.frame(b1,Spec.Pred=pred) #可查看結果
#準確度####
tp <- length(which(all04$x.註冊=='未到'&all04$Spec.Pred=='未到'))#未＝未
tn <- length(which(all04$x.註冊=='報到'&all04$Spec.Pred=='報到'))#來＝來
fp <- length(which(all04$x.註冊=='未到'&all04$Spec.Pred=='報到'))#未＝來
fn <- length(which(all04$x.註冊=='報到'&all04$Spec.Pred=='未到'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

# 4.ctree條件推論樹####
library(party)
a1 <- a
b1 <- b
a1$`z$註冊` <- gsub('報到','Y',a1$`z$註冊`)
a1$`z$註冊` <- gsub('未到','N',a1$`z$註冊`)
b1$`x$註冊` <- gsub('報到','Y',b1$`x$註冊`)
b1$`x$註冊` <- gsub('未到','N',b1$`x$註冊`)

a1$`z$註冊` <- as.factor(a1$`z$註冊`)

b1$`x$註冊` <- as.factor(b1$`x$註冊`)
colnames(a1)= c("C","E","M","S","N","PR","come")
colnames(b1)= c("C","E","M","S","N","PR","come")
a1 <- a1[1:7315,]

fit <- ctree(a1$come~. , 
             data=a1)

par(family = "STKaiti") 
plot(fit, main="Conditional Inference Tree")
table(predict(fit), b1$come)

fi_t <- table(predict(fit), b1$come)
#(fi_t[2,2]/(fi_t[2,2]+fi_t[2,1])) # 對角線的數量/總數量

all05 = data.frame(a1,Spec.Pred=predict(fit)) #可查看結果
#grep('Y',all05$Spec.Pred)

#準確度####
tp <- length(which(all05$come=='N'&all05$Spec.Pred=='N'))#未＝未
tn <- length(which(all05$come=='Y'&all05$Spec.Pred=='Y'))#來＝來
fp <- length(which(all05$come=='N'&all05$Spec.Pred=='Y'))#未＝來
fn <- length(which(all05$come=='Y'&all05$Spec.Pred=='N'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

#隨機森林樹####
#載入隨機樹森林package
#install.packages("randomForest")
library(randomForest)
set.seed(1117)

#(2)跑隨機樹森林模型
a1 <- a
b1 <- b
a1 <- a1[1:7315,]
randomforestM <- randomForest(factor(a1$`z$註冊`)~ ., data = a1, importane = T, proximity = T, do.trace = 100)

randomforestM
round(importance(randomforestM), 2)#衡量每一個變數對Y值的重要性，取到小數點第二位
pred<-predict(randomforestM,newdata=b1)  
pred_out_1<-predict(object=randomforestM,newdata=b1,type="prob")  #输出概率
table <- table(pred,b1$`x$註冊`)  
all06 = data.frame(b1,Spec.Pred=pred) #可查看結果

#準確度####
tp <- length(which(all06$x.註冊=='未到'&all06$Spec.Pred=='未到'))#未＝未
tn <- length(which(all06$x.註冊=='報到'&all06$Spec.Pred=='報到'))#來＝來
fp <- length(which(all06$x.註冊=='未到'&all06$Spec.Pred=='報到'))#未＝來
fn <- length(which(all06$x.註冊=='報到'&all06$Spec.Pred=='未到'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

#類神經網路####
a1 <- a
b1 <- b
a1 <- a1[1:7315,]
#安裝並載入class套件
library(class)
library(dplyr)
#(參數1)準備訓練樣本組答案
trainLabels <- b1$`x$註冊`
#(參數2)(參數3)去除兩個樣本組答案
knnTrain <- a1[, - c(7)]
knnTest <- b1[, - c(7)]
#計算k值(幾個鄰居)通常可以用資料數的平方根
kv <- round(sqrt(10))
kv

#(4)建立模型 
prediction <- knn(train = knnTrain, test = knnTest, cl = trainLabels, k = kv)
cm <- table(x = a1$`z$註冊`, y = prediction)#, dnn = c("實際", "預測"))
cm
all07 = data.frame(b1,Spec.Pred=prediction) #可查看結果

#準確度####
tp <- length(which(all07$x.註冊=='未到'&all07$Spec.Pred=='未到'))#未＝未
tn <- length(which(all07$x.註冊=='報到'&all07$Spec.Pred=='報到'))#來＝來
fp <- length(which(all07$x.註冊=='未到'&all07$Spec.Pred=='報到'))#未＝來
fn <- length(which(all07$x.註冊=='報到'&all07$Spec.Pred=='未到'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

#支持向量機####
#載入套件 
library(e1071)
a1 <- a
b1 <- b
a1 <- a1[1:7315,]

# 建立模型
svmM <- svm(a1$`z$註冊` ~ ., data = a1, probability = TRUE)

# 預測
results <- predict(svmM, b1, probability = TRUE)

# 評估
cm <- table(x = b1$`x$註冊`, y = results)
cm
all08 = data.frame(b1,Spec.Pred=results) #可查看結果
#準確度####
tp <- length(which(all08$x.註冊=='未到'&all08$Spec.Pred=='未到'))#未＝未
tn <- length(which(all08$x.註冊=='報到'&all08$Spec.Pred=='報到'))#來＝來
fp <- length(which(all08$x.註冊=='未到'&all08$Spec.Pred=='報到'))#未＝來
fn <- length(which(all08$x.註冊=='報到'&all08$Spec.Pred=='未到'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

#貝氏分類####
library(e1071)
a1 <- a
b1 <- b
a1 <- a1[1:7315,]
nbcm <- naiveBayes(a1$`z$註冊` ~ ., data = a1)
results <- predict(nbcm, b1)

# 評估
cm <- table(x = b1$`x$註冊`, y = results)
cm
all09 = data.frame(b1,Spec.Pred=results) #可查看結果

#準確度####
tp <- length(which(all09$x.註冊=='未到'&all09$Spec.Pred=='未到'))#未＝未
tn <- length(which(all09$x.註冊=='報到'&all09$Spec.Pred=='報到'))#來＝來
fp <- length(which(all09$x.註冊=='未到'&all09$Spec.Pred=='報到'))#未＝來
fn <- length(which(all09$x.註冊=='報到'&all09$Spec.Pred=='未到'))#來＝未
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標
