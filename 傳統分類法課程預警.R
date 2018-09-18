setwd('/Users/sharon/Documents/銘傳大學/專案研究/課程預警/MOODLE預測學生課程通過/Moodle學習平台/MOODLE預測分析1070425')
library(readxl)
#dat104 <- read_excel('1070430Newdata.xlsx',sheet = '104')
dat105 <- read_excel('1070430Newdata.xlsx',sheet = '105')
dat105 <- read_excel('mculog.xlsx',sheet = '105')

unique(dat105$cour) #cour namber
#d4 <- dat104[grep('05212-1',dat104$cour),]
d5 <- dat105[grep('35108',dat105$cour),c(1,2,10:16,31)]
d5 <- dat105[grep('39212',dat105$cour),c(2:11)]


#z分佈(常態分佈) ####
a <- d5[,-c(1,2,length(d5))]
a <- scale(a)
a<- as.data.frame(a,na.rm=TRUE)
a <- cbind(a,d5$sco)

#刪除因子離群值
library(outliers)
a1 <- a[-outlier(a$`6view`),]
#a1 <- a1[-outlier(a$`6post`),]
a1 <- a1[-(outlier(a$`6reply`)),]
a1 <- a1[-outlier(a$`6view_forum`),]
a1 <- a1[-outlier(a$`6view_post`),]
a1 <- a1[-outlier(a$`6resource`),]
#a1 <- a1[-(outlier(a$`6upload`)),]

#分數門檻值訂定
library(dplyr)
a4 <- mutate(a1, newpass = ifelse(a1$`d5$sco` >= 70, "Y", "N"))

#各決策樹分析
id <- 1:trunc(nrow(a4) * 0.8)
train <- a4[id, ]
test <- a4[-id,]


## 1. 決策樹tree####
#install.packages("tree")

library(tree)
train$newpass <- as.factor(train$newpass)
test$newpass <- as.factor(test$newpass)
train <- train[-c(2,7,8)]
test <- test[-c(2,7,8)]

wdbc.tree <- tree(train$newpass~., data= train)
               
wdbc.tree
summary(wdbc.tree)
plot(wdbc.tree)
par(family = "STKaiti")
text(wdbc.tree)
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


# 2.C50分類樹計算####
a1 <- train
b1 <- test

a1$newpass <- as.factor(a1$newpass)

b1$newpass <- as.factor(b1$newpass)

#a1 <- a1[1:7315,]
#winnow選特徵值
#a.C5=C5.0(a1$come~., data=a1, rules= F,control = C5.0Control(winnow = F))
library(C50)
a.C5 <- C5.0(a1$newpass~ ., data = a1, rules = TRUE)

summary(a.C5)
plot(a.C5)


p <- predict( a.C5, b1, type="class" )
(table.test=table(b1$newpass,p))
cat("Total records(test)=",(table.test[1,1]+table.test[1,2]),"\n")
 
#all03 =data.frame(a1,Spec.Pred=p) #可查看結果
#準確度####
tp <- table.test[2, 2]
tn <- table.test[1, 1]
fp <- table.test[2, 1]
fn <- table.test[1, 2]
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標

# 3.CART分類迴歸樹 ####
#提醒：不要把數值換成因子、數量太少會無法訓練及預測。
library(rpart)
a1 <- train
b1 <- test
#a1 <- a1[1:7315,]
# CART的模型：把存活與否的變數(Survived)當作Y，剩下的變數當作X
cart.model<- rpart(a1$newpass~. , data=a1,model = F)

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
(confus.matrix <- table(real=b1$newpass, predict=pred))
cat("Total records(test)=",(confus.matrix[1,1]+confus.matrix[1,2]),"\n")

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


# 4.ctree條件推論樹####
library(party)
a1 <- train
b1 <- test


a1$newpass <- as.factor(a1$newpass)

b1$newpass <- as.factor(b1$newpass)

fit <- ctree(a1$newpass~. , 
             data=a1)

par(family = "STKaiti") 
plot(fit, main="Conditional Inference Tree")

(fit.matrix <- table(real= a1$newpass, predict=fit))
cat("Total records(test)=",(confus.matrix[1,1]+confus.matrix[1,2]),"\n")

#all03 =data.frame(a1,Spec.Pred=p) #可查看結果
#準確度####
tp <- fit.matrix[2, 2]
tn <- fit.matrix[1, 1]
fp <- fit.matrix[2, 1]
fn <- fit.matrix[1, 2]
accuracy <- ((tp + tn)/(tp + tn + fp + fn))*100 #準確率
recall <- (tp/(tp+fn))*100 #recall
precision <- (tn/(tn + fp))*100 #precision
f1 <- ((2*recall*precision)/(recall+precision)) ##F1-measure綜合評價指標
