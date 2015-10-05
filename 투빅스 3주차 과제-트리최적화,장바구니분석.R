#tree과제

##읽어오기
setwd("C:\\Users\\ss\\Desktop\\tobig\\3주차-의사결정,장바구니\\의사결정나무")
mushrooms<-read.csv("mushrooms.csv",stringsAsFactors = T)
head(mushrooms)
str(mushrooms)

library(tree)   
library(rpart) 

##트리만들기
mushrooms.tr<-tree(type~.,split = c("gini"),data=mushrooms)
summary(mushrooms.tr)
plot(mushrooms.tr);  text(mushrooms.tr) 

##가지치기
mushrooms.cv<-cv.tree(mushrooms.tr)
mushrooms.cv 
plot(mushrooms.cv) 
mushrooms.tr4<-prune.tree(mushrooms.tr,best =4)
plot(mushrooms.tr4);text(mushrooms.tr4)
summary(mushrooms.tr4)

##traing,test나누기
id<-sample(1:8124,round(8124*0.8))
traing_mushrooms<-mushrooms[id,]
test_mushrooms<-mushrooms[-id,]

##학습시키기
traing_mushrooms_tree<-tree(type~.,split = c("gini"),data=traing_mushrooms)
traing_mushrooms_tree_cv<-cv.tree(traing_mushrooms_tree)
traing_mushrooms_tree_cv
plot(traing_mushrooms_tree_cv)
traing_mushrooms_tree_best<-prune.tree(traing_mushrooms_tree,best=4)
plot(traing_mushrooms_tree_best);text(traing_mushrooms_tree_best)
summary(traing_mushrooms_tree_best)

##예측하기
predict_mushrooms<-predict(traing_mushrooms_tree_best,test_mushrooms,type="class")
test_mushromms_labels<-test_mushrooms[,1]

##결과보기
library("caret")
confusionMatrix(data=predict_mushrooms,reference=test_mushromms_labels )

#2.장바구니 분석
library(arules)
setwd("C:\\Users\\ss\\Desktop\\머신러닝\\dataset")
list.files()
read.transactions()
groceries<-read.transactions("groceries.csv",sep=",")
summary(groceries)
groceryrules<-apriori(groceries,parameter = list(support=0.006,confidence=0.25,minlen=2))
summary(groceryrules)
inspect(sort(groceryrules,by="lift")[1:5])
