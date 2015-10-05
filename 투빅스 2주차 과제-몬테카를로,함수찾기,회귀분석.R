##몬테카를로 시뮬레이션
solution<-rep(0,100)
for ( i in 1:100){
  x<-runif(1000,0,1)
  y<-runif(1000,0,1)
  z<-exp(x-1)
  prob<-mean(y<=z)
  solution[i]<-prob
}
mean(solution)

##예측-knn알고리즘
car_train<-read.csv("cartrain.csv",stringsAsFactors = F)
car_test<-read.csv("cartest.csv",stringsAsFactors = F)
car_train_label<-car_train[,1]

car_train<-car_train[,c(-1,-2,-3,-6,-8,-9)]#kind year fuel a n 제거
car_test<-car_test[,c(-1,-2,-5,-7,-8)]#kind year fuel a n 제거

car_train<-scale(car_train)
car_test<-scale(car_test)


install.packages("class")
library("class")

car_pred<-knn(train=car_train,test = car_test,cl=car_train_label,k=3)
sum(car_pred)
install.packages("gmodels")
library(gmodels)

CrossTable(x=car_pred,y="진짜 데이터를 넣어주세요",prop.chisq = F)

####### 로직
a<-c(0,1,1,0,0,1,1,1,0,1,1)
b<-c(a,c(2,2,4,1,1,1,1,0,2))
c<-rep(c(1,0),c(8,7))

my<-function(x,n){
  x<-which(x%%2==1)
  n<-n-1
  return(x[c(diff(x,n)==n,rep(F,n))])
}

my(a,2)
my(a,3)
my(a,4)
my(b,2)
my(b,3)
my(b,4)
my(c,2)
my(c,3)
my(c,4)
