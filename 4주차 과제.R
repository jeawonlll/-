####################################################과제1
#Mlbench 패키지의 Glass 자료에 대하여 
#k=2,3…10으로 하여 k-평균 군집법을 적용하고 
#군집내 오차제곱합과 군집간 오차제곱합의 비를 그려보아라.
#오차제곱합의 비에 의해 선택된 최적의 k 값에서의 k 평균 군집결과와 출력변수의 값을 비교하여라. 
#Hcluster로 나온군집결과와 k평균의 군집결과 또한 비교해 보시오

##데이터셋 가져와서 가공
#install.packages("mlbench")
#??mlbench
library(mlbench)
data("Glass")
str(Glass)

Glass_lable<-Glass$Type
Glass<-Glass[,-length(Glass)]
Glass<-scale(Glass)
summary(Glass)

##kmeans 해보기
set.seed(1234)
Glass_kmeans_2<-kmeans(x=Glass,centers =2)
Glass_kmeans_2$totss#총 오차 제곱합
Glass_kmeans_2$withinss#군집내 오차제곱합
Glass_kmeans_2$betweenss#군집간 오차제곱합
Glass_kmeans_2$betweenss/Glass_kmeans_2$totss*100#군집간오차제곱합/군집전체 오차제곱합
Glass_kmeans_2$tot.withinss/Glass_kmeans_2$betweenss#군집내 오차제곱합과 군집간 오차제곱합 비
#좋은 클러스터링은 군집간 오차제곱합이 크고
#군집내 오차제곱합이 적어야 한다

##kmeans 평가 by 총제곱합분의 군집간오차제곱합
x<-rep(0,10)
for( i in 1:10){
  set.seed(1234)
    Glass_kmeans<-kmeans(x=Glass,centers =i)
  x[i]<-Glass_kmeans$betweenss/Glass_kmeans$totss*100
}

plot(1:10,x,type="l",xlab = "군집의갯수",ylab="군집간오차제곱합/군집전체 오차제곱합")
abline(v=which.max(x),col="red")

## kmeans 평가 by 군집내 오차제곱합/군집간오차제곱합
y<-rep(0,9)
for( i in 2:10){
  set.seed(1234)
  Glass_kmeans<-kmeans(x=Glass,centers =i)
  y[i-1]<-Glass_kmeans$tot.withinss/Glass_kmeans$betweenss
}
y
which.min(y)
diff(y)
plot(2:10,y,type="l",xlab = "군집의수",ylab="총 군집내 오차제곱합/ 군집간 오차제곱합의 ")
abline(v=which.min(y)+1,col="red")
#################
x<-rep(0,9)
y<-rep(0,9)
for( i in 2:10){
  set.seed(0725)
  Glass_kmeans<-kmeans(x=Glass,centers =i)
  x[i-1]<-Glass_kmeans$tot.withinss
  y[i-1]<-Glass_kmeans$betweenss
}
par(mfrow=c(1,3))
plot(2:10,x,main="총 군집내 오차제곱합",xlab = "군집의수",ylab="총 군집내 오차제곱합",type="l")
plot(2:10,y,main="군집간 오차제곱합",xlab = "군집의수",ylab=" 군집간 오차제곱합 ",type="l")
plot(2:10,x/y,main="총 군집내 오차제곱합/ 군집간 오차제곱합",xlab = "군집의수",ylab="총 군집내 오차제곱합/ 군집간 오차제곱합 ",type="l")


##원래 type 과 kmwans-cluser 비교
set.seed(0725)
Glass_kmeans_6<-kmeans(x=Glass,centers =6)

par(mfrow=c(1,2))
library(cluster)
plot(silhouette(Glass_kmeans_6$cluster,dist=dist(Glass)^2),col=1:6)
plot(silhouette(as.numeric(Glass_lable),dist=dist(Glass)^2),col=1:6)


##hcluster 와비교
Glass_hmeans<-hclust(d=dist(Glass)^2,method = "average")
plot(Glass_hmeans,hang=-1)
rect.hclust(Glass_hmeans,k=6,border="red")
plot(silhouette(cutree(Glass_hmeans,k=6),dist=dist(Glass)^2),col=1:6)

result<-NULL
result$real<-Glass_lable
result$kmeans<-as.factor(Glass_kmeans_6$cluster)
result$hmeans<-as.factor(cutree(Glass_hmeans,k=6))

table(result$real,result$kmeans)
table(result$real,result$hmeans)
table(result$kmeans,result$hmeans)

###########################################################과제 2
#data 들어가면 hcluster 시각화 레이더망 플랏처럼 시각화하고 해석 
#데이터 시각화 - 레이더망 그래프
#클러스터링시각화 히트맵으로하셔도됩니다~^^! 구글에찾으면 많이나올꺼에요자료~
data(mtcars)
str(mtcars)

mtcars_hclust<-hclust(dist(scale(mtcars))^2,method = "single")
mtcars_hclust_label<-cutree(mtcars_hclust,k=5)
library(dplyr)
mtcars_plus<-mutate(mtcars=as.factor(mtcars_hclust_label))
head(mtcars_plus)

mtcars_plus<-group_by(mtcars_plus,as.factor(mtcars_hclust_label))


mtcars_meanbyclust<-summarise(mtcars_plus,
                              mean(mpg),mean(cyl),mean(disp),mean(hp),
                              mean(drat),mean(wt),mean(qsec),mean(vs),
                              mean(am),mean(gear),mean(carb))
View(mtcars_meanbyclust)

mtcars_hclust_label_sort<-sort(mtcars_hclust_label)
cluster<-data.frame("clust"=as.vector(mtcars_hclust_label_sort),"name"=names(mtcars_hclust_label_sort))

library(plotrix)
radial.plot(scale(mtcars_meanbyclust[,-1])*2*pi,labels=names(mtcars_meanbyclust)[-1],
            rp.type="p",main="plot",line.col=c("red","blue","green","yellow","black"),
            xlab = "",ylab = "",lwd=2,show.grid = T,show.grid.labels = F)
legend(x=30,y=-10,paste("cluster",1:5),col=c("red","blue","green","yellow","black"),lty=1,lwd=2)
legend(x=-60,y=20,cluster$name,col=rep(c("red","blue","green","yellow","black"),c(3,15,12,1,1)),lty=1)

x11()
#http://www.datamarket.kr/xe/index.php?mid=board_AGDR50&page=2&document_srl=222
#http://gastonsanchez.com/blog/how-to/2012/10/03/Dendrograms.html

#######################################과제
#USArrest data를 가지고(6개로 짤르고) 계층적 분석 돌리고 
#각 변수들끼리 총 6개의 플랏을 그리고
#그 각각의 변수에대한 자기의 생각을 적음 어떠어떠한변수가 영향이있더라 라는것
#계층적군집분석 옵션을 k=6로 하라는말 주장하기 힘들다면 k=3으로 바꿔라 
# 6가지 조합의 2차원그래프
#urbanpop을 제외한 3차원그래프를 그려와라 
#->어떠한 변수가 6개로 계층 군집하는데 중요하게 쓰였다

??USArrests
data(USArrests)
str(USArrests)
USArrests_scale<-scale(USArrests)
#클러스터링
USArrests_hcluser<-hclust(d=dist(USArrests_scale)^2,method = "single")
#그림그리기
plot(USArrests_hcluser,hang=-1)
rect.hclust(USArrests_hcluser,k=6,border="red")

USArrests_hcluser_lable<-cutree(USArrests_hcluser,k=6)
mypal = c("red", "blue", "black", "pink","yellow","green")

par(mfrow=c(2,3))
names(USArrests)
plot(USArrests[,1],USArrests[,2],type="n",main="murder and assualt")
points(USArrests[,1],USArrests[,2],col=mypal[USArrests_hcluser_lable],pch="*",cex=2)
plot(USArrests[,1],USArrests[,3],type="n",main="murder and urbanpop")
points(USArrests[,1],USArrests[,3],col=mypal[USArrests_hcluser_lable],pch="*",cex=2)
plot(USArrests[,1],USArrests[,4],type="n",main="murder and rape")
points(USArrests[,1],USArrests[,4],col=mypal[USArrests_hcluser_lable],pch="*",cex=2)
plot(USArrests[,2],USArrests[,3],type="n",main="assualt and unbanpop")
points(USArrests[,2],USArrests[,3],col=mypal[USArrests_hcluser_lable],pch="*",cex=2)
plot(USArrests[,2],USArrests[,4],type="n",main="assualt and rape")
points(USArrests[,2],USArrests[,4],col=mypal[USArrests_hcluser_lable],pch="*",cex=2)
plot(USArrests[,3],USArrests[,4],type="n",main="urbanpop and rape")
points(USArrests[,3],USArrests[,4],col=mypal[USArrests_hcluser_lable],pch="*",cex=2)


plot(USArrests)#살인과공격강간은 상관관계가 있다 
cor(USArrests)#unbanpop은 다른변수들과 상관관계가 없다.

#변수 3개 선택 
USArrests_scale_3variable<-USArrests_scale[,-3]
#클러스터링
USArrests_hcluser_3variable<-hclust(d=dist(USArrests_scale_3variable)^2,method = "single")
USArrests_hcluser_lable_3variable<-cutree(USArrests_hcluser_3variable,k=6)
#그림그리기
par(mfrow=c(1,1))
plot(USArrests[,1],USArrests[,2],type="n",xlab="murder",ylab="assualt")
points(USArrests[,1],USArrests[,2],col=mypal[USArrests_hcluser_lable_3variable],pch="*",cex=1)
plot(USArrests[,1],USArrests[,4],type="n",xlab="murder",ylab="rape")
points(USArrests[,1],USArrests[,4],col=mypal[USArrests_hcluser_lable_3variable],pch="*",cex=1)
plot(USArrests[,2],USArrests[,4],type="n",xlab="assualt",ylab="rape")
points(USArrests[,2],USArrests[,4],col=mypal[USArrests_hcluser_lable_3variable],pch="*",cex=1)

#k를 3으로 
USArrests_hcluser_lable_3variable_k3<-cutree(USArrests_hcluser_3variable,k=3)

plot(USArrests[,1],USArrests[,2],type="n",xlab="murder",ylab="assualt")
points(USArrests[,1],USArrests[,2],col=mypal[USArrests_hcluser_lable_3variable_k3],pch="*",cex=2)
plot(USArrests[,1],USArrests[,4],type="n",xlab="murder",ylab="rape")
points(USArrests[,1],USArrests[,4],col=mypal[USArrests_hcluser_lable_3variable_k3],pch="*",cex=2)
plot(USArrests[,2],USArrests[,4],type="n",xlab="assualt",ylab="rape")
points(USArrests[,2],USArrests[,4],col=mypal[USArrests_hcluser_lable_3variable_k3],pch="*",cex=2)
#클러스터 3개로 나누는건 의미가없다

#
par(mfrow=c(1,1))
plot(USArrests_hcluser_3variable,hang=-1)
rect.hclust(USArrests_hcluser,k=3,border="blue")
install.packages("rgl")
library(rgl)
plot3d(USArrests$Murder,USArrests$Assault,USArrests$Rape, col=mypal[USArrests_hcluser_lable_3variable], size=1, type='s', box=T)
#해석 검은점으로 나타낸 클러스터는 전반적인 3가지  특성의 범죄가 모두  낮은곳이고
#빨간점으로 나타나낸 클러스터는 전반적으로 3가지  특성의 범죄가 모두  높은곳이다.

## Do the same with centroid clustering and squared Euclidean distance,
## cut the tree into ten clusters and reconstruct the upper part of the
## tree from the cluster centers.
#hc <- hclust(dist(USArrests)^2, "cen")
#memb <- cutree(hc, k = 10)
#cent <- NULL
#for(k in 1:10){
#  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
#}
#hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
#plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
#plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
