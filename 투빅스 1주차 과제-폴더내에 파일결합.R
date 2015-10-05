##투빅스 1주차 과제-폴더내에 파일결합

#디렉토리 설정
getwd()
post<-"C:\\Users\\ss\\Desktop\\tobig\\1주차" #workdirectory 주소 설정
setwd(post) 

#파일이름 저장
file<-list.dirs()

#파일 경로 지정
path<-paste0(post,file)

#결과를 저장할 빈데이터프레임 형성
csv=data.frame() 
csv_rbind=data.frame()
csv_rbind_cbind<-data.frame()
#파일 읽어오기
for( i in 2:length(path)){
  csv_list<-list.files(path[i])
  for( j in 1:length(csv_list)){
    csv<-read.csv(file=paste(path[i],csv_list[j],sep="/"))
    if(j==1){csv_rbind<-csv}else{csv_rbind<-rbind(csv_rbind,csv)}
    }
  if(i==2){
    csv_rbind_cbind<-csv_rbind
  }else{
    csv_rbind_cbind<-cbind(csv_rbind_cbind,csv_rbind)}
  }
dim(csv_rbind_cbind)==c(5*4,3*5)

#매트릭스,매트릭스 곱 해주기
csv_rbind_cbind_matrix<-as.matrix(csv_rbind_cbind)
csv_rbind_cbind_matrix_product<-csv_rbind_cbind_matrix%*%t(csv_rbind_cbind_matrix)
csv_rbind_cbind_matrix_product
#최대값대체
for(i in 1:5){
  csv_rbind_cbind_matrix_product[csv_rbind_cbind_matrix_product%in%max(csv_rbind_cbind_matrix_product)]=0
}

print(csv_rbind_cbind_matrix_product)
write.csv(csv_rbind_cbind_matrix_product,file = "final.csv")
View(csv_rbind_cbind_matrix_product)
