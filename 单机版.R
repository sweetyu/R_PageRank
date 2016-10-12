#构建邻接矩阵
adjacencyMatrix<-function(pages){
  n<-max(apply(pages,2,max))
  A <- matrix(0,n,n)
  for(i in 1:nrow(pages)) A[pages[i,]$dist,pages[i,]$src]<-1
  A
}

#变换概率矩阵（不考虑阻尼系数的情况）
probabilityMatrix<-function(G){
  cs <- colSums(G)
  cs[cs==0] <- 1
  n <- nrow(G)
  A <- matrix(0,nrow(G),ncol(G))
  for (i in 1:n) A[i,] <- A[i,] + G[i,]/cs
  A
}

#变换概率矩阵(考虑阻尼系数d的情况)
dProbabilityMatrix<-function(G,d=0.85){
  cs <- colSums(G)
  cs[cs==0] <- 1
  n <- nrow(G)
  delta <- (1-d)/n
  A <- matrix(delta,nrow(G),ncol(G))
  for (i in 1:n) A[i,] <- A[i,] + d*G[i,]/cs
  A
}

#递归计算矩阵特征值
eigenMatrix<-function(G,iter=100){
  iter<-10
  n<-nrow(G)
  x <- rep(1,n)
  for (i in 1:iter) x <- G %*% x
  x/sum(x)
}

pages<-read.table(file="E:\\R\\PageRank\\page.csv",header=FALSE,sep=",")
names(pages)<-c("src","dist");pages
M<-adjacencyMatrix(pages);M

G<-probabilityMatrix(M);G    #方案一
Q<-eigenMatrix(G,100);Q     

G<-dProbabilityMatrix(M);G   #方案二
Q<-eigenMatrix(G,100);Q 
#Q<-calcEigenMatrix(G);Q  