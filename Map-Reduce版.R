#构建邻接矩阵
adjacencyMatrix<-function(pages){
  n<-max(apply(pages,2,max))
  A <- matrix(0,n,n)
  for(i in 1:nrow(pages)) A[pages[i,]$dist,pages[i,]$src]<-1
  A
}

#Map流程
map<-function(S0 , node){
  S <- apply(S0,2,function(x) x/sum(x))
  if(node=="a")
    S[,1:2]
  else
    S[,3:4]
}

#Reduce流程
reduce<-function(A,B,d=0.85,iter=100){
  n <- nrow(A)
  q <- rep(1,n)
  Ga <- d * A + (1-d)/n * (A[A==0]=1)
  Gb <- d * B + (1-d)/n * (B[B==0]=1)
  for(i in 1:iter){
    qa <- as.matrix(q[1 : ncol(A)])
    qb <- as.matrix(q[(ncol(A)+1) : n])
    q <- Ga %*% qa + Gb %*% qb
  }
  q/sum(q)
}

pages<-read.table(file="E:\\R\\PageRank\\page.csv",header=FALSE,sep=",")
names(pages)<-c("src","dist");pages
M<-adjacencyMatrix(pages);M

A<-map(M,"a")
B<-map(M,"b")
Q<-reduce(A,B);Q