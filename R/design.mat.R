design.mat<-function(book, locations) {
  n<-nrow(book)
  m<-length(locations)
  X<-matrix(1,c(n,1))
  colnames(X)<-"constant"
  for(j in 1:length(locations)){
    beta<-book[,locations[j]]
    beta<-as.factor(beta)
    nivel<-  levels(beta)
    P<-as.character(beta)
    for(i in 1:n){
      xadd<-c(as.numeric(nivel%in%P[i]))
      if(i==1)Y<-xadd
      if(i> 1)Y<-rbind(Y,xadd)
    }
    colnames(Y)<-nivel
    X<-cbind(X,Y)
  }
  rownames(X)<-1:n
  return(X)
}
