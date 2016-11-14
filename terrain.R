world.dim<-function(dim,ele,var){
  foo<-(2^dim)+1
  my.world<-matrix(nrow=foo,ncol=foo)
  # setting the dimensions of the matrix.
  summit<-replicate(4,rnorm(4,mean=ele,sd=var))
  # generating the values for the corners.
  # The user may choose the desired elevation and the amount of variation
  # desired.
  my.world[c(1,foo),c(1,foo)]<-sample(summit,4)
  # inserting a sample of the random values generated.
  return(my.world)
}
terra<-world.dim(3,5,5)

diamond.step<-function(terra){
  bar<-ceiling(ncol(terra)/2)
  
  #' concatenate the corners.
  x<-mean(terra[c(1,(nrow(terra))),c(1,(ncol(terra)))])+rnorm(1)
  
  #'average the corners with a little bit of noise added.
  terra[bar,bar]<-x
  return(terra)
}


square.step<-function(terra){
  bar<-ceiling(ncol(terra)/2)
  
  center<-terra[bar,bar]
  ne<-terra[1,1]
  se<-terra[nrow(terra),1]
  sw<-terra[nrow(terra),ncol(terra)]
  nw<-terra[1,ncol(terra)]
  
  e.mean<-mean(c(ne,se,center))+rnorm(1)
  s.mean<-mean(c(se,sw,center))+rnorm(1)
  w.mean<-mean(c(se,nw,center))+rnorm(1)
  n.mean<-mean(c(ne,nw,center))+rnorm(1)
  
  terra[mean(1:nrow(terra)),1]<-e.mean
  terra[max(1:nrow(terra)),mean(1:ncol(terra))]<-s.mean
  terra[mean(1:nrow(terra)),max(ncol(terra))]<-w.mean
  terra[1,mean(1:ncol(terra))]<-n.mean
  return(terra)
}


diamond.square.step <- function(terra, lakes){
x<-as.integer(log2(ncol(terra)))
  for (i in 2^(x:1)){ 
    for (n.s in seq(1, ncol(terra)-1, by=i)) {  
      for (w.e in seq(1, nrow(terra)-1, by=i)) {
        terra[w.e:(w.e+i),n.s:(n.s+i)] <- diamond.step(terra[w.e:(w.e+i),n.s:(n.s+i)])
        terra[w.e:(w.e+i),n.s:(n.s+i)] <- square.step(terra[w.e:(w.e+i),n.s:(n.s+i)])
      }
    }
  }
if(lakes==TRUE){
terra[terra<0]<-NA
}
return(terra)
}
terra<-diamond.square.step(terra,TRUE)



