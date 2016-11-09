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
  

diamond.step<-function(terra){
  bar<-ceiling(ncol(terra)/2)
  
  # concatenate the corners.
  x<-mean(terra[c(1,(nrow(terra))),c(1,(ncol(terra)))])+rnorm(1,1,1)
  
  # average the corners with a little bit of noise added.
  terra[bar,bar]<-x
  return(terra)
}

terra<-diamond.step(terra)

square.step<-function(terra){
  bar<-ceiling(ncol(terra)/2)
  
  center<-terra[bar,bar]
  ne<-terra[1,1]
  se<-terra[nrow(terra),1]
  sw<-terra[nrow(terra),ncol(terra)]
  nw<-terra[1,ncol(terra)]
  
  e.mean<-mean(c(ne,se,center))+rnorm(1,1,1)
  s.mean<-mean(c(se,sw,center))+rnorm(1,1,1)
  w.mean<-mean(c(se,nw,center))+rnorm(1,1,1)
  n.mean<-mean(c(ne,nw,center))+rnorm(1,1,1)
  
  terra[mean(1:nrow(terra)),1]<-e.mean
  terra[max(1:nrow(terra)),mean(1:ncol(terra))]<-s.mean
  terra[mean(1:nrow(terra)),max(ncol(terra))]<-w.mean
  terra[1,mean(1:ncol(terra))]<-n.mean
  return(terra)
}


diamond_square.step<-function(terra){
  terra<-world.dim(2,100,10)
  
  terra<-diamond.step(terra)
  
  terra<-square.step(terra)
}