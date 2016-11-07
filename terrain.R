world.dim<-function(dim,ele,var){
  my.world<-matrix(nrow=((2^dim)+1),ncol=((2^dim)+1))
  # setting the dimensions of the matrix.
  summit<-replicate(4,rnorm(4,mean=ele,sd=var))
  # generating the values for the corners.
  # The user may choose the desired elevation and the amount of variation
  # desired.
  my.world[1,1]<-sample(summit,1)
  my.world[1,((2^dim)+1)]<-sample(summit,1)
  my.world[((2^dim)+1),1]<-sample(summit,1)
  my.world[((2^dim)+1),((2^dim)+1)]<-sample(summit,1)
  # inserting a sample of the random values generated.
  return(my.world)
}
terra<-world.dim(3,100,10)  

diamond.step<-function(terra){
  #set values for the corners so they may be concatenated and averaged.
  ne<-terra[1,1]
  se<-terra[max(nrow(terra)),1]
  sw<-terra[max(nrow(terra)),max(ncol(terra))]
  nw<-terra[1,max(ncol(terra))]
  # concatenate the corners.
  x<-c(ne,sw,se,nw)
  # average the corners with a little bit of noise added.
  mid<-mean(x)+rnorm(1,1,1)
 for(i in 1:5){
  terra[mean(1:(nrow(terra))/i),mean((1:(ncol(terra))/i))]<-mid
}
  return(terra)
}
terra<-diamond.step(terra)

square.step<-function(){
  center<-terra[mean(1:nrow(terra)),mean(1:ncol(terra))]
  ne<-terra[1,1]
  se<-terra[max(nrow(terra)),1]
  sw<-terra[max(nrow(terra)),max(ncol(terra))]
  nw<-terra[1,max(ncol(terra))]
  a<-c()
  
}