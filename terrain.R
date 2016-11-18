#' @title Topographical terrain represented by numeric values in each cell of
#' a matrix.
#'
#' @description Creates a matrix that is filled beginning with the corners by
#'    a diamond-square step algorithim. The values in each cell represent
#'    terrain altitudes. Lakes are signified by NAs within the matrix if
#'    the user chooses to implement them in terrain.
#' @param dim Size of the grid will be 2^dim+1 (e.g. dim=3; 9X9 grid).
#' @param lakes logical that determines whether or not the negative values
#'    are made to be NAs aka underwater (default: TRUE).
#' @param terra argument of the matrix 'terra' that is manipulated by 
#'    the corresponding functions.
#' @return a terrain matrix; numeric elements indicate heights, and
#'    NAs indicate cells filled with water.
#' @importFrom stats rnorm
#' @importFrom graphics image
#' @examples
#'    terra<-terrain.wrapper(6, TRUE)
#'    image(terra)
#' @name terrain
#' @rdname terrain
#'
#' @export
world.dim<-function(dim){
  foo<-(2^dim)+1
  terra<-matrix(nrow=foo,ncol=foo)
  # setting the dimensions of the matrix.
  summit<-replicate(4,rnorm(4,mean=abs(sample(rnorm(5))),sd=abs(sample(rnorm(1)))))
  # generating the values for the corners.
  # The user may choose the desired elevation and the amount of variation
  # desired.
  terra[c(1,foo),c(1,foo)]<-sample(summit,4)
  # inserting a sample of the random values generated.
  return(terra)
}

#' @rdname terrain
#' @export
diamond.step<-function(terra){
  bar<-ceiling(ncol(terra)/2)
  #finding the middle col
  x<-mean(terra[c(1,(nrow(terra))),c(1,(ncol(terra)))])+rnorm(1)
  #assigning the mean of the appropriate corners to a variable.
  terra[bar,bar]<-x
  return(terra)
}


#' @rdname terrain
#' @export
square.step<-function(terra){
  bar<-ceiling(ncol(terra)/2)
  #finding the middle col
  center<-terra[bar,bar]
  ne<-terra[1,1]
  se<-terra[nrow(terra),1]
  sw<-terra[nrow(terra),ncol(terra)]
  nw<-terra[1,ncol(terra)]
  #assigning each of the corners and center cells to a variable.
  e.mean<-mean(c(ne,se,center))+rnorm(1)
  s.mean<-mean(c(se,sw,center))+rnorm(1)
  w.mean<-mean(c(se,nw,center))+rnorm(1)
  n.mean<-mean(c(ne,nw,center))+rnorm(1)
  #assigning the mean of the appropriate corners and center value to a 
  # variable.
  terra[mean(1:nrow(terra)),1]<-e.mean
  terra[max(1:nrow(terra)),mean(1:ncol(terra))]<-s.mean
  terra[mean(1:nrow(terra)),max(ncol(terra))]<-w.mean
  terra[1,mean(1:ncol(terra))]<-n.mean
  return(terra)
}
#' @rdname terrain
#' @export
diamond.square.step <- function(terra){
  x<-as.integer(log2(ncol(terra)))
  #equation in order to get the original 'dim' value that is put in by the 
  #user.
  for (i in 2^(x:1)){
    #i is assigned to each of the values needed for the submatrices.
    for (n.s in seq(1, ncol(terra)-1, by=i)) {
      for (w.e in seq(1, nrow(terra)-1, by=i)) {
        terra[w.e:(w.e+i),n.s:(n.s+i)] <- diamond.step(terra[w.e:(w.e+i),n.s:(n.s+i)])
        terra[w.e:(w.e+i),n.s:(n.s+i)] <- square.step(terra[w.e:(w.e+i),n.s:(n.s+i)])
        #running the diamond.step and square.step on each submatrix of the 
        #terrain. Each iteration is fed into the corresponding submatrices of 
        #the terrain matrix.
      }
    }
  }
  return(terra)
}

#' @rdname terrain
#' @export
terrain.wrapper<-function(dim, lakes){
  terra<-world.dim(dim)
  terra<-diamond.square.step(terra)
  #feeding the world.dim and each iteration of diamond.square.step 
  #into the terra matrix.
  if(lakes==TRUE){
    terra[terra<0]<-NA
  }
  #conditional statement that is determined by the user. TRUE if 
  #bodies of water are wanted. FALSE if not.
  return(terra)
  image(terra)
}
terra<-terrain.wrapper(6, TRUE)
