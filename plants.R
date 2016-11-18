#' @title Inserts plant species into the terrain matrix.
#'
#' @description Simulation of plant species placed on the terrain matrix
#' built in 'terrain.r'. Where there is terrain, the plant species
#' initially survive, then reproduce, and eventually compete with each
#' other.
#' @param repduc the probability that the corresponding plant species
#' reproduces.
#' @param survive the probability that the corresponding plant species
#' survives.
#' @param comp.mat contains the competition parameters for the species
#' simulated.
#' @return initiate.plants objects
#' @importFrom stats rnorm
#' @examples
#' plants<-run.eco(terra=terrain.wrapper(6,TRUE),10,repduc=c(.4, .6),
#' survive=c(.58,.65))
#' @name plant.arr
#' @rdname plant.arr
#'
#' @export
initiate.plants<-function(repduc, survive,comp.mat, names=NULL){
  if(is.null(names))
    names <- letters[seq_along(repduc)]
    # store the letters of the vector length of the value 'repduc'
  if(length(repduc) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
    # stop the fxn and return error if the length of the two arguments
    #are not equivalent.
  repduc <- setNames(repduc, names)
    # set the values of repduc to the corresponding logical value of names.
  survive<- setNames(survive,names)
    # set the values of repduc to the corresponding logical value of names.
  return(list(repduc=repduc, survive=survive, names=names))

}
#' @param cell argument used to determine the given content of a matrix
#' cell.
#' @rdname plant.arr
#' @export
survive <- function(cell, info){
  #some code to check whether cell is empty or has water.
  if(is.na(cell)==TRUE){
    cell<-NA
  #if the cell has an NA it remains in the cell.
  }else{
    if(cell==''){
      for(i in sample(length(info$survive))){
      #takes one individual species and the corresponding survive value and
      #assigns it to i.
      if(runif(1) <= info$survive[i]){
      #if the survivival ratio of the species selected is greater than the
      #value produced by runif(1) the logical value that represents the
      #species selected is fed into the cell.
      cell<-names(info$survive[i])
  }else{
      cell<-''
      #if the survivival ratio of the species selected is less than the
      #value produced by runif(1) the logical value that represents the
      #species selected is fed into the cell.
    }
      }
    }
  }
  return(cell)
}
#' @param plants argument to call the 'plants' array in order to be
#' manipulated.
#' @param info argument used provide the information about all
#' simulated plant species.
#' @return the alterations done by plant.timestep
#' @rdname plant.arr
#' @export
plant.timestep <- function(plants, info){
  for(i in 1:ncol(plants)){
    for(j in 1:nrow(plants)){
      plants[j,i]<-survive(plants[j,i], info)
    }
  }
  #if ()
  #plants <- reproduce(j, i, plants, info)
  return(plants)
}
#' @param terra An \code{\link{world.dim}} object
#' @param timesteps amount of 'timesteps' being applied. Gives the
#' third dimension of the plants array.
#' @return plants array with simulated species inserted.
#' @rdname plant.arr
#' @export
run.eco<-function(terra, timesteps, repduc, survive){
  repduc<-repduc
  survive<-survive
  info<-initiate.plants(repduc,survive)
  plants <- array("", dim=c(dim(terra),timesteps+1))
  for(i in seq_len(dim(plants)[3]))
    plants[,,i][is.na(terra)] <- NA
  for(k in (seq_len(dim(plants)[3]-1)))
    plants[,,k] <- plant.timestep(plants[,,k], info)
    k<-k+1
  return(plants)
}
plants<-run.eco(terra,10,repduc=c(.4, .6),survive=c(.58,.65))
#my code here and my code in my package looks a little different.
#I had to alter the code in the package a bit in order to get it to 
#pass the check.


