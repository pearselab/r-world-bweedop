
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
#'    plants<-run.eco(terra, 10)
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
  if(is.matrix(comp.mat)==F)
    stop("The competition is not a matrix")
  repduc <- setNames(repduc, names)
    # set the values of repduc to the corresponding logical value of names.
  survive<- setNames(survive,names)
    # set the values of repduc to the corresponding logical value of names.
  return(list(repduc=repduc, survive=survive, comp.mat=comp.mat,
              names=names))
}

repduc<-c(0.6,0.4)
survive<-c(0.6,0.5)
comp.mat<-matrix(nrow = length(repduc),ncol = length(survive))

info<-initiate.plants(repduc,survive,comp.mat)
#' @param cell argument used to determine the given content of a matrix
#' cell.
#' @param info argument used provide the information about all 
#' simulated plant species.
#' @rdname plant.arr
#' @export
survive <- function(cell, info){
  #...some code to check whether cell is empty or has water...
  if(is.na(cell)==TRUE){
    cell<-NA
  }else{ 
    if(cell==''){
      for(i in sample(length(info$survive))){
      if(runif(1) <= info$survive[i]){
      cell<-names(info$survive[i])
  }else{
      cell<-''
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
  return(plants)
}
#' @param terra argument to call the terrain matrix 'terra'
#' @param timesteps amount of 'timesteps' being applied. Gives the 
#' third dimension of the plants array.
#' @return plants array with simulated species inserted.
#' @rdname plant.arr
#' @export
run.eco<-function(terra, timesteps){
  plants <- array("", dim=c(dim(terra),timesteps+1))
  #...why timesteps+1, do you think?...
  for(i in seq_len(dim(plants)[3]))
    plants[,,i][is.na(terra)] <- NA
  for(k in seq_len(dim(plants)[3]))
    plants[,,k] <- plant.timestep(plants[,,k], info)


  return(plants)
}
plants<-run.eco(terra, 10)

