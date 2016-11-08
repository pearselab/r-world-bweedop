initiate.plants<-function(repduc, survive,comp.mat, names=NULL){
  if(is.null(names))
    names <- letters[seq_along(repduc)]
    # store the letters of the vector length of the value 'repduc'
  if(length(reproduction) != length(survive))
    stop("Reproduction and survival parameters needed for all species")
    # stop the fxn and return error if the length of the two arguments 
    #are not equivalent.
  repduc <- setNames(repduc, names)
    # set the values of repduc to the corresponding logical value of names.
  return(list(repduc=repduc, survive=survive, comp.mat=comp.mat,
              names=names))
}

survive <- function(cell, info){
  #...some code to check whether cell is empty or has water...
  if (cell = "" | cell=NA){
    
  }
  if(runif(1) <= info$survive[plant])
    #$The plant survived! so do something...
}

plant.timestep <- function(plants, terrain, info){
  survive <- function(plant, info){
    #...survive function...
  }
  #...looping et al...
  return(new.plants.matrix)
}

reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  Filter
  #Filter()
  
  
  # competition (needs to be within the reproduction fxn)
  sample(species_names, 1, prob=comp.mat[row,column])
  
  return(plants)
}

