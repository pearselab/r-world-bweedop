
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
survive<-c(0.74,0.85)
comp.mat<-matrix(nrow = length(repduc),ncol = length(survive))

info<-initiate.plants(repduc,survive,comp.mat)

area.cells<-sample(c(info$names,'',NA))
plants<-matrix(area.cells,6,6)
plants<-plants[sample.int(nrow(plants)),sample.int(ncol(plants))]


survive <- function(cell, info){
  #...some code to check whether cell is empty or has water...
  if(is.na(cell)==TRUE)
    cell<-NA
  if((cell)=='')
    cell<-''
  for(i in 1:length(info$survive)){
  if(runif(1) <= info$survive[i]){
    #$The plant survived! so do something...
    cell<-names(info$survive[i])
  }else{
    cell<-""
  }
  }
  return(cell)
}

plant.timestep <- function(plants, terrain, info){
  survive <- function(cell, info){
  }
  for(i in 1:ncol(plants)){
    for(j in 1:nrow(plants)){
      plants<-survive(plants[j,i])
    }
  }
  #...looping et al...
  return(plants)
}
plant.timestep(plants)
plants <- array("", dim=c(dim(terrain),timesteps+1))
#...why timesteps+1, do you think?...
for(i in seq_len(dim(plants)[3]))
  plants[,,i][is.na(terrain)] <- NA

reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  Filter
  #Filter()
  
  
  # competition (needs to be within the reproduction fxn)
  sample(species_names, 1, prob=comp.mat[row,column])
  
  return(plants)
}



reproduce <- function(row, col, plants, info){
  possible.locations <- as.matrix(expand.grid(row+c(-1,0,1), col+c(-1,0,1)))
  #...now filter out which ones are not water-logged and reproduce there...
  #...being careful to check you do have somewhere to reproduce to!...
  return(plants)
}

sample(species_names, 1, prob=comp.mat[row,column])

unbalanced <- run.plant.ecosystem(terrain, 100, survive=c(.95,.95),
                                  repro=c(.4, .6), comp.mat=matrix(c(.7,.3,.3,.7),2))