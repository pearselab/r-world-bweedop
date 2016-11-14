initiate.herbivore<-function(repduc, eat,comp.mat, names=NULL){
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