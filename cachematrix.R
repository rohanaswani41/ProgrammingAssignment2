
#make Cache matrix is a function which will give methods to set and get the inverse and the matrix
makeCacheMatrix <- function(x = matrix()) {
    cacheinv<-NULL
    
    
  set <- function(userval = matrix()){
    x<<-userval
    cacheinv<<-NULL
    
  }
  
  get<-function() x
  
  
  setInverse<- function(inv){
    cacheinv <<- inv
    return(cacheinv)
  }
  
  getInverse<-function() cacheinv
  
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Write a short comment describing this function
##This function will check if there is already an inverse present in the matrix and if yes it returns it or it tries to solve it

cacheSolve <- function(x=cacheMatrix(1:4,nrows=2,ncol=2), ...) {
        ## Return a matrix that is the inverse of 'x'
      presentInverse <- x$getInverse()
      
      ## if the matrix is not null and it is a matrix 
      if(!is.null(presentInverse) && is.matrix(presentInverse)){
          return(presentInverse)
        
      }
      else if(!is.null(x$get()) && is.matrix(x$get())){
        ##get the matrix and solve its inverse
        newMatrix <- x$get()
        presentInverse<- solve(newMatrix)
        x$setInverse(presentInverse)
        return(presentInverse)
      }
      else{
        
        message("Something went wrong")
      }
  
}
