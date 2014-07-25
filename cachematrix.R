## This function creates a secial "Matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  
  set<-function(y){
    ## function to set a matrix
    x<<-y
    m<<-NULL
    
  }
  
  get<-function() x
## the above line will return the actual Matrix that you passed
  setMatrixInverse<-function(solve) m<<-solve
## the above line will assign the inverse matrix to m
  getMatrixInverse<-function() m
## the above line will return the inverse matrix from cache
  
  list(set=set, get=get, setMatrixInverse=setMatrixInverse, 
       getMatrixInverse=getMatrixInverse)
}


## This function computes the inverse of the special "Matrix"
## returned by the makeCacheMatrix function above. If the
## inverse has already been calculated (and matrix has not changed),
## then cacheSolve should retreive the inverse from cache

cacheSolve <- function(x, ...) {
## Input to this function is the output of makeCacheMatrix function        
## Return a matrix that is the inverse of 'x'
  m<-x$getMatixInverse()
  if(!is.null(m)){
    message("getting cache data")
    return(m)
    
  }
  
  data<-x$get()
  m<-solve(data,...)
  x$setMatrixInverse(m)
  m
}
