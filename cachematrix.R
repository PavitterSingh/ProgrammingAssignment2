## This function creates a secial "Matrix" object that
## can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m<-NULL
  as.data.frame(m)
  set<-function(y){
    ## function to set a matrix
    x<<-y
    m<<-NULL
    
  }
  
  get<-function() x
  setMatrixInverse<-function(solve) m<<-solve
  getMatrixInverse<-function() m
  
  list(set=set, get=get, setMatrixInverse=setMatrixInverse, 
       getMatrixInverse=getMatrixInverse)
}


## This function computes the inverse of the special "Matrix"
## returned by the makeCacheMatrix function above. If the
## inverse has already been calculated (and matrix has not changed),
## then cacheSolve should retreive the inverse from cache

cacheSolve <- function(x, ...) {
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
