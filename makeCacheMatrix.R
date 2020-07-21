## makeCacheMatrix creates a list containing a function to
## set/get value of matrix and set/get inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x}
  setInverse<-function(inverse){inv<<-inverse}
  getInverse<-function(){inv}
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}

## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  inv<-x$getInverse()
  if(!in.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setInverse(inv)
  inv
}
