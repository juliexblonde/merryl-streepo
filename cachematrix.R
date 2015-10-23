## I can just barely tell you what these functions do.  There
## is the ability to create and replace a matrix.  There is 
## also the ability to invert the matrix and cache the inversion.
## These functions also serve to begin to sort of clarify
## the incredibly confusing world of scoping in R.


## The purpose of makeCacheMatrix is to create a matrix and 
## a cache, as well as some functions to replace and/or 
## invert those values

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmat <- function(matr) m <<- matr
  getinverse <- function() m
  list(set = set, get = get,
       setmat = setmat,
       getinverse = getinverse)
     
}


## cacheSolve returns the inverse of matrix 'x' into the
## cache 'm'.  

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setmat(m)
    m
  
}
