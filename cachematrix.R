## This file contains the makeCacheMatrix and cacheSolve functions required for
## completion of the R Programming - Week 3 programming assignment.

## makeCacheMatrix creates a cached matrix object, given a matrix, for use with
## the cacheSolve function defined below.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Given a cached matrix object created by makeCacheMatrix, return the inverse
## of the matrix, either using the a cached value if present, or by calculating
## it using solve().

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinverse(i)
  i
}
