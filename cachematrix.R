## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than computing it 
## repeatedly. The purpose of these functions is to cache the inverse of a 
## matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  x_i <- NULL
  set <- function(y) {
    x <<- y
    x_i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) x_i <<- inverse
  getinverse <- function() x_i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then cacheSolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_i <- x$getinverse()
  if(!is.null(x_i)) {
    message("getting cached data")
    return(x_i)
  }
  data <- x$get()
  x_i <- solve(data, ...)
  x$setinverse(x_i)
  x_i
}
