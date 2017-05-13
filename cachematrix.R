## makeCacheMatrix and cacheSolve are functions to be used in order to prevent
## the cost of calculating multiple times the inverse of a same matrix by using a caching system

## Function that creates a special "matrix" object that can cache its inverse
## by the use of setters and getters.

makeCacheMatrix <- function(x = matrix()) {
  i   <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setsolve <- function(inverse) i <<- inverse
  getsolve <- function() i
  return(list(set = set, get = get, setsolve = setsolve, getsolve = getsolve))
}


## Function that computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  i <- x$getsolve()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setsolve(i)
  return(i)
}
