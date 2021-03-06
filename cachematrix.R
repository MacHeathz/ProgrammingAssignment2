## There's 4 functions in this file:
## makeCacheMatrix and cacheSolve provide caching functionality for the inverse of
## a matrix.
## makeVector and cachemean provide caching functionality for the mean of a vector and
## were provided as examples for the matrix caching functions.


## This function creates a special "matrix" object that can cache its inverse. It
## returns a list containing 4 getter and setter functions, you can call them using
## the dollar sign: i.e. m$get() returns a matrix object.
## 
## Arguments
## Optional argument m = matrix(): the matrix this list represents.
##
## Functions
## get(): returns the matrix of this list
## set(y): set the matrix to matrix y 
## getInverse(): returns this matrix's inverse
## setInverse(y): Sets the matrix's inverse to value y
##
## Returns
## list with 4 functions.
##
makeCacheMatrix <- function(m = matrix()) {
  inv <- NULL
  get <- function() m
  set <- function(n) {
    m <<- n
    inv <<- NULL
  }
  getInverse <- function() inv
  setInverse <- function(inverse) inv <<- inverse
  list(get = get, set = set, getInverse = getInverse, setInverse = setInverse)
}

## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cacheSolve retrieves the
## inverse from the cache.
## 
## Arguments
## list x, created using makeCacheMatrix. Using cached inverse value if possible.
##
## Returns
## Numeric value with the inverse of the supplied list's matrix.
##
cacheSolve <- function(x, ...) {
  result <- x$getInverse()
  if (!is.null(result)) {
    message("Getting cached inverse")
    return(result)
  }
  data <- x$get()
  result <- solve(data, ...)
  x$setInverse(result)
  result
}

#################### EXAMPLE FUNCTIONS BELOW ########################################

## MakeVector returns a list with some getter and setter functions for it's innards,
## a value x and it's mean.
## 
## Arguments
## Optional argument x = numeric(): the vector this list represents
##
## Functions
## get(): returns the numeric vector of this list
## set(y): set the vector to numeric vector y 
## getmean(): returns this vector's mean
## setmean(y): Sets the vector's mean to value y
##
## Returns
## list with 4 functions
##
makeVector <- function(x = numeric()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean)
}

## Returns the mean of vector x, using it's cache if possible.
## 
## Arguments
## list x, from makevector. Using cached mean value if possible.
##
## Returns
## Numeric value with the mean of the supplied vector.
cachemean <- function(x, ...) {
  m <- x$getmean()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- mean(data, ...)
  x$setmean(m)
  m
}
