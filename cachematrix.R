## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.
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
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.
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

#################### EXAMPLE FUNCTIONS #############################################

## MakeVector returns a list with some getter and setter functions for it's innards,
## a value x and it's mean.
## 
## Arguments
## Optional argument x = numeric(): the vector this list represents
##
## Functions
## get: returns the numeric vector of this list
## set(y): set the vector to numeric vector y 
## getmean: returns this vector's mean
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