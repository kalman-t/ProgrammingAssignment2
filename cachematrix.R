## These functions were made to illustrate lexical scoping and caching in R. Below, I created two functions that creates an object that stores a matrix and cache its inverse.

## makeCacheMatrix is a function that creates a list containing a function to set and get the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  set.inv <- function(solve) inv <<- solve
  get.inv <- function() inv
  list(set = set, get = get, set.inv = set.inv, get.inv = get.inv)
}


## cacheSolve is a function that gives the inverse of the matrix returned by the function makeCacheMatrix

cacheSolve <- function(x, ...) {
  inv <- x$get.inv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$set.inv(inv)
  inv
}
