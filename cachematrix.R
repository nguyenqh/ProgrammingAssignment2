## This script provides functions for caching the inverse of a matrix
## makeCacheMatrix creates a special "matrix" object that can cache its inverse
## cacheSolve return the inverse of the "matrix" object, either newly calculated or cached

## Function description:
# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL             # initialize the inverse to NULL (not computed)            
    set <- function(y) {    
      x <<- y               # set the matrix to a given matrix y
      inv <<- NULL          # initialize the inverse
    }
    get <- function() x     # get the matrix 
    setinverse <- function(invx) inv <<- invx   # set the inverse to a given matrix invx
    getinverse <- function() inv                # return the inverse
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)               # return a list of functions
}


## Function description:
# This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), 
# then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()             # get the cached inverse
    if(!is.null(inv)) {               # if the inverse has been computed
      message("getting cached data")
      return(inv)                     # just return the cached value and exit the function
    }
    data <- x$get()                   # if the inverse has NOT been computed, get the matrix
    inv <- solve(data, ...)           # calculate the inverse
    x$setinverse(inv)                 # cache the calculated inverse
    inv                               # return the newly computed inverse
}