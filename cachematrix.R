## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # Initialize the cache
cache <- NULL
# Setter function to set the matrix
setMatrix <- function(newValue) {
  x <<- newValue
  cache <<- NULL  # Clear the cache when the matrix is set
}
# Getter function to get the matrix
getMatrix <- function() {
  x
}
cacheInverse <- function() {
  if (!is.null(cache)) {
    message("Getting cached inverse")
    return(cache)
  }
  if (is.null(x) || !is.matrix(x) || !all(dim(x) == c(ncol(x), ncol(x)))) {
    stop("Input is not a valid square matrix.")
  }
  message("Calculating and caching the inverse")
  cache <- solve(x)
  cache
}

  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse)
}


## Write a short comment describing this function
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  # Check if a cached inverse exists
  cached_inverse <- x$getInverse()
  # If a cached inverse exists, return it
  if (!is.null(cached_inverse)) {
    message("Retrieving cached inverse")
    return(cached_inverse)
  } else {
    message("Calculating the inverse since no cached inverse exists")
    # Calculate and cache the inverse
    inverse <- x$cacheInverse()
    return(inverse)
  }
        ## Return a matrix that is the inverse of 'x'
}
