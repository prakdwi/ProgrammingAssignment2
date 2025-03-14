## This script contains two functions: 
## 1. makeCacheMatrix - Creates a special matrix object that can cache its inverse.
## 2. cacheSolve - Computes the inverse of the special matrix and caches it for future use.

## makeCacheMatrix creates a special matrix object that can store its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Variable to store the inverse
  
  set <- function(y) {
    x <<- y    # Assign new matrix value
    inv <<- NULL  # Reset cached inverse
  }
  
  get <- function() x  # Function to return the matrix
  
  setInverse <- function(inverse) inv <<- inverse  # Store the computed inverse
  
  getInverse <- function() inv  # Retrieve the cached inverse
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve computes the inverse of the matrix stored in makeCacheMatrix.
## If the inverse is already cached, it retrieves it instead of computing again.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)  # Return cached inverse
  }
  
  data <- x$get()
  inv <- solve(data, ...)  # Compute inverse
  x$setInverse(inv)  # Store inverse in cache
  
  inv
}
