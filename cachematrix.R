# Function to create a special matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  
  # Function to set the matrix value and invalidate the cached inverse
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Function to get the matrix value
  get <- function() x
  
  # Function to compute and cache the inverse of the matrix
  cacheInverse <- function() {
    if (!is.null(inverse)) {
      message("Getting cached inverse")
      return(inverse)
    }
    
    inverse <- solve(x)
    cacheInverse <<- inverse
    
    inverse
  }
  
  # Return the functions as a list
  list(set = set, get = get, cacheInverse = cacheInverse)
}

# Function to compute the inverse of a matrix using the cached value if available
cacheSolve <- function(x) {
  inverse <- x$cacheInverse()
  inverse
}
