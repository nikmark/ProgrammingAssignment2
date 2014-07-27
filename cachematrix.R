## Functions that cache the inverse of a matrix

## Create a special matrix object for caching the value of the inverse,
## with setter and getter for both matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
  
  # Field for containing the invese of the matrix
  inverse <- NULL
  
  # Function for setting the value of the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  # Function for getting the value of the matrix
  get <- function() x
  
  # Function for setting the inverse of the matrix
  setinverse <- function(setInverse) {
    inverse <<- setInverse
  }
  
  # Function for get the inverse
  getinverse <- function() inverse
  
  # Return a list of all the above functions
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  
}


## Compute the inverse of the matrix returned by the previous function.
## If the inverse has already been calculated, then the function 
## should retrieve the cached value of the inverse.

cacheSolve <- function(x, ...) {
  
  inverse <- x$getinverse()
  
  # Check if there is the cached inverse of teh matrix
  if(!is.null(inverse)) {
    message("getting cached inverse")
    return(inverse)
  }
  
  # If not compute the inverse and cache the value
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setinverse(inverse)
  
  ## Return a matrix that is the inverse of 'x'
  inverse
}
