## Put comments here that give an overall description of what your
## functions do

## This function creates 4 sub-function all stored within the makeCacheMatrix environment, which is itself contained in the global environment.

makeCacheMatrix <- function(mtrx = matrix()) {
  invrs <- NULL
  # Create the 'set' sub-function
  set <- function(y) {
    mtrx <<- y
    invrs <<- NULL
  }
  # Create the sub-function which retrieves the matrix
  get <- function() mtrx
  # Create the inverse making sub-function
  setinverse <- function(inverse) invrs <<- inverse
  # Finally, we create the sub-function that retrieves the inverse of the matrix
  getinverse <- function() invrs
  # And here's the list of sub-functions returned by the main function
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function accesses the makeCacheMatrix environment to solve for the matrix inverse, which is then stored in the cache.

cacheSolve <- function(x, ...) {
  # get the inverse from the other function
  invs <- x$getinverse()
  # Check for cached data, let us know if it finds any, then gives it to us.
  if(!is.null(invs)) {
    message("Retrieving cached data")
    return(invs)
  }
  # If there is no cached data, then we calculate the inverse here, cache it, and report it back.
  matr <- x$get()
  invs <- solve(matr, ...)
  x$setinverse(invs)
  invs
}
