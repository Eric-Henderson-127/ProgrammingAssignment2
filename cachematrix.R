## The following functions define an object that contains a matrix and helper functions to allow for a cache-able inverse matrix
## and a wrapper function over the solve function that is capable of operating on the specially defined matrix object

makeCacheMatrix <- function(x = matrix()) {
  # Takes a matrix object and return a list object which contains the supplied matrix as well as helper functions
  # which supports caching of the solve function output (the inverse matrix)
  #
  # Args:
  #   x: An invertable matrix
  #
  # Returns:
  #   A list object containing the input matrix and helper functions to store the inverse matrix
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
  # Takes a list object, as returned from makeCacheMatrix, determines if an inverse matrix is already cached,
  # if not already cached the function calculates the inverse matrix then caches the result, and then returns the
  # cached inverse matrix
  #
  # Args:
  #   x: A list object containing an invertable matrix and helper functions to cache/read an inverted matrix
  #   ...: Other arguments to be passed to solve function
  #
  # Returns:
  #   output of solve(x, ...) or cached output from prior function call
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
