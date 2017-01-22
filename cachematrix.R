## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## MakeCachematrix is function which will cache inverse of matrix provided as input argument to the function.
## makecacheMatrix requires input of type matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function
## This function again takes a matrix as input argument and check if its already there in cache and inverse is calculated 
## if TRUE cached inverse is returned  else its inverse will be calculated

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
