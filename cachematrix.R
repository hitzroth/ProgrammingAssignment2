## creates a matrix object with a cacheable inverse feature
## and a function to manipulate the cacheable inverse

## function that creates a matrix object that can both
## read and write a matrix and that matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  # start with a null inverse
  inverse <- NULL
  # alter the matrix and reset the inverse with scoping assignment
  set <- function(y) {
    # alter the matrix to the new state
    x <<- y
    # reset the inverse to null when matrix is altered
    inv <<- NULL 
  }
  # return the matrix
  get <- function() x 
  # set the cached inverse
  setinverse <- function(solve) inverse <<- solve
  # return the cached inverse
  getinverse <- function() inverse
  # navigate the matrix object
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## function to return the inverse of the matrix object
## the above function created

cacheSolve <- function(x, ...) {
  # get the cached inverse, if any
  inverse <- x$getinverse()
  # return the cached inverse if not null
  if(!is.null(inverse)) { 
    message("getting cached data")
    return(inverse)
  }
  # otherwise get the matrix  
  data <- x$get() 
  # find the inverse
  inverse <- solve(data, ...) 
  # cache the inverse
  x$setinverse(inverse) 
  # return the inverse
  inverse
}