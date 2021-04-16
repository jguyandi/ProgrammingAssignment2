## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a matrix/list containing a function to 
## set the matrix’s value, get this value, set the inverse matrix 
## value, and get this value.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the
## inverse by returning the cached data from the first function, 
## if the function inverse matrix exists. If not, it solves the 
## inverse matrix through the solve function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
