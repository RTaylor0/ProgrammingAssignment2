## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y;
    i <<- NULL;
  }
  get <- function() return(x);
  set_inverse <- function(inverse) i <<- inverse;
  get_inverse <- function() return(i);
  return(list(set = set, get = get, set_inverse = set_inverse, get_inverse = get_inverse))

}


## This function computes the inverse of the special "matrix" returned by

cacheSolve <- function(x, ...) {
  i <- x$get_inverse()
  if(!is.null(i)) {
    message("Getting cached data...")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$set_inverse(i)
  return(i)
}
