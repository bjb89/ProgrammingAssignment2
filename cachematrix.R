## These functions are built to maintain a correct value for the inverse
## of a matrix.  The value of the inverse is cached, so that, when
## the inverse is requested, it is only recalculated if the original matrix
## has changed.

## function takes as input a matrix (should be non-singular) and returns
## a list of 4 functions, which (1) set the value of the matrix, (2) get
## the value of the matrix, (3) set the inverse, (4) get the value of inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(i) inv <<- i
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## function expects a list of 4 functions from makeCacheMatrix
## if inverse is already set in the list, function returns that
## else, the inverse is recalculated and returned.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
        ## Return a matrix that is the inverse of 'x'
}
