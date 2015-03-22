

## makeCacheMatrix(x)
##
## Arguments: x -   square numeric or complex matrix containing the coefficients of the linear system.
##
## This function creates a matrix that can cache the computed value of
## its inverse.  To use it, pass in a matrix.  For example:
##
## > x <- c=rbind(c(1, -1/4), c(-1/4, 1))
## > y <- makeCacheMatrix(x)
##

makeCacheMatrix <- function(x = matrix()) {

  inverse_m <- NULL

  set <- function(y) {
    x <<- y
    inverse_m <<- NULL
  }

  get <- function() {
    return(x)
  }

  setInverse <- function(s) {
    inverse_m <<- s
  }

  getInverse <- function() {
    return(inverse_m)
  }

  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve(x)
##
## Arguments: x - a makeCacheMatrix matrix
##
## This function returns the inverse of the special given matrix, x, which
## is of class makeCacheMatrix().  If the matrix has not changed, then the function
## returns the cached value of the inverse matrix.  If the matrix has changed, then
## it recomputes the inverse first.
##

cacheSolve <- function(x, ...) {

  inv <- x$getInverse()

  if ( !is.null(inv) ) {
    message("getting cached inverse")
    return(inv)
  }

  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)

  return(inv)
}
