## Put comments here that give an overall description of what your
## functions do

## Contains functions that get and set the inverse value of a matrix.
## If the inverse matrix has already been calculated, it skips the computation 
## below

makeCacheMatrix <- function(x = matrix()) {
  i <- null
  set <- function(y) {
    x <<- y
    i <<- null
  }
  get <- function() x
  setInverse <- function(solve) i <<- solve
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Checks to see if the inverse is already calculated. If not, find and set it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(x)
  x$setInverse(i)
  i
}
