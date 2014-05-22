## This script is designed to show a facet of lexical scoping using two
## functions and the <<- operator.

## This first function creates a 'special' object which is really a list
## of operations to calculate the inverse of a matrix and store it.
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of the 'special' matrix created above
## firstchecking to see if already present in which case that is returned
## and calculation skipped.  If not inverse calculated and stored in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message('getting cached data')
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
  
}
