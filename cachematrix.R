
## This function creates a special "matrix" object that can cache its inverse.
## The "<<-" operator search for the variable being assigned in parent environments
## alowing to modify the matrix (x) and the inverse (m) from the inner functions
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
      x <<- y
      m <<- NULL
  }
  get <- function() x
  setInverse <- function(matrixInverse) m <<- matrixInverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
## When the matrix changes, m is set to NULL and the inverse is recalculated
cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      m <- x$getInverse()
      if(!is.null(m)) {
          message("Getting cached data")
          return(m)
      }
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
}
