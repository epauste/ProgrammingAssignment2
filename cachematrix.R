## The functions makeCacheMatrix and cacheSolve are designed to work together to allow potentially computationally heavy 
## procedures ( in this case the inversion of a square matrix ) to be cached after initial calculation, so that 
## the results can be retreived quickly when needed without the need to recalculate.
## Typical Usuage:
## x <- makeCacheMatrix(matrix(1:4,2,2))
## cacheSolve(x) - Note first use it will calculate and cache the inverted matrix
## cacheSolve(x) - Subsequent use, inverted matrix will be retreived from the cache

## makeCacheMatrix
## This function accepts one argument, a square invertible matrix ( note no checking is performed ).
## The reveived matrix will then be inverted, and its cachee will be stored.
## The function returns a list of functions that can be used to set / get the following data
## set - set matrix data
## get - get matrix data 
## setInverse - set inverted Matrix vals
## getInverse - get inverted Matrix vals
##

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setInverse <- function(inverted) m <<- inverted
      getInverse <- function() m
      list(set = set, get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


## cacheSolve
## This function computes the inverse of the special "matrix" returned by function makeCacheMatrix . 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
## In this scenario, the user will be informed that the cached version is being returned.
## Otherwise the inverse of the matrix is calculated and the cache updated. 
##

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	  m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setInverse(m)
      m

}
