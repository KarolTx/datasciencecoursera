
## cacheMatrix
##
## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than computing it
## repeatedly. Therefor the following functions will compute the inverse
## of a matrix (if necessary) and store it in cache. Otherwise they will
## just retrieve the cached results.

## makeCacheMatrix:
## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  # init
  m <- NULL

  # set/get methods
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x

  # set/get solve methods
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m

  # "object mapping"
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## cacheSolve:
## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cacheSolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x, ...) {
  # looking into cache
  m <- x$getsolve()

  if(!is.null(m)) {
    # found cached result
    message("getting cached data")
    return(m)
  }

  # compute inverse
  data <- x$get()
  m <- solve(data, ...)

  # store in cache
  x$setsolve(m)

  return(m)
}
