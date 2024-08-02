## This function creates a special "matrix" object that can cache its inverse.
## The functions within the special "matrix" object can set and get the matrix,
## and set and get the inverse of the matrix.

## This function initializes the special "matrix" object and its methods.

makeCacheMatrix <- function(x = matrix()) {
  iv <- NULL
  set <- function(y) {
    x <<- y
    iv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) iv <<- inverse
  getinverse <- function() iv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  iv <- x$getinverse()
  if (!is.null(iv)) {
    message("getting cached data")
    return(iv)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(iv)
  i
}
