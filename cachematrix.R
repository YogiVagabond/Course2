## Caching the Inverse of a Matrix:
## Matrix inversion is a heavy and time-taking computation and it is beneficial 
## to cache the inverse of a matrix rather than calculating it repeatedly.
## Below are two functions which have been employed to create a special object 
## that stores a matrix and caches its inverse.

## Function to create a special "matrix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Function to compute the inverse of the matrix created by the function above. 
## If the inverse is present in the cache (& the matrix is the same as before), 
## then it will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix object that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}