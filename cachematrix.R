###
###This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
 invmat <- NULL
  set <- function(y) {
    x <<- y
    invmat <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invmat <<- inverse
  getinverse <- function() invmat
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


### This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
### If the inverse has already been calculated (and the matrix has not changed),
### then the cachesolve  retrieve the inverse from the cache.
### Computing the inverse of a square matrix . The matrix  x is assumed to be a square invertible matrix.

cacheSolve <- function(x, ...) {
 invmat <- x$getinverse()
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  data <- x$get()
  invmat <- solve(data)
  x$setinverse(invmat)
  invmat
}
